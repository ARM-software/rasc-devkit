/*
 *  Copyright (C) 2007, 2008 ARM
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see http://www.gnu.org/licenses/.
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

/* Define to enable consistency checks during normal operations. 
 * This should only be done when editing the idescs[] in rasc-opc.c. */
/* #define DEBUG_IDESC_PARAMS */

#include "as.h"
#include "bfd.h"
#include "subsegs.h"
#include "safe-ctype.h"
#include "elf/rasc.h"
#include "dwarf2dbg.h"

#include "../opcodes/rasc-opc.h"
#include "tc-rasc.h"

static int max_bcc3_skip_cnt = 8;

static const char *mne_strings[] =
  {
#define MNE_(X) #X
    MNE_LIST
#undef MNE_
  };

/* Trick to override .text into .rodata */
static void rasc_s_section(int);

/* Define characters with special meanings to GAS.  */
const char comment_chars[] = "/";
const char line_comment_chars[] = "#/"; /* only at start of line is '#' a comment */
const char line_separator_chars[] = ";";
const char EXP_CHARS[] = "eE";
const char FLT_CHARS[] = "rRsSfFdDxXpP";

/* The target specific pseudo-ops which we support.  */
const pseudo_typeS md_pseudo_table[] =
{
  { "export",   s_globl,          0 },
  { "import",   s_ignore,         0 },
  { "page",     listing_eject,    0 },

  /* Trick to override .rodata sections to .text */
  { "section",   rasc_s_section, 0 },
  { "section.s", rasc_s_section, 0 },
  { "sect",      rasc_s_section, 0 },
  { "sect.s",    rasc_s_section, 0 },

  { 0,          0,                0 }
};

typedef unsigned long long rasc_u64;

/* RASC instruction record returned from assemble_ex */
struct rasc_inst
{
  rasc_u64 instr64;                /* The actual instruction, LSB adjusted */
  const char *mne;                 /* The mnemonic (name of the instruction) */
  const struct idesc_params *params; /* Information about the binary fields etc */

  /* specifies relocation work to be done */
  /* .X_op == O_absent indicates no work to be done */
  expressionS reloc_exprs[RASC_IMMS]; 
};

void rasc_frob_label (symbolS *sym)
{
  dwarf2_emit_label (sym);
}

/****************************************************************************/

/* Return non-zero if there idesc_params exists for "mne".
   Accepts a mnemonic that has several idescs */
static int 
exists_iparams_for_mne(const char *mne)
{
  int cnt, i, j;
  const struct idesc_params *iptable = isa_get_iptable(&cnt);
  for (i = 0; i < cnt; i++)
  {
    for (j = 0; j < iptable[i].op_cnt; j++)
    {
      if (!strcmp(mne_strings[iptable[i].mnes[j]], mne))
        return 1;
    }
  }
  return 0;
}

/****************************************************************************/
/* Instruction length tracking */

/* Annoying kludge to store data alongside the frag representation. */
/* I had horrifying experiences with TC_FRAG_DATA so I had to
   use a separate table instead /EP */

/* The GAS TC_FRAG_INIT is intensely weird.
   TC_FRAG_INIT is _not_ called when the frag is first created
   rather it seems to be called when the _next_ frag is created.
   Therefore we cannot depend on the data being stored in the frag
   during the 1st assembly pass. */

struct track_data
{
  int tracked_bytes;
  int tracked_instructions;
  char tracked_counts[MAX_TRACKED_BYTES];
  int magic;
  int last_skip_cnt;
};

struct tt_entry
{
  fragS *fragP;
  struct track_data data;
};

static struct tt_entry *g_tt_table = NULL;
static int g_tt_cnt = 0;
static int g_tt_alloced = 0;

/* Find the corresponding track_data field */
static struct track_data *
tt_lookup(fragS *fragP)
{
  int i;
  for (i = 0; i < g_tt_cnt; i++)
    if (g_tt_table[i].fragP == fragP)
      return &g_tt_table[i].data;

  /* Add a new entry */
  if (g_tt_cnt == g_tt_alloced)
  {
    g_tt_alloced = 32 + g_tt_alloced * 2;
    g_tt_table = xrealloc(g_tt_table, g_tt_alloced * sizeof(struct tt_entry));
  }
  g_tt_table[g_tt_cnt].fragP = fragP;
  g_tt_table[g_tt_cnt].data.tracked_bytes = 0;
  g_tt_table[g_tt_cnt].data.tracked_instructions = 0;
  g_tt_table[g_tt_cnt].data.magic = 0x1234babe;
  g_tt_table[g_tt_cnt].data.last_skip_cnt = -1;
  g_tt_cnt++;
  return &g_tt_table[g_tt_cnt - 1].data;
}

/* Make a record of an instruction starting at 'base', having size 'size' */
/* The records are kept so we know how to skip instructions later */
static void track_instruction(fragS *fragP, char *base, int size)
{
  int offset = base-fragP->fr_literal;
  struct track_data *data = tt_lookup(fragP);

  assert(offset >= 0);

  /* Check that the track_data has been properly initialized */
  assert(data->magic == 0x1234babe);

  if (offset + size > MAX_TRACKED_BYTES)
  {
    /* Tracking ceased (frag too long) */
  }
  else if (fragP->fr_type == rs_machine_dependent)
  {
    /* Tracking ignored in variable part */
    /* Avoid tracking variable part, that changes anyway during relaxation.
       Instead the relaxation table should have additional no. of. insns
       information */
  }
  else if (offset != data->tracked_bytes)
  {
    /* Cannot track */
    /* offset - data->tracked_bytes unnacounted for */
  }
  else
  {
    /* The valid branch destinations get an instruction count assigned to
     * them */
    assert(data->tracked_bytes < MAX_TRACKED_BYTES);
    data->tracked_counts[data->tracked_bytes++] = data->tracked_instructions;

    /* The invalid branch destination get -1 assigned to them */
    while (size > 1)
    {
      assert(data->tracked_bytes < MAX_TRACKED_BYTES);
      data->tracked_counts[data->tracked_bytes++] = -1; 
      size--;
    }
    data->tracked_instructions++;
  }
}

/***************************************************************************/
/* reloc_inferral: Autodetection of suitable relocation types */

/* This table specifies how to deduce a BFD_RELOC_xxx enum         */
/* from the RASC assembler idesc_imm_field structure               */
/* NOTE: this must be kept in sync with the howtos in elf32-rasc.c */
static const struct reloc_inferral
{
  int pos;
  int size;
  int pcrel;
  int factor;
  int nag_odd;
  int nag_ovf;
  enum bfd_reloc_code_real reloc;
} reloc_inferrals[]= 
{ /* pos size pcrel factor nag_odd nag_ovf enum bfd_reloc_code_real */
  {   4,  12,   0,   16,    0,       1,    BFD_RELOC_RASC_IMM16_HIGH12 },   /* NOTE: not inferred in md_apply_fix */
  {   5,   3,   0,   1,     1,       0,    BFD_RELOC_RASC_IMM16_LOW3_POS5 },
  {   4,   4,   0,   1,     1,       0,    BFD_RELOC_RASC_IMM16_LOW4_POS4 },
  {   8,   4,   0,   1,     1,       0,    BFD_RELOC_RASC_IMM16_LOW4_POS8 },
  {   6,   6,   0,   1,     1,       0,    BFD_RELOC_RASC_IMM16_LOW6_POS6 },

  {   5,   3,   0,   2,     1,       0,    BFD_RELOC_RASC_IMM16BY2_LOW3_POS5 },
  {   4,   4,   0,   2,     1,       0,    BFD_RELOC_RASC_IMM16BY2_LOW4_POS4 },
  {   4,   4,   0,   4,     1,       0,    BFD_RELOC_RASC_IMM16BY4_LOW4_POS4 },
  {   6,   6,   0,   4,     1,       0,    BFD_RELOC_RASC_IMM16BY4_LOW6_POS6 },

  {   5,  11,   1,   1,     1,       1,    BFD_RELOC_RASC_PC11BY1 },
  {   3,   9,   1,   1,     1,       1,    BFD_RELOC_RASC_PC9BY1 },
  {   5,   7,   1,   1,     1,       1,    BFD_RELOC_RASC_PC7BY1 },
  {   3,   5,   1,   1,     1,       1,    BFD_RELOC_RASC_PC5BY1 },
  {   5,   3,   1,   1,     1,       1,    BFD_RELOC_RASC_PC3BY1 },

  {   5,  11,   1,   2,     1,       1,    BFD_RELOC_RASC_PC11BY2 },
  {   3,   9,   1,   2,     1,       1,    BFD_RELOC_RASC_PC9BY2 },
  {   5,   7,   1,   2,     1,       1,    BFD_RELOC_RASC_PC7BY2 },
  {   3,   5,   1,   2,     1,       1,    BFD_RELOC_RASC_PC5BY2 },
  {   5,   3,   1,   2,     1,       1,    BFD_RELOC_RASC_PC3BY2 },
};

static const struct reloc_inferral *
find_reloc_inferral(const struct idesc_imm_field *field)
{
  unsigned i;
  for (i = 0; i< sizeof(reloc_inferrals)/sizeof(*reloc_inferrals); i++)
    if (field->pos    == reloc_inferrals[i].pos &&
        field->size   == reloc_inferrals[i].size &&
        field->pcrel  == reloc_inferrals[i].pcrel &&
        field->factor == reloc_inferrals[i].factor)
      return &reloc_inferrals[i];
  return NULL;
}

/****************************************************************************
 * Relaxation 
 */

/********* RASC relaxation codes *********/ 
/* For now we don't use the 'C' macro from the other architectures */
#define C_NONE               (0)
#define C_BR                 (1)
#define C_BSR                (2)
#define C_BR8                (3)
#define C_BSR8               (4)
#define C_BR16               (5)
#define C_BSR16              (6)
#define C_BR24               (7)
#define C_BSR24              (8)

#define C_BCC3_REG_REG       (9) /* b<cc> Rd,Rn,range3 */
#define C_BCC3_REG_REG_IMP  (10) /* b<cc> Rd,Rn,range3 */
#define C_BCC3_REG_IMM      (11) /* b<cc> Rd,imm,range3 */
#define C_BCC3_REG_IMMS8    (12) /* b<cc> Rd,imm,range3, suffix8 */
#define C_BCC3_REG_IMMS16   (13) /* b<cc> Rd,imm,range3, suffix16 */
#define C_BCC3_REG_IMMS24   (14) /* b<cc> Rd,imm,range3, suffix24 */
#define C_BCC3_REG_IMMS32   (15) /* b<cc> Rd,imm,range3, suffix32 */

#define C_BCC8_REG_REG      (16) /* b<cc> Rd,Rn,range8 */
#define C_BCC8_REG_REG_IMP  (17) /* b<cc> Rd,Rn,range8 */
#define C_BCC8_REG_IMM      (18) /* b<cc> Rd,imm,range8 */
#define C_BCC8_REG_IMMS8    (19) /* b<cc> Rd,imm8,range8 */
#define C_BCC8_REG_IMMS16   (20) /* b<cc> Rd,imm16,range8 */
#define C_BCC8_REG_IMMS24   (21) /* b<cc> Rd,imm24,range8 */
#define C_BCC8_REG_IMMS32   (22) /* b<cc> Rd,imm32,range8 */

#define C_BCC16_REG_REG     (23) /* b<cc> Rd,Rn,range16 */
#define C_BCC16_REG_REG_IMP (24) /* b<cc> Rd,Rn,range16 */
#define C_BCC16_REG_IMM     (25) /* b<cc> Rd,imm,range16 */
#define C_BCC16_REG_IMMS8   (26) /* b<cc> Rd,imm8,range16 */
#define C_BCC16_REG_IMMS16  (27) /* b<cc> Rd,imm16,range16 */
#define C_BCC16_REG_IMMS24  (28) /* b<cc> Rd,imm24,range16 */
#define C_BCC16_REG_IMMS32  (29) /* b<cc> Rd,imm32,range16  */

#define C_BZ                (30) /* beq/bne Rd,range(5 or so) */
#define C_BZ8               (31) /* beq/bne Rd,range8 */
#define C_BZ16              (32) /* beq/bne Rd,range16 */

#define C_DBCC              (33) /* dbcc Rd,range(5 or so) */
#define C_DBCC8             (34) /* dbcc Rd,range8 */
#define C_DBCC16            (35) /* dbcc Rd,range16 */

#define C_LPC               (36) /* lpc Rd,range2 */
#define C_LPC8              (37) /* lpc Rd,range8 */
#define C_LPC16             (38) /* lpc Rd,range16 */
#define C_LPC24             (39) /* lpc Rd,range24 */

/* NOTE! These must be updated when new codes are introduced above */
#define IS_BCC3(c)  ((c) >= C_BCC3_REG_REG  && (c) <= C_BCC3_REG_IMMS32)
#define IS_BCC8(c)  ((c) >= C_BCC8_REG_REG  && (c) <= C_BCC8_REG_IMMS32)
#define IS_BCC16(c) ((c) >= C_BCC16_REG_REG && (c) <= C_BCC16_REG_IMMS32)

/* A custom relaxation descriptor */
struct rasc_relax_type
{
  int rlx_valid;            /* Is this a valid relaxation state? */
  int field_pos;            /* LSB location of field to modify */
  int field_size;           /* Size of field to modify */
  long rlx_forward;         /* Forward reach. Signed number. > 0.  */
  long rlx_backward;        /* Backward reach. Signed number. < 0.  */
  unsigned char rlx_length; /* Bytes length of this address.  */
  relax_substateT rlx_more; /* Next longer relax-state.
			       0 means there is no 'next' relax-state.  */
};

/******** The relaxation table ********/
/* Part of this table is initialized in relax_keep_field_info */
/* The ordering in this table is dictated by the 'C_xxx' macros above */
static struct rasc_relax_type relax_table[] =
{
  { 0, 0,0,       0,        0, 0, 0 },               /* C_NONE */

  /* BR/BSR and variants */
  { 1,-1,0,      -1,        1, 1, C_BR8   },         /* C_BR */
  { 1,-1,0,      -1,        1, 1, C_BSR8  },         /* C_BSR */
  { 1,-1,0,     128,     -127, 2, C_BR16  },         /* C_BR8 */
  { 1,-1,0,     128,     -127, 2, C_BSR16 },         /* C_BSR8 */
  { 1,-1,0,   32768,   -32767, 3, C_BR24  },         /* C_BR16 */
  { 1,-1,0,   32768,   -32767, 3, C_BSR24 },         /* C_BSR16 */
  { 1,-1,0, 8388608, -8388607, 4, 0 },               /* C_BR24 */
  { 1,-1,0, 8388608, -8388607, 4, 0 },               /* C_BSR24 */

  /* BCC3 */
  { 1,-1,0, 0, 0, 2,     C_BCC8_REG_REG      },  /* C_BCC3_REG_REG */
  { 1,-1,0, 0, 0, 2,     C_BCC8_REG_REG_IMP  },  /* C_BCC3_REG_REG_IMP */
  { 1,-1,0, 0, 0, 2,     C_BCC8_REG_IMM      },  /* C_BCC3_REG_IMM */
  { 1,-1,0, 0, 0, 3,     C_BCC8_REG_IMMS8    },  /* C_BCC3_REG_IMMS8 */
  { 1,-1,0, 0, 0, 4,     C_BCC8_REG_IMMS16   },  /* C_BCC3_REG_IMMS16 */
  { 1,-1,0, 0, 0, 5,     C_BCC8_REG_IMMS24   },  /* C_BCC3_REG_IMMS24 */
  { 1,-1,0, 0, 0, 6,     C_BCC8_REG_IMMS32   },  /* C_BCC3_REG_IMMS32 */

  /* BCC8 */
  { 1,-1,0, 129, -126, 3,   C_BCC16_REG_REG     },  /* C_BCC8_REG_REG     */
  { 1,-1,0, 129, -126, 3,   C_BCC16_REG_REG_IMP },  /* C_BCC8_REG_REG_IMP */
  { 1,-1,0, 129, -126, 3,   C_BCC16_REG_IMM     },  /* C_BCC8_REG_IMM     */
  { 1,-1,0, 130, -125, 3+1, C_BCC16_REG_IMMS8   },  /* C_BCC8_REG_IMMS8   */
  { 1,-1,0, 131, -124, 3+2, C_BCC16_REG_IMMS16  },  /* C_BCC8_REG_IMMS16  */
  { 1,-1,0, 132, -123, 3+3, C_BCC16_REG_IMMS24  },  /* C_BCC8_REG_IMMS24  */
  { 1,-1,0, 133, -122, 3+4, C_BCC16_REG_IMMS32  },  /* C_BCC8_REG_IMMS32  */

  /* BCC16 */
  { 1,-1,0, 0,      0,      4,      0 },  /* C_BCC16_REG_REG */
  { 1,-1,0, 0,      0,      4,      0 },  /* C_BCC16_REG_REG_IMP */
  { 1,-1,0, 0,      0,      4,      0 },  /* C_BCC16_REG_IMM */
  { 1,-1,0, 0,      0,      4+1,    0 },  /* C_BCC16_REG_IMMS8 */
  { 1,-1,0, 0,      0,      4+2,    0 },  /* C_BCC16_REG_IMMS16 */
  { 1,-1,0, 0,      0,      4+3,    0 },  /* C_BCC16_REG_IMMS24 */
  { 1,-1,0, 0,      0,      4+4,    0 },  /* C_BCC16_REG_IMMS32 */

  /* BZ varieties */
  { 1,-1,0,    93,        -32, 2, C_BZ8      },       /* C_BZ */
  { 1,-1,0,   129,       -126, 3, C_BZ16     },       /* C_BZ8 */
  { 1,-1,0, 32769,     -32766, 4, 0          },       /* C_BZ16 */

  /* DBCC varieties */
  { 1,-1,0,      0,       -29, 2, C_DBCC8    },       /* C_DBCC */
  { 1,-1,0,    129,      -126, 3, C_DBCC16   },       /* C_DBCC8 */
  { 1,-1,0,  32769,    -32766, 4, 0          },       /* C_DBCC16 */

  /* LPC varieties */
  { 1,-1,0,      -1,        1, 2, C_LPC8     },       /* C_LPC */
  { 1,-1,0,     129,     -126, 3, C_LPC16    },       /* C_LPC8 */
  { 1,-1,0,   32769,   -32766, 4, C_LPC24    },       /* C_LPC16 */
  { 1,-1,0, 8388609, -8388606, 5, 0          },       /* C_LPC24 */
};

/* Relaxation state modifier. Take a basic relax-type and add an immediate
 * suffix length */
/* Should only be called with ISA-supported suffix lengths */
static int
rasc_get_imm_suffixed_relax_type(int basic_type, int imm_suffix_len)
{
  /* Unfortunatly, immedate suffixes affect the relax type */
  if (basic_type == C_BCC3_REG_IMM)
  {
    int state;
    if (imm_suffix_len == 0)
      state = C_BCC3_REG_IMM;
    else if (imm_suffix_len == 1)
      state = C_BCC3_REG_IMMS8;
    else if (imm_suffix_len == 2)
      state = C_BCC3_REG_IMMS16;
    else if (imm_suffix_len == 3)
      state = C_BCC3_REG_IMMS24;
    else if (imm_suffix_len == 4)
      state = C_BCC3_REG_IMMS32;
    else
      state = C_NONE;
    assert(relax_table[state].rlx_valid);
    return state;
  }
  assert(imm_suffix_len == 0);
  return basic_type;
}

/* Return the relax type if the instruction is relaxable, otherwize 0 */
/* Called from wihtin rasc_assemble */
static int
rasc_get_relax_type(const struct rasc_inst *inst, int imm_suffix_len)
{
  const struct idesc_params *ip = inst->params;
  unsigned i;
  int rtype = C_NONE;
  struct
  {
    const char *mne;
    int relax_type;
  } mappings[] =
  {
    {"br",   C_BR, },
    {"bsr",  C_BSR, },
    {"dbpl", C_DBCC, },
    {"dbne", C_DBCC, },
    {"lpc",  C_LPC, }
  };

  /* Simple cases can be inferred from the mappings */
  for (i = 0; i < sizeof(mappings)/sizeof(*mappings); i++)
    if (!strcmp(inst->mne, mappings[i].mne))
    {
      rtype = mappings[i].relax_type;
      goto check;
    }

  /* Check for reg,imm,3_bit_pcrel */
  if (ip->imm[1].pos >=0 && ip->imm[1].size == 3 && ip->imm[1].pcrel == 1
      && ip->imm[0].pos >= 0)
  {
    rtype = rasc_get_imm_suffixed_relax_type(C_BCC3_REG_IMM, imm_suffix_len);
    goto check;
  }

  /* Check for reg,reg,3_bit_pcrel */
  if (ip->imm[0].pos >= 0 && ip->imm[0].size == 3 && ip->imm[0].pcrel == 1
      && ip->imm[1].pos < 0)
  {
    if (!strcmp(inst->mne, "beq") ||
        !strcmp(inst->mne, "bne"))
      rtype = C_BCC3_REG_REG_IMP; /* instr with implied opcode bit */
    else
      rtype = C_BCC3_REG_REG;
    goto check;
  }

  /* Check for beq/bne reg,>3_bit_pcrel */
  if (ip->imm[0].pos >= 0 && ip->imm[0].size > 3 && ip->imm[0].pcrel == 1
      && ip->imm[1].pos < 0)
  {
    if (!strcmp(inst->mne, "beq") ||
        !strcmp(inst->mne, "bne"))
      rtype = C_BZ; /* instr with implied opcode bit */
    goto check;
  }

  return C_NONE;

check:
  if (!relax_table[rtype].rlx_valid)
    rtype = C_NONE;
  return rtype;
}

static enum bfd_reloc_code_real
relax_type_get_reloc(int relax_type)
{
  /* Mutli-entry relaxation types */
  if (IS_BCC3(relax_type)) return BFD_RELOC_RASC_PC3BY1;
  if (IS_BCC8(relax_type)) return BFD_RELOC_8_PCREL;
  if (IS_BCC16(relax_type)) return BFD_RELOC_16_PCREL;

  /* Single-entry relaxation types */
  switch(relax_type)
  {
    case C_BR8:    return BFD_RELOC_8_PCREL;
    case C_BSR8:   return BFD_RELOC_8_PCREL;
    case C_BR16:   return BFD_RELOC_RASC_BR_BSR_16; /* linker relaxable */
    case C_BSR16:  return BFD_RELOC_RASC_BR_BSR_16; /* linker relaxable */
    case C_BR24:   return BFD_RELOC_RASC_BR_BSR_24; /* linker relaxable */
    case C_BSR24:  return BFD_RELOC_RASC_BR_BSR_24; /* linker relaxable */
    case C_BZ:     return BFD_RELOC_RASC_PC7BY1;
    case C_BZ8:    return BFD_RELOC_8_PCREL;
    case C_BZ16:   return BFD_RELOC_16_PCREL;
    case C_DBCC:   return BFD_RELOC_RASC_PC5BY1;
    case C_DBCC8:  return BFD_RELOC_8_PCREL;
    case C_DBCC16: return BFD_RELOC_16_PCREL;
    case C_LPC8:   return BFD_RELOC_8_PCREL;
    case C_LPC16:  return BFD_RELOC_RASC_LPC_16; /* linker relaxable */
    case C_LPC24:  return BFD_RELOC_RASC_LPC_24; /* linker relaxable */
    default:       return BFD_RELOC_NONE;
  }
}

/* Dynamically initialized data.
   Called during rasc_emit to initialize a relaxation state */
static void
relax_keep_field_info(int relax_type, const struct idesc_imm_field *field)
{
  int j = relax_type;
  int cnt = sizeof(relax_table)/sizeof(*relax_table);

  assert(field->pcrel);
  assert(j >= 0 && j < cnt);

  while (j)
  {
    assert(relax_table[j].rlx_valid);

    if (relax_table[j].field_pos == -1)
    {
      /* init for the first time */
      relax_table[j].field_pos =  field->pos;
      relax_table[j].field_size = field->size;
    }
    else
    {
      /* Already been there. */
      assert(relax_table[j].field_pos == field->pos);
      assert(relax_table[j].field_size == field->size);
      break;
    }

    j= relax_table[j].rlx_more;
    assert(j >= 0 && j < cnt);
  }
}


/* Check how many instruction a BCC3 instruction skips */
/* Subfunction of rasc_relax_frag. */
/* Return -1 if the skip count is unknown */
/* fragP - frag being relaxed */
/* aim - destination relative to current insn */
static int
check_bcc3_skip_cnt(fragS *fragP, offsetT aim)
{
  unsigned int rtype = fragP->fr_subtype;

  assert(rtype < sizeof(relax_table)/sizeof(*relax_table));

  int relax_len = relax_table[rtype].rlx_length;
  int remaining_distance = aim - relax_len;

  if (remaining_distance <= 0)
    return -1; /* skipping less than 1 instruction */

  int count = 0;
  fragS *cur_frag = fragP->fr_next;
  while (remaining_distance && cur_frag)
  {
    struct track_data *data = tt_lookup(cur_frag);
    
    if (remaining_distance < data->tracked_bytes) /* Hit in this track array? */
    {
      int c = data->tracked_counts[remaining_distance];
      if (c == -1)
      {
        as_warn_where(fragP->fr_file, fragP->fr_line,
		      "Branch to unaligned destination");
        return -1;  /* no corresponding instruction count */
      }
      remaining_distance = 0;
      count += c;
    }
    else /* Consume all of the tracked part */
    {
      remaining_distance -= data->tracked_bytes;
      count += data->tracked_instructions;
    }

    if (remaining_distance)
    {
      if (data->tracked_bytes < cur_frag->fr_fix)
        /* Frag has tracked bytes. Could be a ".word" for instance. */
        return -1;  /* no corresponding instruction count */

      /* Take the variable part into account. */
      if (cur_frag->fr_type == rs_machine_dependent)
      {
        unsigned int rtype = cur_frag->fr_subtype;
        assert(rtype < sizeof(relax_table)/sizeof(*relax_table));
        int relax_len = relax_table[rtype].rlx_length;
        
        if (remaining_distance < relax_len)
        {
          as_warn_where(fragP->fr_file, fragP->fr_line,
			"Branch to unaligned destination");
          return -1;  /* no corresponding instruction count */
        }
        remaining_distance -= relax_len;
        count++;
      }
    }

    /* No need to search more than the maximum skip count distance.  */
    if (count > max_bcc3_skip_cnt)
      return -1;

    cur_frag = cur_frag->fr_next;
  }

  if (remaining_distance)
  {
    as_warn_where(fragP->fr_file, fragP->fr_line, "Branch beyond known code");
    return -1;
  }

  return count;
}

/* Returns true if the BCC3 can skip the specified no. of instructions */
static int
skip_cnt_ok_for_bcc3(int skip_cnt)
{
  if (skip_cnt < 1
      || skip_cnt > max_bcc3_skip_cnt
      || isa_get_branch_suffix_bytes(3, skip_cnt - 1))
    return 0;
  return 1;
}

/* Subfunction of relax_frag                                      */
/* See if the frag 'fragP' would reach its 'aim' in state 'state' */
/* aim is relative to the start of the 'var' part                 */
/* (that is, relative to the address of the branch instrunction)  */
/* Side-effect : remember how many instructions a BCC3 would skip */
static int
frag_state_reaches(fragS *fragP, relax_substateT state, offsetT aim)
{
  /* We cannot know how far it is to weak symbols.  Force the longest
     addressing mode. */
  if (S_IS_WEAK (fragP->fr_symbol))
    return 0;

  if (IS_BCC3(state))
    {
      /* Check how many instructions the BCC3 skips */
      int skip_cnt = check_bcc3_skip_cnt(fragP, aim);
      tt_lookup(fragP)->last_skip_cnt = skip_cnt;
      return skip_cnt_ok_for_bcc3(skip_cnt);
    }

  /* Check against the range specified in the relax_table. */
  /* Suffix codes should already have been stripped from that range */
  if (aim >= relax_table[state].rlx_backward &&
      aim <= relax_table[state].rlx_forward)
    return 1; /* yes, we reach */

  return 0; /* no, we need a longer addressing mode */
}

/* Relax a fragment by scanning relax_table */
/* A custom 'md_relax_frag' was needed since the default (relax_frag) does
not support ranges that don't include zero. */
long
rasc_relax_frag(segT segment, fragS *fragP, long stretch)
{
  const struct rasc_relax_type *this_type;
  const struct rasc_relax_type *start_type;
  relax_substateT next_state;
  relax_substateT this_state;
  offsetT growth;
  offsetT aim;
  addressT target;
  addressT address;
  symbolS *symbolP;
  const struct rasc_relax_type *table;

  target = fragP->fr_offset;
  address = fragP->fr_address;
  table = relax_table;
  this_state = fragP->fr_subtype;
  start_type = this_type = table + this_state;
  symbolP = fragP->fr_symbol;

  if (symbolP)
    {
      fragS *sym_frag;

      sym_frag = symbol_get_frag (symbolP);

#ifndef DIFF_EXPR_OK
#if !defined (MANY_SEGMENTS) && !defined (BFD_ASSEMBLER)
      know ((S_GET_SEGMENT (symbolP) == SEG_ABSOLUTE)
            || (S_GET_SEGMENT (symbolP) == SEG_DATA)
            || (S_GET_SEGMENT (symbolP) == SEG_BSS)
            || (S_GET_SEGMENT (symbolP) == SEG_TEXT));
#endif
      know (sym_frag != NULL);
#endif
      know (S_GET_SEGMENT (symbolP) != absolute_section
            || sym_frag == &zero_address_frag);
      target += S_GET_VALUE (symbolP);

      /* If frag has yet to be reached on this pass,
         assume it will move by STRETCH just as we did.
         If this is not so, it will be because some frag
         between grows, and that will force another pass.  */

      if (stretch != 0
          && sym_frag->relax_marker != fragP->relax_marker
          && S_GET_SEGMENT (symbolP) == segment)
        {
          target += stretch;
        }
    }

  aim = target - address - fragP->fr_fix;

  /* This loops differs from the origininal in that it supports of ranges that
     don't include 0, and even auto-calculated ranges such as instruction-
     counting semantics */
  for (next_state = this_type->rlx_more; next_state;)
    if (frag_state_reaches(fragP,this_state, aim))
      next_state = 0; /* No need to grow */
    else
    {
      /* Grow to next state.  */
      this_state = next_state;
      this_type = table + this_state;
      next_state = this_type->rlx_more;
    }

  growth = this_type->rlx_length - start_type->rlx_length;
  if (growth != 0)
    fragP->fr_subtype = this_state;
  return growth;
}

/* Check the minimum and maximum instruction length after relaxation */
static void
get_min_max_relax_len(unsigned int rtype, int *minlen, int *maxlen)
{
  assert(rtype < sizeof(relax_table)/sizeof(*relax_table));
  *minlen = relax_table[rtype].rlx_length;
  *maxlen = *minlen;
  while (relax_table[rtype].rlx_more)
  {
    rtype = relax_table[rtype].rlx_more;
    assert(rtype < sizeof(relax_table)/sizeof(*relax_table));
    assert(relax_table[rtype].rlx_length > *maxlen); /* increasing order expected */
    *maxlen = relax_table[rtype].rlx_length;
  }
}

/* Combine two bytes into an instruction word */
static unsigned short
combine_bytes(char byte0, char byte1)
{
  return (( ((unsigned short) (unsigned char) byte0) ) |
	  ( ((unsigned short) (unsigned char) byte1) << 8 ));
}

/* Called just before address relaxation, return the length
   by which a fragment must grow to reach it's destination.  */
int
md_estimate_size_before_relax (fragS *fragP, segT segment_type)
{
  /* See if it is a branch to an unknown destination */
  /* In that case relax it to the longest mode */
  if (fragP->fr_symbol && S_GET_SEGMENT (fragP->fr_symbol) != segment_type)
  {
    /* relax away... */

    int rtype = fragP->fr_subtype;
    while (relax_table[rtype].rlx_more)
      rtype = relax_table[rtype].rlx_more;
    fragP->fr_subtype = rtype;
    /* The frag will pass through the relaxation with an incorrect aim,
       but that won't affect anything. */
  }

  return relax_table[fragP->fr_subtype].rlx_length;
}

/* Called after relaxing, change the frags so they know how big they are.  */
void
md_convert_frag (bfd *abfd ATTRIBUTE_UNUSED,
		 segT sec ATTRIBUTE_UNUSED,
		 fragS *fragP)
{
  enum bfd_reloc_code_real rasc_reloc = relax_type_get_reloc(fragP->fr_subtype);
  unsigned char *buffer = (unsigned char *)(fragP->fr_fix + fragP->fr_literal);
  int relax_type = fragP->fr_subtype;
  int bsuffix_len = 0;

  if (IS_BCC8(relax_type) ||
      relax_type==C_BR8 ||
      relax_type==C_BSR8 ||
      relax_type==C_DBCC8 ||
      relax_type==C_LPC8 ||
      relax_type==C_BZ8)
    bsuffix_len= 1;
  else if (IS_BCC16(relax_type) ||
           relax_type==C_BR16 ||
           relax_type==C_BSR16 ||
           relax_type==C_DBCC16 ||
           relax_type==C_LPC16 ||
           relax_type==C_BZ16)
    bsuffix_len= 2;
  else if (relax_type==C_BR24 || relax_type==C_BSR24 || relax_type==C_LPC24)
    bsuffix_len= 3;

  /* Fix suffixed branches using common code. */
  if (bsuffix_len)
  {
    /* Direct fixup of the suffix code */
    int relax_len = relax_table[relax_type].rlx_length;
    int field_pos = relax_table[relax_type].field_pos;
    int field_size = relax_table[relax_type].field_size;

    /* Any instruction should be at least 2 bytes long at this stage. */
    assert(relax_len >= 2);
    unsigned short bcc = combine_bytes(buffer[0], buffer[1]);

    int bsuffix_code = isa_get_branch_suffix_code(field_size, bsuffix_len);
    assert(bsuffix_code != -1);
    assert(field_pos != -1);
    assert(field_size != 0);

    bcc &= ~(((1<<field_size)-1) << field_pos); /* clear the range field */
    bcc |= (bsuffix_code << field_pos);         /* set it to the suffix_code */
    md_number_to_chars((char *) buffer, (valueT) bcc, 2);

    /* Standard PC relative fixup for the branch suffix */
    fixS *fixP =
      fix_new (fragP, fragP->fr_fix + relax_len - bsuffix_len, bsuffix_len,
	       fragP->fr_symbol, fragP->fr_offset, 1, /* pcrel */
	       rasc_reloc);

    fixP->fx_line = fragP->fr_line;          /* keep for proper warnings */
    goto done;
  }

  /* Instruction-skipping semantics for BCC3 */
  if (IS_BCC3(relax_type))
  {
    /* No need to change the instruction, just fix the RRR field */
    /* Get the skip count determined during rasc_relax_frag */
    int skip_cnt = tt_lookup(fragP)->last_skip_cnt;
    assert(skip_cnt_ok_for_bcc3(skip_cnt));

    /* Direct fixup of branch range */
    unsigned short bcc = combine_bytes(buffer[0], buffer[1]);
    bcc &= ~(7 << 5); /* clear the range field */
    bcc |= ((skip_cnt - 1) << 5); /* set it to skip_cnt-1 */
    md_number_to_chars((char *)buffer, (valueT)bcc, 2);
    goto done;
  }

  /* Just a plain PC-relative fixup, no suffix */
  if (relax_type == C_BR ||
      relax_type == C_BSR ||
      relax_type == C_DBCC ||
      relax_type == C_LPC ||
      relax_type == C_BZ ||
      IS_BCC3(relax_type))
  {
    /* No need to change the instruction, just set up the fixup */
    int bias = relax_table[relax_type].rlx_backward;
    assert(rasc_reloc != BFD_RELOC_NONE);

    fixS *fixP = fix_new (fragP, fragP->fr_fix, 2,
			  fragP->fr_symbol, fragP->fr_offset, 1, /* pcrel */
			  rasc_reloc);
    fixP->fx_addnumber = -bias;  /* save range field bias for the reloc */
    fixP->fx_line = fragP->fr_line;   /* keep for proper warnings */

    goto done;
  }

  /* Unimplemented relaxation */
  abort();

done:
  fragP->fr_fix += relax_table[relax_type].rlx_length;
}

/****************************************************************************
 *
 * RASC assembler engine
 *
 */

/*********************************************************
 * Lexical analysis
 */

static bool
streq_nocase(const char *a, const char *b)
{
  while (*a && *b && TOLOWER(*a) == TOLOWER(*b))
  {
    a++;
    b++;
  }
  return !*a && !*b;
}

static int
is_alpha(int a)
{
  return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z');
}

static int
is_alpha_numeric(int a)
{
  return ((a >= 'a' && a <= 'z')
	  || (a >= 'A' && a <= 'Z')
	  || (a >= '0' && a <= '9'));
}

static void
skip_hspace(const char **pp)
{
  while (**pp == ' ' || **pp == '\t')
    (*pp)++;
}

/* Only 0-15 are general-purpose hardware registers. */
/* lr,sp are aliases for r14,r15 */
#define REG_R0 (0)
#define REG_R1 (1)
#define REG_R2 (2)
#define REG_R3 (3)
#define REG_R4 (4)
#define REG_R5 (5)
#define REG_R6 (6)
#define REG_R7 (7)
#define REG_R8 (8)
#define REG_R9 (9)
#define REG_R10 (10)
#define REG_R11 (11)
#define REG_R12 (12)
#define REG_R13 (13)
#define REG_R14 (14)
#define REG_R15 (15)
#define REG_LR (REG_R14)
#define REG_SP (REG_R15)
#define REG_PC (16)
#define REG_SR (17)
#define REG_C0 (18)
#define REG_C1 (19)
#define REG_C2 (20)
#define REG_C3 (21)
#define REG_C4 (22)
#define REG_C5 (23)
#define REG_C6 (24)
#define REG_C7 (25)
#define REG_C8 (26)
#define REG_C9 (27)
#define REG_C10 (28)
#define REG_C11 (29)
#define REG_C12 (30)
#define REG_C13 (31)
#define REG_C14 (32)
#define REG_C15 (33)
#define REG_C16 (34)
#define REG_C17 (35)
#define REG_C18 (36)
#define REG_C19 (37)
#define REG_C20 (38)
#define REG_C21 (39)
#define REG_C22 (40)
#define REG_C23 (41)
#define REG_C24 (42)
#define REG_C25 (43)
#define REG_CMAX REG_C25
#define REG_V0  (44)
#define REG_V1  (45)
#define REG_V2  (46)
#define REG_V3  (47)
#define REG_V4  (48)
#define REG_V5  (49)
#define REG_V6  (50)
#define REG_V7  (51)
#define REG_V8  (52)
#define REG_V9  (53)
#define REG_V10 (54)
#define REG_V11 (55)
#define REG_V12 (56)
#define REG_V13 (57)
#define REG_V14 (58)
#define REG_V15 (59)

const struct { const char *str; int reg; } special_regs[] =
{
  { "r0", REG_R0 },
  { "r1", REG_R1 },
  { "r2", REG_R2 },
  { "r3", REG_R3 },
  { "r4", REG_R4 },
  { "r5", REG_R5 },
  { "r6", REG_R6 },
  { "r7", REG_R7 },
  { "r8", REG_R8 },
  { "r9", REG_R9 },
  { "r10", REG_R10 },
  { "r11", REG_R11 },
  { "r12", REG_R12 },
  { "r13", REG_R13 },
  { "r14", REG_LR },
  { "r15", REG_SP },

  { "lr", REG_LR },
  { "sp", REG_SP },
  { "pc", REG_PC },
  { "sr", REG_SR },

  { "c0", REG_C0 },
  { "c1", REG_C1 },
  { "c2", REG_C2 },
  { "c3", REG_C3 },
  { "c4", REG_C4 },
  { "c5", REG_C5 },
  { "c6", REG_C6 },
  { "c7", REG_C7 },
  { "c8", REG_C8 },
  { "c9", REG_C9 },
  { "c10", REG_C10 },
  { "c11", REG_C11 },
  { "c12", REG_C12 },
  { "c13", REG_C13 },
  { "c14", REG_C14 },
  { "c15", REG_C15 },
  { "c16", REG_C16 },
  { "c17", REG_C17 },
  { "c18", REG_C18 },
  { "c19", REG_C19 },
  { "c20", REG_C20 },
  { "c21", REG_C21 },
  { "c22", REG_C22 },
  { "c23", REG_C23 },
  { "c24", REG_C24 },
  { "c25", REG_C25 },

  /* Control register aliases */
  { "vbr",  REG_C0 },
  { "epc",  REG_C1 },
  { "far",  REG_C2 },

  { "v0", REG_V0 },
  { "v1", REG_V1 },
  { "v2", REG_V2 },
  { "v3", REG_V3 },
  { "v4", REG_V4 },
  { "v5", REG_V5 },
  { "v6", REG_V6 },
  { "v7", REG_V7 },
  { "v8", REG_V8 },
  { "v9", REG_V9 },
  { "v10", REG_V10 },
  { "v11", REG_V11 },
  { "v12", REG_V12 },
  { "v13", REG_V13 },
  { "v14", REG_V14 },
  { "v15", REG_V15 }
};

/* Return register in *r or return 0 on failure */
static int
scan_reg(const char **pp, int *r)
{
  unsigned int cnt = 0;
  int reg = -1;
  unsigned int i;
  char buf[16];

  skip_hspace(pp);

  /* First char must be a letter */
  if (!is_alpha((*pp)[0]))
    return 0;

  /* Check length and copy into tolower'ed buffer */
  while (is_alpha_numeric((*pp)[cnt]) && cnt < sizeof(buf))
  {
    buf[cnt] = TOLOWER((*pp)[cnt]);
    cnt++;
  }

  /* Special registers */
  for (i = 0; reg == -1 && i<(sizeof(special_regs)/sizeof(*special_regs)); i++)
    if (strlen(special_regs[i].str) == cnt &&
        strncmp(special_regs[i].str, buf, cnt) == 0)
      reg= special_regs[i].reg;

  if (reg != -1)
  {
    *r = reg;
    *pp += cnt;
    return 1;
  }

  return 0; /* not a register */
}

static int
scan_delim(const char **pp, char delim)
{
  skip_hspace(pp);
  if (**pp == delim)
  {
    (*pp)++;
    return 1;
  }
  return 0;
}

static int
scan_end(const char **pp)
{
  skip_hspace(pp);
  return **pp == 0 || **pp == '@';
}

/****************************************************************
 * Integer operand parsing
 */

/* Call GAS's expression parser. Temporarily switch input_line_pointer. */
static char *
parse_exp(char *s, expressionS *e)
{
  char *save;
  char *new_input;

  /* Skip whitespace.  */
  while (ISSPACE (* s))
    ++ s;

  save = input_line_pointer;
  input_line_pointer = s;

  expression (e);

  if (e->X_op == O_absent)
    as_bad (_("missing operand"));

  new_input = input_line_pointer;
  input_line_pointer = save;

  return new_input;
}

static void
int_operand_set_const(expressionS *expr, int val)
{
  char tmp[16];
  sprintf(tmp,"%d",val);
  (void) parse_exp(tmp, expr);
}

/*
 * Full version of integer operand parser. Uses the GAS expression parser
 * Fills in the struct int_operand
 * Returns true and advances *pp if the operand was parsed successfully
 */
static bool
parse_int_operand(const char **pp, expressionS *expr)
{
  const char *str = *pp;
  int dummy;

  skip_hspace(&str);

  /* Avoid collision with register syntax */
  if (scan_reg(&str, &dummy))
  {
    expr->X_op = O_absent;
    return false;
  }

  /* Avoid collision between RASC bracket addressing mode
     and the literal syntax of the general parser. */
  if (*str == '[')
  {
    expr->X_op = O_absent;
    return false; /* Not an integer */
  }

  str= parse_exp((char *)str, expr); /* TODO: avoid error messages in this one */

  if (expr->X_op == O_absent)
    return false; /* parse failed */

  *pp = str;
  return true;
}


/* Debug function */
static void
str_from_int_operand(char *buf, const expressionS *expr)
{
  if (expr->X_op == O_absent)
    strcpy(buf, "<unset>");
  else if (expr->X_op == O_constant)
    sprintf(buf, "0x%04x", (unsigned int)expr->X_add_number);
  else
    strcpy(buf, "<expr>");
}

/****************************************************************
 * RHS (right hand side) parsing
 */

static int
parse_rhs(const char *mne,
	  const char *rhs,
	  int *rhs_rd,
	  int *rhs_rn,
	  int *rhs_rm,
	  int *rhs_ro,
	  expressionS *rhs_imm0,
	  expressionS *rhs_imm1,
	  int *rhs_fmt)
{
  const int debug = 0; /* set this to enable debug printouts */

  const char *pos;
  int rd, rn, rm, ro;
  expressionS imm0, imm1;

  *rhs_rd = -1;
  *rhs_rn = -1;
  *rhs_rm = -1;
  *rhs_ro = -1;
  rhs_imm0->X_op = O_absent;
  rhs_imm1->X_op = O_absent;
  *rhs_fmt = FMT_PLAIN;

  pos = rhs;
  if (scan_end(&pos))
  {
    if (debug)
      fprintf(stderr, "No-operand format\n");
    return 1;
  }

  pos = rhs;
  if (parse_int_operand(&pos, &imm0) && scan_end(&pos))
  {
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr, "Immediate format: %s\n", imm0_str);
    }
    *rhs_imm0 = imm0;
    return 1;
  }

  /* Rd          */
  /* Rd,Rn       */
  /* Rd,Rn,Rm    */
  /* Rd,Rn,Rm,Ro */
  pos = rhs;
  if (scan_reg(&pos, &rd))
  {
    if (scan_end(&pos))
    {
      if (debug)
        fprintf(stderr, "One-register format: r%d\n", rd);
      *rhs_rd = rd;
      return 1;
    }
    else if (scan_delim(&pos, ',') && scan_reg(&pos, &rn))
    {
      if (scan_end(&pos))
      {
        if (debug)
          fprintf(stderr, "Two-register format: r%d,r%d\n", rd, rn);
        *rhs_rd = rd;
        *rhs_rn = rn;
        return 1;
      }
      else if (scan_delim(&pos, ',') && scan_reg(&pos, &rm))
      {
        if (scan_end(&pos))
        {
          if (debug)
            fprintf(stderr, "Three-register format: r%d,r%d,r%d\n", rd, rn, rm);
          *rhs_rd = rd;
          *rhs_rn = rn;
          *rhs_rm = rm;
          return 1;
        }
        else if (scan_delim(&pos, ',') && scan_reg(&pos, &ro))
        {
          if (scan_end(&pos))
          {
            if (debug)
              fprintf(stderr, "Four-register format: r%d,r%d,r%d,r%d\n",
		      rd, rn, rm, ro);
            *rhs_rd = rd;
            *rhs_rn = rn;
            *rhs_rm = rm;
            *rhs_ro = ro;
            return 1;
          }
        }
      }
    }
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm0) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_imm0 = imm0;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr,"Register-immediate format: r%d, %s\n", rd, imm0_str);
    }
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm1) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_imm0 = imm0;
    *rhs_imm1 = imm1;
    if (debug)
    {
      char imm0_str[16];
      char imm1_str[16];
      str_from_int_operand(imm0_str, &imm0);
      str_from_int_operand(imm1_str, &imm1);
      fprintf(stderr,"Register-immediate-range format: r%d, %s, %s\n",
	      rd, imm0_str, imm1_str);
    }
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm0) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr,"Register-register-range format: r%d, r%d, %s\n",
	      rd, rn, imm0_str);
    }
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_BRACKET;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr,"Register-offset-addressing format: r%d, [r%d, %s]\n",
	      rd, rn, imm0_str);
    }
    return 1;
  }

  /* Rd,[Rn] is an alias for Rd,[Rn,0] */
  /* Vd,[Rn] is not an alias. No immediate added in this case */
  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    if (mne[0] != 'v')
      int_operand_set_const(rhs_imm0, 0); /* Implicit zero-valued immediate */
    *rhs_fmt = FMT_BRACKET;
    if (debug)
      fprintf(stderr, "Register-only-addressing format: r%d, [r%d]\n", rd, rn);
    return 1;
  }

  /* imm,[Rn] */
  pos = rhs;
  if (parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_BRACKET;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);

      fprintf(stderr, "Register-only-addressing format: #%s, [r%d]\n",
	      imm0_str, rn);
    }
    return 1;
  }

  /* imm,[Rn] */
  pos = rhs;
  if (parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, '+') &&
      scan_delim(&pos, '+') &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_BRACKET_PLUS_PLUS;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);

      fprintf(stderr, "Register-only-addressing format: #%s, [r%d++]\n",
	      imm0_str, rn);
    }
    return 1;
  }

  /* Vd,[Rn++] */
  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, '+') &&
      scan_delim(&pos, '+') &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_fmt = FMT_BRACKET_PLUS_PLUS;
    if (debug)
      fprintf(stderr, "Post-increment format: r%d, [r%d++]\n", rd, rn);
    return 1;
  }

  /* Rd,Vn[imm] */
  pos= rhs;
  if (scan_reg(&pos,&rd) &&
      scan_delim(&pos,',') &&
      scan_reg(&pos,&rn) &&
      scan_delim(&pos,'[') &&
      parse_int_operand(&pos,&imm0) &&
      scan_delim(&pos,']') &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_INDEX;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr, "Index format: r%d, r%d[%s]\n", rd, rn, imm0_str);
    }
    return 1;
  }

  /* Vd[imm],Vn */
  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, '[') &&
      parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ']') &&
      scan_delim(&pos, ',') &&
      scan_reg(&pos, &rn) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_INDEX0;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr, "Index0 format: r%d[%s], r%d\n", rd, imm0_str, rn);
    }
    return 1;
  }

  /* Vd[imm],imm */
  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, '[') &&
      parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ']') &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm1) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_imm0 = imm0;
    *rhs_imm1 = imm1;
    *rhs_fmt = FMT_INDEX0;
    if (debug)
    {
      char imm0_str[16];
      char imm1_str[16];
      str_from_int_operand(imm0_str, &imm0);
      str_from_int_operand(imm1_str, &imm1);
      fprintf(stderr, "Index0 format: r%d[%s], %s\n", rd, imm1_str, imm0_str);
    }
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_delim(&pos, '[') &&
      parse_int_operand(&pos, &imm0) &&
      scan_delim(&pos, ']') &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_imm0 = imm0;
    *rhs_fmt = FMT_BRACKET;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr, "Absolute addressing format: r%d, [%s]\n", rd, imm0_str);
    }
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, '-') &&
      scan_reg(&pos, &rn) &&
      scan_end(&pos))
  {
    if (rn != 14)
      as_fatal("Register list must be terminated by r14");

    *rhs_rd = rd;
    *rhs_fmt = FMT_LIST;
    if (debug)
      fprintf(stderr, "Register list format: r%d-r%d\n", rd, rn);
    return 1;
  }

  pos = rhs;
  if (scan_reg(&pos, &rd) &&
      scan_delim(&pos, ',') &&
      scan_reg(&pos, &rn) &&
      scan_delim(&pos, ',') &&
      scan_reg(&pos, &rm) &&
      scan_delim(&pos, ',') &&
      parse_int_operand(&pos, &imm0) &&
      scan_end(&pos))
  {
    *rhs_rd = rd;
    *rhs_rn = rn;
    *rhs_rm = rm;
    *rhs_imm0 = imm0;
    if (debug)
    {
      char imm0_str[16];
      str_from_int_operand(imm0_str, &imm0);
      fprintf(stderr, "Reg-reg-reg-immediate format: r%d,r%d,r%d,%s\n",
	      rd, rn, rm, imm0_str);
    }
    return 1;
  }

  pos = rhs;
  if (parse_int_operand(&pos,&imm0) &&
      scan_delim(&pos,',') &&
      parse_int_operand(&pos,&imm1) &&
      scan_end(&pos))
  {
    *rhs_imm0 = imm0;
    *rhs_imm1 = imm1;
    if (debug)
    {
      char imm0_str[16];
      char imm1_str[16];
      str_from_int_operand(imm0_str, &imm0);
      str_from_int_operand(imm1_str, &imm1);
      fprintf(stderr, "Immediate-range format: %s, %s\n", imm0_str, imm1_str);
    }
    return 1;
  }

  if (debug)
    fprintf(stderr, "Unknown addressing mode\n");  
  return 0;
}

/* Parse mnemonic. Return no. of chars parsed */
static int
parse_mne(char line_mne[16], const char *line)
{
  int mnelen = 0;
  while (line[mnelen] && !ISSPACE(line[mnelen]) && mnelen < 15)
  {
    line_mne[mnelen] = line[mnelen];
    mnelen++;
  }
  line_mne[mnelen] = 0;
  return mnelen;
}

/* Translate nop pseudo-opcode into a real instruction */
static void
translate_nop(const char **line)
{
  char line_mne[16];
  if (parse_mne(line_mne, *line) == 3 &&
      !strcmp(line_mne, "nop"))
    *line = NOP_INSTR;
}

/* Transform conditional branches to their canonical form */
/* OH: 2005-06-10: Tillfälligt hack. Kanonisera villkorliga branchar. */
static int
make_canonical_branch(char *line_mne, int *rhs_rd, int *rhs_rn,
		      expressionS *rhs_imm)
{
  /* FORM: b<cond> rd,rn,range --> swap arguments */
  if (*rhs_rd >= 0 && *rhs_rn >= 0)
  {
    #define TRY_TRANSFORM(from,to)                               \
      if (streq_nocase(line_mne, from)) {                        \
        strcpy(line_mne, to);                                    \
        {int temp = *rhs_rd; *rhs_rd = *rhs_rn; *rhs_rn = temp;} \
        return 1; }
    TRY_TRANSFORM("bgt","blt");
    TRY_TRANSFORM("bge","ble");
    TRY_TRANSFORM("bhi","blo");
    TRY_TRANSFORM("bhs","bls");
    #undef TRY_TRANSFORM
  }
  /* FORM: b<cond> rd,imm,range --> imm = imm +/- 1 */
  /* TODO: overflow handling when adding 1 /EP
     bls rd,0xffff != blo rd,0x0000
  */
  if (*rhs_rd >= 0 && *rhs_rn == -1 && rhs_imm->X_op != O_absent)
  {
    #define TRY_TRANSFORM(from,to,offset)                        \
      if (streq_nocase(line_mne, from)) {                        \
        strcpy(line_mne, to);                                    \
        rhs_imm->X_add_number += offset;                         \
        return 1; }
    TRY_TRANSFORM("ble","blt",+1);
    TRY_TRANSFORM("bgt","bge",+1);
    TRY_TRANSFORM("bls","blo",+1);
    TRY_TRANSFORM("bhi","bhs",+1);
    #undef TRY_TRANSFORM
  }
  return 0;
}

/* Remove pathological branches, to avoid problem with "implied opcode"
 * when rd=rn */
/* OH: 2005-07-17: Tillfälligt hack. Undvik problem med "implied opcode"
 * när rd=rn. */
/* När så är fallet gäller det inte lägre att villkor inverteras vid byte
 * av argument */
static int
avoid_pathological_branch(char *line_mne, int *rhs_rd, int *rhs_rn,
			  expressionS *rhs_imm)
{
  if (*rhs_rd >= 0 && *rhs_rn == *rhs_rd)
  {
    if (streq_nocase(line_mne, "beq") ||
	streq_nocase(line_mne, "ble") ||
	streq_nocase(line_mne, "bls"))
    {
      /* These branches will always be taken. Replace with unconditional
       * branch. */
      strcpy(line_mne, "br");
      *rhs_rd = -1;
      *rhs_rn = -1;
      return 1;
    }
    if (streq_nocase(line_mne, "bne") ||
	streq_nocase(line_mne, "blt") ||
	streq_nocase(line_mne, "blo"))
    {
      /* These branches will never be taken. Replace with nop.
       * (no nop available --> mov) */
      strcpy(line_mne, "mov");
      rhs_imm->X_op = O_absent;
      return 1;
    }
  }
  return 0;
}

/* Simplify "beq/bne Rd,0,label" into "beq/bne Rd,label" */
static void
simplify_branch(char *line_mne, expressionS *rhs_imm0, expressionS *rhs_imm1)
{
  if ((streq_nocase(line_mne, "beq") ||
       streq_nocase(line_mne, "bne")) &&
      rhs_imm0->X_op == O_constant &&
      rhs_imm1->X_op != O_absent &&
      rhs_imm0->X_add_number == 0)
  {
    *rhs_imm0 = *rhs_imm1;
    rhs_imm1->X_op = O_absent;
  }
}

/* Early fixup of constants */
static void
rasc_insert_constants(struct rasc_inst *inst, expressionS *rhs_imm)
{
  /* Insert immediates and reloc_exprs */
  int k;
  const struct idesc_params *ip = inst->params;
  for (k = 0; k < RASC_IMMS; k++)
  {
    if (ip->imm[k].pos >= 0)
    {
      int done = 0;
      if (rhs_imm[k].X_op == O_constant) /* plonk in constant right now */
      {
        int val = rhs_imm[k].X_add_number;

        if (!ip->imm[k].pcrel && isa_imm_fits_value(&ip->imm[k], val))
        {
          /* Fits in the immediate field. Insert it now and be done with it. */
          val -= ip->imm[k].bias;    /* implicit term added to bit field */
          val /= ip->imm[k].factor;  /* implicit factor applied to bit field */
          inst->instr64 |= ((rasc_u64) val) << ip->imm[k].pos;
          done = 1;
        }
      }

      if (!done)
      {
        /* Keep the GAS expression for fixup later */
        inst->reloc_exprs[k] = rhs_imm[k];
      }
    }
  }
}

/* Transform mov's using special registers for more intuitive syntax */
/* Warn for the old deprecated opcodes (only to be used internally from now) */
/* mov reg,sr => mfsr reg */
/* mov sr,reg => mtsr reg */
/* mov reg,Cn => mfcr reg,imm */
/* mov Cd,reg => mtcr reg,imm */
static void
translate_mov_special(char *line_mne, int *rhs_rd, int *rhs_rn,
		      expressionS *rhs_imm)
{
  if (streq_nocase(line_mne, "mtcr") ||
      streq_nocase(line_mne, "mfcr") ||
      streq_nocase(line_mne, "mtsr") ||
      streq_nocase(line_mne, "mfsr"))
  {
    /* TODO: Decide if they are deprecated or not... */
#if 0
    as_warn("Deprecated opcode '%s'",line_mne);
#endif
  }
  else if (streq_nocase(line_mne, "mov"))
  {
    if (*rhs_rd >= 0 && *rhs_rn == REG_SR && rhs_imm->X_op == O_absent)
    {
      strcpy(line_mne, "mfsr");
      *rhs_rn = -1;
    }
    else if (*rhs_rd == REG_SR && *rhs_rn >= 0 && rhs_imm->X_op == O_absent)
    {
      strcpy(line_mne, "mtsr");
      *rhs_rd = *rhs_rn;
      *rhs_rn = -1;
    }
    else if (*rhs_rd >=0 && *rhs_rn >= REG_C0 && *rhs_rn <= REG_CMAX
	     && rhs_imm->X_op == O_absent)
    {
      strcpy(line_mne,"mfcr");
      int_operand_set_const(rhs_imm, (*rhs_rn) - REG_C0);
      *rhs_rn = -1;
    }
    else if (*rhs_rd >= REG_C0 && *rhs_rd <= REG_CMAX && *rhs_rn >= 0
	     && rhs_imm->X_op == O_absent)
    {
      strcpy(line_mne, "mtcr");
      int_operand_set_const(rhs_imm, (*rhs_rd) - REG_C0);
      *rhs_rd = *rhs_rn;
      *rhs_rn = -1;
    }
  }
}

/* Transform rts into "jmp lr" if there no rts instruction exists in the ISA */
static void
translate_rts(char *line_mne, int *rhs_rd, int *rhs_rn, expressionS rhs_imm[2])
{
  if (streq_nocase(line_mne, "rts"))
  {
    if (*rhs_rd == -1 && *rhs_rn == -1 &&
	rhs_imm[0].X_op == O_absent &&
	rhs_imm[1].X_op == O_absent)
    {
      strcpy(line_mne, "jmp");
      *rhs_rd = REG_LR;
    }
  }
}

/* Translate "vsbm imm0,imm1" into "vsbm Vd,Vn,imm0" */
/* This is done because we don't have any clean support for the
 * split 8-bit immediate */
static void
translate_vsbm(char *line_mne, int *rhs_rd, int *rhs_rn,
	       expressionS *rhs_imm0, expressionS *rhs_imm1)
{
  if (streq_nocase(line_mne, "vsbm"))
  {
    if (*rhs_rd != -1 || *rhs_rn != -1 ||
        rhs_imm0->X_op == O_absent ||
        rhs_imm1->X_op == O_absent)
    {
      as_fatal("Bad addressing mode for vsbm");
      return;
    }

    if (rhs_imm0->X_op != O_constant)
    {
      as_fatal("Constant required for lane mask");
      return;
    }

    int mask = rhs_imm0->X_add_number;
    if (mask < 0 || mask > 255)
    {
      as_fatal("Lane mask out of range");
      return;
    }

    *rhs_rd = REG_V0 + (mask >> 4);
    *rhs_rn = REG_V0 + (mask & 15);
    *rhs_imm0 = *rhs_imm1;
    rhs_imm1->X_op = O_absent;
  }
}

/* Optimize "mov" into "movn" where that is benificial for code size */
/* Optimize "mov" into "lpc" where that is benificial for code size */
static void
optimize_mov(char *line_mne, expressionS *rhs_imm)
{
  if (streq_nocase(line_mne, "mov"))
  {
    if (rhs_imm->X_op == O_constant &&
        ((rhs_imm->X_add_number >> 24) & 0xff) == 0xff)
    {
      strcpy(line_mne, "movn");
      rhs_imm->X_add_number = ~(rhs_imm->X_add_number);
    }
    /* Automatic use of PC-relative instruction */
    else if (rhs_imm->X_op != O_absent &&
             rhs_imm->X_op != O_constant)
    {
      strcpy(line_mne, "lpc");
    }
  }
}

/* Transform SIMD instruction with 16-bit immediate field into
   8-bit immediate form (mnemonic with 'b' ending) */
static void
optimize_vimm(char *line_mne,
	      int *rhs_rd,
	      int fmt,
	      expressionS *rhs_imm0,
	      expressionS *rhs_imm1)
{
  if (TOLOWER(line_mne[0]) != 'v')
    return; /* not a SIMD instruction */

  if (!(*rhs_rd >= REG_V0 && *rhs_rd <= REG_V15))
    return; /* avoid vmov r0,v1[n] */

  if (TOLOWER(line_mne[1]) == 's' && rhs_imm1->X_op == O_absent)
    return; /* avoid vseq v0,v1,bytemask */

  /* vmov vd[s],vn/rn/imm: optimization concerns the second immediate */
  if (fmt == FMT_INDEX0)
    rhs_imm0 = rhs_imm1;

  if (rhs_imm0->X_op == O_constant &&
      rhs_imm0->X_add_number >= 0 &&
      rhs_imm0->X_add_number <= 255)
  {
    char new_mne[40];
    assert(strlen(line_mne) < 39);
    strcpy(new_mne, line_mne);
    strcat(new_mne, "b");
    if (exists_iparams_for_mne(new_mne)) /* does the instruction have a byte variety? */
      strcpy(line_mne, new_mne);         /* change to byte */
  }
}

/* Transform new-style names to the obsolete names still used by binutils.  */
static void
transform_unsigned_mne(char *line_mne)
{
  if (strcmp (line_mne, "vmaclu") == 0)
    strcpy(line_mne, "vmacl");

  if (strcmp (line_mne, "vmaxu") == 0)
    strcpy(line_mne, "vmax");

  if (strcmp (line_mne, "vminu") == 0)
    strcpy(line_mne, "vmin");

  if (strcmp (line_mne, "vmullu") == 0)
    strcpy(line_mne, "vmull");
}

/* Check registers against register field class (rfc) */
static int
check_register(int rfc, int reg)
{
  switch(rfc)
  {
    case RFC_NONE:
      return (reg == -1);
    case RFC_RREG:
      return (reg >= REG_R0 && reg <= REG_R15);
    case RFC_R0_TO_R13:
      return (reg >= REG_R0 && reg <= REG_R13);
    case RFC_R0_TO_R14:
      return (reg >= REG_R0 && reg <= REG_R14);
    case RFC_IMPLIED_R15:
      return (reg == 15);
    case RFC_VREG:
      return (reg >= REG_V0 && reg <= REG_V15);
    default:
      /* Unimplemented register field class */
      abort ();
  }
}

/* Return the number part of a register enum */
static int
reg_number_part(int reg)
{
  if (reg >= REG_R0 && reg <= REG_R15)
    return reg-REG_R0;
  if (reg >= REG_V0 && reg <= REG_V15)
    return reg-REG_V0;
  abort ();
}

/*
 * rasc_assemble -- Main entry point of RASC assembler engine.
 *
 * Parse instruction into a a struct rasc_inst. Return true on success.
 */
static bool
rasc_assemble(struct rasc_inst *inst, const char *line)
{
  unsigned int i,cnt;
  const struct idesc_params *iptable= isa_get_iptable((int *) &cnt);

  /***** Fill in default values of rasc_inst structure *****/
  inst->instr64= BADINSTR;
  inst->params= 0;
  for (i= 0; i<2; i++)
    inst->reloc_exprs[i].X_op= O_absent; /* indicate no relocation */

  int full_matchcnt= 0;
  int mne_matchcnt= 0;
  int match_no0= -1;
  int match_no1= -1;

  /* Translate nop pseudo-opcode */
  translate_nop(&line);

  /* Parse mnemonic */
  char line_mne[16];
  int mnelen= parse_mne(line_mne,line);
  if (!mnelen)
  {
    as_fatal("Mnemonic expected");
    return false;
  }

  /***** Parse right-hand side (RHS) *****/
  int rhs_rd, rhs_rn, rhs_rm, rhs_ro, rhs_fmt;
  expressionS rhs_imm[RASC_IMMS];
  int rhs_ok= parse_rhs(line_mne, line+mnelen,
			&rhs_rd, &rhs_rn, &rhs_rm, &rhs_ro,
			&rhs_imm[0], &rhs_imm[1], &rhs_fmt);
  if (!rhs_ok)
  {
    as_fatal("Unsupported addressing mode");
    return false;
  }

  /* Transform conditional branches to their canonical form */
  make_canonical_branch(line_mne, &rhs_rd, &rhs_rn, &rhs_imm[0]);

  /* Avoid br<cond> rn,rn,<range> (hence same register twice) */
  avoid_pathological_branch(line_mne, &rhs_rd, &rhs_rn, &rhs_imm[0]);

  /* Simplify "beq/bne Rd,0,label" into "beq/bne Rd,label" */
  simplify_branch(line_mne, &rhs_imm[0], &rhs_imm[1]);

  /* Transform mov's to/from special registers */
  /* also warn for the deprecated forms */
  translate_mov_special(line_mne, &rhs_rd, &rhs_rn, &rhs_imm[0]);

  /* Transform "rts" into "jmp lr" */
  translate_rts(line_mne, &rhs_rd, &rhs_rn, rhs_imm);

  /* Change the rhs of vsbm to support split 8-bit field  */
  translate_vsbm(line_mne, &rhs_rd, &rhs_rn, &rhs_imm[0], &rhs_imm[1]);

  /* Optimize mov to movn or lpc */
  optimize_mov(line_mne, &rhs_imm[0]);

  /* Optimize SIMD instruction with 16-bit immediate into 8-bit immediate */
  optimize_vimm(line_mne, &rhs_rd, rhs_fmt, &rhs_imm[0], &rhs_imm[1]);

  /* Transform new-style names to the obsolete names still used by binutils. */
  transform_unsigned_mne(line_mne);

  /***** Check against idesc_params table *****/

  for (i= 0; i<cnt; i++)
  {
    int j;
    const struct idesc_params *ip= &iptable[i];

    for (j= 0; j<ip->op_cnt; j++)
    {
      const char *mne= mne_strings[ip->mnes[j]];

      /***** Match mnemonic *****/
      if (!streq_nocase(mne,line_mne))
        continue; /* mnemonic mismatch */
      mne_matchcnt++; /* Keep track of recognized opcode */

      /***** Match addressing syntax *****/
      if (rhs_fmt != ip->format)
        continue; /* Addressing syntax mismatch */

      /***** Match RHS immediate operands against idesc 'm' and 'R' fields *****/
      if ((ip->imm[0].pos==-1) != (rhs_imm[0].X_op == O_absent))
        continue; /* First immediate field misssing or unexpected */

      if ((ip->imm[1].pos==-1) != (rhs_imm[1].X_op == O_absent))
        continue; /* Second immediate field misssing or unexpected */

      /***** Match RHS register operands against ip *****/

      /* Check registers against register field class (rfc) */
      if (!check_register(ip->rd.rfc, rhs_rd) ||
          !check_register(ip->rn.rfc, rhs_rn) ||
          !check_register(ip->rm.rfc, rhs_rm) ||
          !check_register(ip->ro.rfc, rhs_ro))
        continue; /* Register mismatch */

      /* instruction match complete */
      full_matchcnt++;              /* count this match  */
      match_no0= match_no1;         /* for nagging about multiple matches */
      match_no1= i;                 /* -"- */

      /***** Handle register implied opcode bit *****/
      if (ip->rd_carries_opcode)
      {
        assert(rhs_rd >= 0 && rhs_rn >= 0); /* only applicable to two-reg instr */
        if ((rhs_rd > rhs_rn) != (j&1)) /* LSB of opcode not matching Rd>Rn? */
        {
          int t= rhs_rd; rhs_rd= rhs_rn; rhs_rn= t; /* swap operands */
        }
      }

      /***** Build instruction *****/
      inst->instr64= ip->ival;
      inst->mne= mne_strings[ip->mnes[j]];
      inst->params= ip; /* keep information about fields for later processing */
      if (ip->op_pos >= 0)
	inst->instr64 |= ((rasc_u64) (j>>ip->rd_carries_opcode)) << ip->op_pos;
      if (ip->rd.pos >= 0)
	inst->instr64 |= ((rasc_u64) reg_number_part(rhs_rd)) << ip->rd.pos;
      if (ip->rn.pos >= 0)
	inst->instr64 |= ((rasc_u64) reg_number_part(rhs_rn)) << ip->rn.pos;
      if (ip->rm.pos >= 0)
	inst->instr64 |= ((rasc_u64) reg_number_part(rhs_rm)) << ip->rm.pos;
      if (ip->ro.pos >= 0)
	inst->instr64 |= ((rasc_u64) reg_number_part(rhs_ro)) << ip->ro.pos;

      /* Insert immediates and reloc_exprs */
      rasc_insert_constants(inst, rhs_imm);

#ifndef DEBUG_IDESC_PARAMS
      return true;
#endif
    } /* end for j */
  } /* end for i */

  if (full_matchcnt == 1)
    return true; /* Unambigous assembling done */

  if (full_matchcnt > 1)
    as_fatal("Ambiguous instruction - idesc entries %d and %d both match '%s'",
	     match_no0, match_no1, line);
  else if (!mne_matchcnt)
    as_fatal("Unknown mnemonic %s", line_mne);
  else
    as_fatal("Bad operand or addressing mode for %s instruction", line_mne);

  return false;
}

/****************************************************************************
 *
 * Instruction emitting
 *
 */

/**************************************************************/

static int
get_reloc_code_for_appendix(int is_suffix, int bytes)
{
  assert(is_suffix);

  if (bytes == 1)
    return BFD_RELOC_8;
  if (bytes == 2)
    return BFD_RELOC_16;
  if (bytes == 3)
    return BFD_RELOC_24;
  if (bytes == 4)
    return BFD_RELOC_32;

  /* no reloc code available */
  abort();
}

/* Check if immediate field needs appendix (prefix/suffix words).  */
/* Return no. of extension bytes needed.  */
static int
inst_check_imm_appendix(const struct rasc_inst *inst, int k,
			int *suffix, int *suffix_code)
{
  const struct idesc_params *ip = inst->params;
  const expressionS *expr = &inst->reloc_exprs[k];
  int val = expr->X_add_number;
  int bytes_needed = 0;

  if (expr->X_op != O_constant)
  {
    /* Unresolved value, assume a default size is needed.  */
    bytes_needed = DEFAULT_IMM_SIZE / 8;
  }
  else
  {
    /* Range check, and check against reserved codes.  */
    if (isa_imm_fits_value(&ip->imm[k], val))
      bytes_needed = 0; /* fits directly */
    else if (val & 0xff000000)
      bytes_needed = 4;
    else if (val & 0x00ff0000)
      bytes_needed = 3;
    else if (val & 0x0000ff00)
      bytes_needed = 2;
    else
      bytes_needed = 1;
  }

  if (bytes_needed)
  {
    int len;
    len = isa_select_appendix(&ip->imm[k], bytes_needed, suffix, suffix_code);
    if (!len)
      as_bad("Immediate larger than the available appendix modes "
	     "(need %d bytes)", bytes_needed);

    return len;
  }

  return 0;  /* No appendix needed.  */
}

/* Emit instruction (with reloc/relax if applicable) into the current frag. */
/* Return no. of bytes emitted                                              */
static void
rasc_emit(const struct rasc_inst *inst)
{
  /***** Check immediate fields for relaxation and relocation *****/

  int unresolved[RASC_IMMS]= {0};
  int reloc_types[RASC_IMMS]= {0};
  int i;
  int relax_field= -1, relax_type= C_NONE;
  int appendix_field= -1, appendix_is_suffix= 0, appendix_bytes= 2;
  int appendix_reloc= BFD_RELOC_NONE;
  int suffix_code= 0, suffix_len= 0;

  /* The size of the instruction is unknown, so tie the debug info to the
     start of the instruction.  */
  /* TODO: Check what we win by using correct length (and if we need to
     update it when relaxing. */
  dwarf2_emit_insn(0);

  /* Any field that needs relocation? */
  for (i= 0; i<RASC_IMMS; i++)
    if (inst->reloc_exprs[i].X_op != O_absent)
    {
      /* Unresolved immediate field. */
      /* Look up relocation type for the unresolved field */
      const struct idesc_imm_field *field= &inst->params->imm[i];
      if (field->pcrel)
      {
        /* pcrel field can be subject to relaxation */
        assert(relax_field==-1); /* only one field can be relaxed */
        relax_field= i;
      }
      else
      {
        /* Only non-pcrel field can be extended for now */
        const struct reloc_inferral *inferral= find_reloc_inferral(field);
        if (!inferral)
        {
          as_bad("Relocation not supported for this instruction field");
          fprintf(stderr,"pos=%d size=%d pcrel=%d factor=%d bias=%d\n",
            field->pos,field->size,field->pcrel,field->factor,field->bias);
        }
        else
        {
          unresolved[i]= 1; /* remember to resolve it below */
          reloc_types[i]= inferral->reloc;

          /* Check if immediate seems to need the prefix/suffix */
          int is_suffix, ext_bytes;
          ext_bytes= inst_check_imm_appendix(inst, i, &is_suffix, &suffix_code);
          if (ext_bytes)
          {
            assert(appendix_field==-1); /* only one field can be prefixed */
            appendix_field= i;
            appendix_is_suffix= is_suffix;
            appendix_bytes= ext_bytes;
            appendix_reloc= get_reloc_code_for_appendix(is_suffix, ext_bytes);
          }
        }
      }
    }

  if (appendix_field >= 0 && appendix_is_suffix)
    suffix_len= appendix_bytes;

  /* Don't relax branches that have no corresponding relax type */
  if (relax_field >= 0)
  {
    /* When using immediate suffixes, that affects the relax type */
    /* Figure out if relaxation is possible for the instruction */
    relax_type= rasc_get_relax_type(inst, suffix_len); /* initial relaxation type */

    if (relax_type == C_NONE)
    {
      as_bad("Don't know how to relax this instruction");
      relax_field= -1;
    }
    else /* Note field details, for relaxation */
      relax_keep_field_info(relax_type, &inst->params->imm[relax_field]);
  }

  /**** Emit instruction word ****/

  /* Allocate space in frag. This includes suffix_len */
  char *output;
  fragS *frag_to_fix;
  if (relax_field >= 0)
  {
    /* Use a relaxable frag */
    /* Check the minimum and maximum instruction length after relaxation */
    /* All possible suffix lengths are stored as separate entries in the
     * relaxation table. */
    int min_length, max_length;
    get_min_max_relax_len(relax_type,&min_length,&max_length);

    /* Hack to get the pointer to the frag_var fragment:
       Ensure first that it will fit in frag_now */
    frag_grow(max_length);
    frag_to_fix= frag_now; /* frag_var will open a new frag */

    output= frag_var(rs_machine_dependent,
       max_length,min_length,relax_type,
       inst->reloc_exprs[relax_field].X_add_symbol,
       inst->reloc_exprs[relax_field].X_add_number, 0);

    /* For relaxable instructions, md_convert_frag will set up the fixup */
    unresolved[relax_field]= 0; /* no fixup to be done right now */
  }
  else
  {
    output= frag_more(inst->params->ibytes + suffix_len); /* Use a fixed frag */
    frag_to_fix= frag_now;
  }

  rasc_u64 tmp= inst->instr64;

  /* Insert suffix code into instruction word */
  if (appendix_field >= 0 && appendix_is_suffix)
  {
    tmp |= ((rasc_u64) suffix_code) << inst->params->imm[appendix_field].pos;
    unresolved[appendix_field]= 0; /* Don't add fixup as well */
  }

  /* Write the instruction into the frag */
  for (i= 0; i<inst->params->ibytes; i++)
  {
    output[i]= (char) tmp;
    tmp >>= 8;
  }

  /* Add fixups for the instruction word */
  for (i= 0; i<RASC_IMMS; i++)
    if (unresolved[i])
    {
      expressionS *exp= (expressionS *) &inst->reloc_exprs[i];
      fixS *fix=
        fix_new_exp (frag_to_fix,         /* frag: the code fragment affected */
          output-frag_to_fix->fr_literal, /* where: offset of instruction within fragment */
          2,exp,                          /* size,exp                         */
          inst->params->imm[i].pcrel,     /* pcrel                            */
          reloc_types[i]);                /* r_type                           */

      /* Keep track of the bias of the relocated field for md_apply_fix */
      fix->fx_addnumber= -inst->params->imm[i].bias;
    }

  /* Add fixup for the immediate suffix, if any */
  if (suffix_len)
  {
    expressionS *exp= (expressionS *) &inst->reloc_exprs[appendix_field];
    if (exp->X_op == O_constant)
    {
      /* Immediate constant.  No need to add a relocation. */
      int val = exp->X_add_number;
      for (i = 0; i < suffix_len; i++)
      {
	output[inst->params->ibytes + i] = val;
	val = val >> 8;
      }
    }
    else
    {
      /* Add relocation info */
      fixS *fix =
	fix_new_exp (frag_to_fix,
		     output + inst->params->ibytes - frag_to_fix->fr_literal,
		     appendix_bytes,exp,0,appendix_reloc);

      /* Keep track of the bias of the relocated field for md_apply_fix */
      fix->fx_addnumber= -inst->params->imm[appendix_field].bias;
    }
  }

  /* Keep track of the instruction so that we know how to skip it */
  track_instruction(frag_to_fix, output, inst->params->ibytes + suffix_len);
}

/* This is the guts of the machine-dependent assembler.  STR points to a
   machine dependent instruction.  This function is supposed to emit
   the frags/bytes it assembles to.  */
void
md_assemble (char *str)
{
  struct rasc_inst inst;
  /* Drop leading whitespace.  */
  while (ISSPACE (* str))
    str ++;

  /* Call the main RASC assembler */
  if (rasc_assemble(&inst, str))
  {
    assert(inst.instr64 != BADINSTR); /* rasc_assemble would have returned false. */

    /* Emit instruction, relaxable or relocatable where applicable */
    rasc_emit(&inst);
  }
}

/* Trick to override .text into .rodata */
static void
rasc_s_section (int ignore)
{
/*#define HAVE_TEXT_SECTION_LITERALS*/
#ifdef HAVE_TEXT_SECTION_LITERALS
  /* Overide .rodata or .rodata.xxx... into .text  */
  char * ilp = input_line_pointer;

  while (*ilp != 0 && ISSPACE (*ilp))
    ++ ilp;

  if (strncmp (ilp, ".rodata", 7) == 0
      && (ISSPACE (ilp[5]) || *ilp == '\n' || *ilp == '\r' || *ilp == '.'))
  {
    /* skip the rest of the line */
    while (*ilp != 0 && *ilp != ';' && *ilp != '\n' && *ilp != '\r')
      ilp++;
    input_line_pointer = ilp;
    obj_elf_text (ignore);
    return;
  }
#endif
  obj_elf_section (ignore);
}

symbolS *
md_undefined_symbol (char *name ATTRIBUTE_UNUSED)
{
  return 0;
}

/* This function is called once, at assembler startup time.  This should
   set up all the tables, etc that the MD part of the assembler needs.  */
void
md_begin (void)
{
  /* This is both a simplification (we don't have to write md_apply_fix)
     and support for optimizations such as branch shortening in the linker. */
  linkrelax = 1;
}

void
rasc_end (void)
{
  subseg_set (text_section, 0);
}

/* Various routines to kill one day.  */
/* Equal to MAX_PRECISION in atof-ieee.c */
#define MAX_LITTLENUMS 6

/* Turn a string in input_line_pointer into a floating point constant of type
   type, and store the appropriate bytes in *litP.  The number of LITTLENUMS
   emitted is stored in *sizeP.  An error message is returned, or NULL on OK.*/
char *
md_atof (int type, char * litP, int * sizeP)
{
  int prec;
  LITTLENUM_TYPE words[MAX_LITTLENUMS];
  int    i;
  char * t;

  switch (type)
    {
    case 'f':
    case 'F':
    case 's':
    case 'S':
      prec = 2;
      break;

    case 'd':
    case 'D':
    case 'r':
    case 'R':
      prec = 4;
      break;

    case 'x':
    case 'X':
      prec = 6;
      break;

    case 'p':
    case 'P':
      prec = 6;
      break;

    default:
      *sizeP = 0;
      return _("Bad call to MD_NTOF()");
    }

  t = atof_ieee (input_line_pointer, type, words);

  if (t)
    input_line_pointer = t;

  *sizeP = prec * sizeof (LITTLENUM_TYPE);

  for (i = prec - 1; i >= 0; i--)
    {
      md_number_to_chars (litP, (valueT) words[i], sizeof (LITTLENUM_TYPE));
      litP += sizeof (LITTLENUM_TYPE);
    }

  return 0;
}

const char *md_shortopts = "m:";
struct option md_longopts[] = {
  { "max-skip", required_argument, NULL, 0},
  { NULL, no_argument, NULL, 0}
};

size_t md_longopts_size = sizeof (md_longopts);

int
md_parse_option (int c ATTRIBUTE_UNUSED, char *arg ATTRIBUTE_UNUSED)
{
  if (c == 'm'
      && strncmp(arg, "max-skip=", 9) == 0
      && (arg[9] >= '0' && arg[9] <= '8')
      && arg[10] == '\0')
  {
      max_bcc3_skip_cnt = arg[9] - '0';
      return 1;
  }
  return 0;
}

void
md_show_usage (FILE *stream ATTRIBUTE_UNUSED)
{
}

int md_short_jump_size;

void
md_create_short_jump (char * ptr ATTRIBUTE_UNUSED,
		      addressT from_Nddr ATTRIBUTE_UNUSED,
		      addressT to_Nddr ATTRIBUTE_UNUSED,
		      fragS * frag ATTRIBUTE_UNUSED,
		      symbolS * to_symbol ATTRIBUTE_UNUSED)
{
  as_fatal (_("failed sanity check: short_jump"));
}

void
md_create_long_jump (char * ptr ATTRIBUTE_UNUSED,
		     addressT from_Nddr ATTRIBUTE_UNUSED,
		     addressT to_Nddr ATTRIBUTE_UNUSED,
		     fragS * frag ATTRIBUTE_UNUSED,
		     symbolS * to_symbol ATTRIBUTE_UNUSED)
{
  as_fatal (_("failed sanity check: long_jump"));
}


/* Applies the desired value to the specified location.
   Also sets up addends for 'rela' type relocations.  */

void
md_apply_fix (fixS *fixP, valueT *valP, segT segment ATTRIBUTE_UNUSED)
{
  /* We have linker relaxing. Leave all PC-relative relocation work to BFD */
  /* Also note we don't add the bias here, since linker must understand
     the true branch target */

  /* Save 'val' for the addend in the relocation record. */
  offsetT val = *valP;
  fixP->fx_addnumber = val;
  fixP->fx_done= 0;
}

void
md_operand (expressionS *expressionP)
{
  /* Ignore leading hash symbol, if present.  */
  if (*input_line_pointer == '#')
    {
      input_line_pointer ++;
      expression (expressionP);
    }
}

int md_long_jump_size;

/* Round up a section size to the appropriate boundary.  */
valueT
md_section_align (segT segment ATTRIBUTE_UNUSED, valueT size)
{
  return size;                  /* Byte alignment is fine */
}

/* The location from which a PC relative jump should be calculated,
   given a PC relative reloc.  */
long
md_pcrel_from_section (fixS *fixp, segT sec)
{
  /* If the symbol is undefined or defined in another section
     we leave the add number alone for the linker to fix it later.  */
  if (fixp->fx_addsy != NULL
      && (! S_IS_DEFINED (fixp->fx_addsy)
          || (S_GET_SEGMENT (fixp->fx_addsy) != sec)))
  {
    /* The implicit field offset is now added in md_apply_fix,
       so we can simply return zero here.  */
    return 0;
  }

  /* The case where we are going to resolve things...  */
  return fixp->fx_where + fixp->fx_frag->fr_address;
}

/* Based on tc-mn10300.c. Suitable for linker relaxation.
   Supports expressions that subtract a label from another. */
arelent *
tc_gen_reloc (asection *section, fixS *fixp)
{
  arelent *reloc;
  reloc = (arelent *) xmalloc (sizeof (arelent));

  reloc->howto = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  if (reloc->howto == (reloc_howto_type *) NULL)
  {
    as_bad_where (fixp->fx_file, fixp->fx_line,
                  _("reloc %d not supported by object file format"),
                  (int) fixp->fx_r_type);
    return NULL;
  }
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;

  if (fixp->fx_subsy
      && S_GET_SEGMENT (fixp->fx_subsy) == absolute_section)
  {
    fixp->fx_offset -= S_GET_VALUE (fixp->fx_subsy);
    fixp->fx_subsy = 0;
  }

  if (fixp->fx_addsy && fixp->fx_subsy)
  {
    reloc->sym_ptr_ptr = NULL;

    /* If we got a difference between two symbols, and the
       subtracted symbol is in the current section, use a
       PC-relative relocation.  If both symbols are in the same
       section, the difference would have already been simplified
       to a constant.  */
    if (S_GET_SEGMENT (fixp->fx_subsy) == section)
    {
      enum bfd_reloc_code_real pcrel_reloc= BFD_RELOC_NONE;

      reloc->sym_ptr_ptr = (asymbol **) xmalloc (sizeof (asymbol *));
      *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
      reloc->addend = (reloc->address - S_GET_VALUE (fixp->fx_subsy)
                       + fixp->fx_offset);

      switch (fixp->fx_r_type)
      {
        case BFD_RELOC_8:
	  pcrel_reloc = BFD_RELOC_8_PCREL;
	  break;
        case BFD_RELOC_16:
	  pcrel_reloc = BFD_RELOC_16_PCREL;
	  break;
        case BFD_RELOC_24:
	  pcrel_reloc = BFD_RELOC_24_PCREL;
	  break;
        case BFD_RELOC_32:
	  pcrel_reloc = BFD_RELOC_32_PCREL;
	  break;
        default:
	  break; /* Try to compute the absolute value below.  */
      }

      if (pcrel_reloc != BFD_RELOC_NONE)
      {
        reloc->howto = bfd_reloc_type_lookup (stdoutput, pcrel_reloc);
        return reloc;
      }
    }

    if ((S_GET_SEGMENT (fixp->fx_addsy) != S_GET_SEGMENT (fixp->fx_subsy))
        || S_GET_SEGMENT (fixp->fx_addsy) == undefined_section)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
                    "Difference of symbols in different sections is not supported");
    }
    else
    {
      char *fixpos = fixp->fx_where + fixp->fx_frag->fr_literal;
      int size = 0;
      reloc->addend = (S_GET_VALUE (fixp->fx_addsy)
                       - S_GET_VALUE (fixp->fx_subsy) + fixp->fx_offset);

      switch (fixp->fx_r_type)
      {
        case BFD_RELOC_8:
	  size = 1;
	  break;
        case BFD_RELOC_16:
	  size = 2;
	  break;
        case BFD_RELOC_24:
	  size= 3;
	  break;
        case BFD_RELOC_32:
	  size= 4;
	  break;
        default:
          reloc->sym_ptr_ptr = (asymbol **) bfd_abs_section_ptr->symbol_ptr_ptr;
          return reloc;
      }

      md_number_to_chars (fixpos, reloc->addend, size);
    }

    if (reloc->sym_ptr_ptr)
      free (reloc->sym_ptr_ptr);
    free (reloc);
    return NULL;
  }
  else
  {
    reloc->sym_ptr_ptr = (asymbol **) xmalloc (sizeof (asymbol *));
    *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
    reloc->addend = fixp->fx_offset;
  }
  return reloc;
}

/* Return true if the fix can be handled by GAS, false if it must
   be passed through to the linker. */
bfd_boolean
rasc_fix_adjustable (fixS *fixP)
{
  /* We need the symbol name for the VTABLE entries.  */
  if (   fixP->fx_r_type == BFD_RELOC_VTABLE_INHERIT
      || fixP->fx_r_type == BFD_RELOC_VTABLE_ENTRY)
    return 0;

  return 1;
}
