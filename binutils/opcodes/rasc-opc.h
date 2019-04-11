/*
 *  Copyright (C) 2007 ARM
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

#ifndef _RASC_OPC_H
#define _RASC_OPC_H
#include "ansidecl.h"
#include "rasc-dis.h"

#define NOP_INSTR "mov r0,r0"

/* The number of bits reserved for unknown symbols */
#define DEFAULT_IMM_SIZE (32)

/****************************************************************************/

/* Cooked information about idesc immediate/branch target field */
struct idesc_imm_field
{
  int pos;        /* LSB bit pos. -1 = invalid field               */
  int size;       /* size in bits                                  */
  int pcrel;      /* 0=absolute 1=pc relative                      */
  int factor;     /* implicit factor applied to bit field          */
  int bias;       /* implicit term added to bit field              */
  int suffixable; /* 0=cannot have suffixes, 1=might mave suffixes */ 
};

/* Register field classes */
#define RFC_NONE        (0) /* No register */
#define RFC_RREG        (1) /* r0-r15 */
#define RFC_R0_TO_R13   (2) /* r0-r14 */
#define RFC_R0_TO_R14   (3) /* r0-r14 */
#define RFC_IMPLIED_R15 (4) /* Implied to be r15, no bitfield */
#define RFC_VREG        (5) /* v0-v15 */

struct idesc_reg_field
{
  int pos;        /* LSB bit pos. -1 = invalid field */
  int rfc;        /* Register field class, RFC_xxx above */
};

/* Max no. of immediate/branch target fields in an instruction */
#define RASC_IMMS (2)

/* Cooked idesc. Specification of fieds within an instruction etc */
struct idesc_params
{
  int ibytes;               /* No. of bytes the instruction occupies */
  unsigned long long ival;  /* Bits that must be set to 1 to match instruction */
  unsigned long long imask; /* Bits that are checked against ival */ 
  int op_pos;               /* Position of opcode field, -1 = no opcode field */
  int op_size;              /* Size of opcode field,      0 = no opcode field */
  int op_cnt;               /* opcode count including Rd-carried opcode bit   */
  const enum mne_idx *mnes; /* one mnemonic for each opcode */

  struct idesc_reg_field rd;    /* Information about Rd operand */
  struct idesc_reg_field rn;    /* Information about Rn operand */
  struct idesc_reg_field rm;    /* Information about Rm operand */
  struct idesc_reg_field ro;    /* Information about Ro operand */
  int rd_carries_opcode;

  /* Information about immediates / branch ranges fields
     imm[0]: 'm' field, or 'R' field if there is no 'm' field
     imm[1]: 'R' field when there is also an 'm' field */
  struct idesc_imm_field imm[RASC_IMMS];  
  
  int format;       /* FMT_xxx specifying syntax of instruction */
};

#define REG_BITS (4)

/* Formatting of RHS side */
#define FMT_PLAIN             (0)
#define FMT_BRACKET           (1)
#define FMT_BRACKET_PLUS_PLUS (2)
#define FMT_LIST              (3)
#define FMT_INDEX             (4)
#define FMT_INDEX0            (5)

/* No. of bytes instruction granularity */
int isa_get_bytes_per_unit(void);

/* Get the decoded idesc_params table. Return no. of entries in ipcnt */
const struct idesc_params *isa_get_iptable(int *ipcnt);

/* Get the first non-pcrel field */
const struct idesc_imm_field *get_non_pcrel_field(const struct idesc_params *iparams);

/* Get the first pcrel field */
const struct idesc_imm_field *get_pcrel_field(const struct idesc_params *iparams);

/* Does the code correspond to a suffix? */
/* In that case return the no. of suffix bytes, otherwise 0 */
int isa_get_imm_suffix_bytes(const struct idesc_imm_field *imm, int code);

/* Get a specific imm suffix code. Return -1 if there is none. */
int isa_get_imm_suffix_code(int field_size, int code);

/* Does the code correspond to a suffix? */
/* In that case return the no. of suffix bytes, otherwise 0 */
int isa_get_branch_suffix_bytes(int field_size, int code);

/* Get a specific branch suffix code. Return -1 if there is none. */
int isa_get_branch_suffix_code(int field_size, int code);

/* Return 1 if BCC3 branches have instruction-counting semantics */
int isa_bcc3_icnt_semantics(void);

/* See if branch suffixes are relative to the _end_ of the suffix rather than
   the _start_ */
int isa_is_branch_suffix_rel_to_end(void);

/* Check if value fits without immediate prefix or suffix */
int isa_imm_fits_value(const struct idesc_imm_field *imm, int val);

/* Return the length of an appendix mode that fits an unsigned constant of 'bytes' size */
/* Also return whether a suffix (rather than prefix) was chosen, and in that case its code */
int isa_select_appendix(const struct idesc_imm_field *imm, int bytes, int *is_suffix, int *suffix_code);

/* Find idesc entry that corresonds to instr  */ 
/* Return pointer to idesc_params or -1 on failure. */
const struct idesc_params *isa_find_iparams(unsigned long long instr);

#endif // _RASC_OPC_H
