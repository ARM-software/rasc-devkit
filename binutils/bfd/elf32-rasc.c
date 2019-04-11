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

#include "bfd.h"
#include "sysdep.h"
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/rasc.h"

/* RELA relocs are used here...  */

static bfd_reloc_status_type rasc_elf_unsupported_reloc(bfd *, arelent *,
							asymbol *, PTR,
							asection *, bfd *,
							char **);
static bfd_boolean rasc_elf_relax_delete_bytes(bfd *, asection *, bfd_vma, int);

static reloc_howto_type * rasc_elf_howto_table [(int) R_RASC_max];

/* Sorry for the Swedish
struct reloc_howto_struct  // Långa dokumentationen finns i <build>/bfd/bfd.h
{
  unsigned int type;       // back-end specifikt nummer (R_RASC_xxx)
  unsigned int rightshift; // shiftning innan relok-skrivning
  int size;                // Verkar syfta på instruktionsordets storlek (0 = byte, 1 = short, 2 = long)
  unsigned int bitsize;    // no-of-bits in item, used for overflow check
  boolean pc_relative;     // Instruera relokeraren att dra bort 'iaddr' innan relok-skrivning
  unsigned int bitpos;     // bitpos för bitfältet som ska relokeras
  enum complain_overflow complain_on_overflow;    // Error-check-typ complain_overflow_bitfield/unsigned/signed
  bfd_reloc_status_type (*special_function)(...); // customfunktion för 'skumma' relokeringar
  char *name;              // textsträng med relokeringens namn "R_RUSC_xxx"
  boolean partial_inplace; // endast satt för R_RASC_RELATIVE; säger att addenden lagras direkt i sektionen
  bfd_vma src_mask;        // satt till 0xffffffff för R_RASC_RELATIVE, noll annars. Säger att fältet ska läsas innan relokering
  bfd_vma dst_mask;        // Mask för instruktionsfältet som ska modifieras vid relokering
  boolean pcrel_offset;    // Satt för de pc-relativa Mcore-modesen. Betyder att offsetfältet är initierat till 0
};
*/

/* We let R_RASC_PC3 be unsupported, since it has complex semantics and should
   therefore be handled by the assembler instead */
static reloc_howto_type rasc_elf_howto_raw[] =
{
  HOWTO(R_RASC_NONE,              0,2,32, FALSE, 0, complain_overflow_bitfield, NULL,                        "R_RASC_NONE",              FALSE,0,0,         FALSE),
  HOWTO(R_RASC_32,                0,2,32, FALSE, 0, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_32",                FALSE,0,0xffffffff,FALSE),
  HOWTO(R_RASC_24,                0,2,24, FALSE, 0, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_24",                FALSE,0,0x00ffffff,FALSE),
  HOWTO(R_RASC_16,                0,1,16, FALSE, 0, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_16",                FALSE,0,0x0000ffff,FALSE),
  HOWTO(R_RASC_8,                 0,0, 8, FALSE, 0, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_8",                 FALSE,0,0x000000ff,FALSE),
  HOWTO(R_RASC_32_PCREL,          0,2,32, TRUE,  0, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_32_PCREL",          FALSE ,0,0xffffffff,TRUE),
  HOWTO(R_RASC_24_PCREL,          0,2,24, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_24_PCREL",          FALSE ,0,0x00ffffff,TRUE),
  HOWTO(R_RASC_16_PCREL,          0,1,16, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_16_PCREL",          FALSE ,0,0x0000ffff,TRUE),
  HOWTO(R_RASC_8_PCREL,           0,0, 8, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_8_PCREL",           FALSE ,0,0x000000ff,TRUE),
  HOWTO(R_RASC_IMM16_HIGH12,      4,1,12, FALSE, 4, complain_overflow_bitfield, bfd_elf_generic_reloc,       "R_RASC_IMM16_HIGH12",      FALSE,0,0xfff0,    FALSE),
  HOWTO(R_RASC_IMM16_LOW3_POS5,   0,1, 3, FALSE, 5, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16_LOW3_POS5",   FALSE,0,0x00e0,    FALSE),
  HOWTO(R_RASC_IMM16_LOW4_POS4,   0,1, 4, FALSE, 4, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16_LOW4_POS4",   FALSE,0,0x00f0,    FALSE),
  HOWTO(R_RASC_IMM16_LOW4_POS8,   0,1, 4, FALSE, 8, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16_LOW4_POS8",   FALSE,0,0x0f00,    FALSE),  
  HOWTO(R_RASC_IMM16_LOW6_POS6,   0,1, 6, FALSE, 6, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16_LOW6_POS6",   FALSE,0,(0x3f<<6), FALSE),  
  HOWTO(R_RASC_IMM16BY2_LOW3_POS5,1,1, 3, FALSE, 5, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16BY2_LOW3_POS5",FALSE,0,0x00e0,    FALSE),
  HOWTO(R_RASC_IMM16BY2_LOW4_POS4,1,1, 4, FALSE, 4, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16BY2_LOW4_POS4",FALSE,0,0x00f0,    FALSE),
  HOWTO(R_RASC_IMM16BY4_LOW4_POS4,2,1, 4, FALSE, 4, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16BY4_LOW4_POS4",FALSE,0,0x00f0,    FALSE),
  HOWTO(R_RASC_IMM16BY4_LOW6_POS6,2,1, 6, FALSE, 6, complain_overflow_dont,     bfd_elf_generic_reloc,       "R_RASC_IMM16BY4_LOW6_POS6",FALSE,0,(0x3f<<6), FALSE),
  HOWTO(R_RASC_PC11BY1,           0,1,11, TRUE,  5, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC11BY1",           FALSE,0,(0x7ff<<5),TRUE),
  HOWTO(R_RASC_PC9BY1,            0,1, 9, TRUE,  3, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC9BY1",            FALSE,0,(0x1ff<<3),TRUE),
  HOWTO(R_RASC_PC7BY1,            0,1, 7, TRUE,  5, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC7BY1",            FALSE,0,(0x7f<<5), TRUE),
  HOWTO(R_RASC_PC5BY1,            0,1, 5, TRUE,  3, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC5BY1",            FALSE,0,(0x1f<<3), TRUE),
  HOWTO(R_RASC_PC3BY1,            0,0, 3, TRUE,  5, complain_overflow_unsigned, rasc_elf_unsupported_reloc,  "R_RASC_PC3BY1",            FALSE,0,(0x7<<5),  TRUE),
  HOWTO(R_RASC_PC11BY2,           1,1,11, TRUE,  5, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC11BY2",           FALSE,0,(0x7ff<<5),TRUE),
  HOWTO(R_RASC_PC9BY2,            1,1, 9, TRUE,  3, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC9BY2",            FALSE,0,(0x1ff<<3),TRUE),
  HOWTO(R_RASC_PC7BY2,            1,1, 7, TRUE,  5, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC7BY2",            FALSE,0,(0x7f<<5), TRUE),
  HOWTO(R_RASC_PC5BY2,            1,1, 5, TRUE,  3, complain_overflow_unsigned, bfd_elf_generic_reloc,       "R_RASC_PC5BY2",            FALSE,0,(0x1f<<3), TRUE),
  HOWTO(R_RASC_PC3BY2,            1,0, 3, TRUE,  5, complain_overflow_unsigned, rasc_elf_unsupported_reloc,  "R_RASC_PC3BY2",            FALSE,0,(0x7<<5),  TRUE),

  HOWTO(R_RASC_LPC_24,            0,2,24, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_LPC_24",            FALSE ,0,0x00ffffff,TRUE),
  HOWTO(R_RASC_LPC_16,            0,1,16, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_LPC_16",            FALSE ,0,0x0000ffff,TRUE),
  HOWTO(R_RASC_BR_BSR_24,         0,2,24, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_BR_BSR_24",         FALSE ,0,0x00ffffff,TRUE),
  HOWTO(R_RASC_BR_BSR_16,         0,1,16, TRUE,  0, complain_overflow_signed,   bfd_elf_generic_reloc,       "R_RASC_BR_BSR_16",         FALSE ,0,0x0000ffff,TRUE),

  HOWTO(R_RASC_GNU_VTINHERIT,     0,2, 0, FALSE, 0, complain_overflow_dont,     NULL,                        "R_RASC_GNU_VTINHERIT",     FALSE,0,0,         FALSE),
  HOWTO(R_RASC_GNU_VTENTRY,       0,2, 0, FALSE, 0, complain_overflow_dont,     _bfd_elf_rel_vtable_reloc_fn,"R_RASC_GNU_VTENTRY",       FALSE,0,0,FALSE),  
  HOWTO(R_RASC_RELATIVE,          0,2,32, FALSE, 0, complain_overflow_signed,   NULL,                        "R_RASC_RELATIVE",          TRUE, 0xffffffff,0xffffffff,FALSE)
};

#ifndef NUM_ELEM
#define NUM_ELEM(a) (sizeof (a) / sizeof (a)[0])
#endif


static int inited= 0;

/* Initialize the rasc_elf_howto_table, so that linear accesses can be done.  */
static void
rasc_elf_howto_init (void)
{
  unsigned int i;
  if (inited)
    return;

  for (i = NUM_ELEM (rasc_elf_howto_raw); i--;)
  {
    unsigned int type= rasc_elf_howto_raw[i].type;
    BFD_ASSERT (type < NUM_ELEM (rasc_elf_howto_table));
    rasc_elf_howto_table [type] = & rasc_elf_howto_raw [i];
  }
  
  inited= 1;
}

static reloc_howto_type *
rasc_elf_reloc_type_lookup (bfd * abfd ATTRIBUTE_UNUSED,
			    bfd_reloc_code_real_type code)
{
  enum elf_rasc_reloc_type rasc_reloc = R_RASC_NONE;

  switch (code)
    {
    case BFD_RELOC_NONE:                    rasc_reloc = R_RASC_NONE; break;
    case BFD_RELOC_32:                      rasc_reloc = R_RASC_32; break;
    case BFD_RELOC_24:                      rasc_reloc = R_RASC_24; break;
    case BFD_RELOC_16:                      rasc_reloc = R_RASC_16; break;
    case BFD_RELOC_8:                       rasc_reloc = R_RASC_8; break;
    case BFD_RELOC_32_PCREL:                rasc_reloc = R_RASC_32_PCREL; break;
    case BFD_RELOC_24_PCREL:                rasc_reloc = R_RASC_24_PCREL; break;
    case BFD_RELOC_16_PCREL:                rasc_reloc = R_RASC_16_PCREL; break;
    case BFD_RELOC_8_PCREL:                 rasc_reloc = R_RASC_8_PCREL; break;
    case BFD_RELOC_RASC_IMM16_HIGH12:       rasc_reloc = R_RASC_IMM16_HIGH12; break;
    case BFD_RELOC_RASC_IMM16_LOW3_POS5:    rasc_reloc = R_RASC_IMM16_LOW3_POS5; break;
    case BFD_RELOC_RASC_IMM16_LOW4_POS4:    rasc_reloc = R_RASC_IMM16_LOW4_POS4; break;
    case BFD_RELOC_RASC_IMM16_LOW4_POS8:    rasc_reloc = R_RASC_IMM16_LOW4_POS8; break;
    case BFD_RELOC_RASC_IMM16_LOW6_POS6:    rasc_reloc = R_RASC_IMM16_LOW6_POS6; break;
    case BFD_RELOC_RASC_IMM16BY2_LOW3_POS5: rasc_reloc = R_RASC_IMM16BY2_LOW3_POS5; break;
    case BFD_RELOC_RASC_IMM16BY2_LOW4_POS4: rasc_reloc = R_RASC_IMM16BY2_LOW4_POS4; break;
    case BFD_RELOC_RASC_IMM16BY4_LOW4_POS4: rasc_reloc = R_RASC_IMM16BY4_LOW4_POS4; break;
    case BFD_RELOC_RASC_IMM16BY4_LOW6_POS6: rasc_reloc = R_RASC_IMM16BY4_LOW6_POS6; break;
    case BFD_RELOC_RASC_PC11BY1:            rasc_reloc = R_RASC_PC11BY1; break;
    case BFD_RELOC_RASC_PC9BY1:             rasc_reloc = R_RASC_PC9BY1; break;
    case BFD_RELOC_RASC_PC7BY1:             rasc_reloc = R_RASC_PC7BY1; break;
    case BFD_RELOC_RASC_PC5BY1:             rasc_reloc = R_RASC_PC5BY1; break;
    case BFD_RELOC_RASC_PC3BY1:             rasc_reloc = R_RASC_PC3BY1; break;
    case BFD_RELOC_RASC_PC11BY2:            rasc_reloc = R_RASC_PC11BY2; break;
    case BFD_RELOC_RASC_PC9BY2:             rasc_reloc = R_RASC_PC9BY2; break;
    case BFD_RELOC_RASC_PC7BY2:             rasc_reloc = R_RASC_PC7BY2; break;
    case BFD_RELOC_RASC_PC5BY2:             rasc_reloc = R_RASC_PC5BY2; break;
    case BFD_RELOC_RASC_PC3BY2:             rasc_reloc = R_RASC_PC3BY2; break;
    case BFD_RELOC_RASC_LPC_24:             rasc_reloc = R_RASC_LPC_24; break;
    case BFD_RELOC_RASC_LPC_16:             rasc_reloc = R_RASC_LPC_16; break;
    case BFD_RELOC_RASC_BR_BSR_24:          rasc_reloc = R_RASC_BR_BSR_24; break;
    case BFD_RELOC_RASC_BR_BSR_16:          rasc_reloc = R_RASC_BR_BSR_16; break;
    case BFD_RELOC_VTABLE_INHERIT:          rasc_reloc = R_RASC_GNU_VTINHERIT; break;
    case BFD_RELOC_VTABLE_ENTRY:            rasc_reloc = R_RASC_GNU_VTENTRY; break;
    case BFD_RELOC_RVA:                     rasc_reloc = R_RASC_RELATIVE; break;
    default:
      return (reloc_howto_type *)NULL;
    }

  rasc_elf_howto_init();  /* Initialize howto table if needed */

  return rasc_elf_howto_table [(int) rasc_reloc];
};

static reloc_howto_type *
rasc_elf_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED,
				 const char *r_name)
{
  unsigned int i;

  for (i = 0;
       i < (sizeof (rasc_elf_howto_table)
	    / sizeof (rasc_elf_howto_table[0]));
       i++)
    if (rasc_elf_howto_table[i]->name != NULL
	&& strcasecmp (rasc_elf_howto_table[i]->name, r_name) == 0)
      return rasc_elf_howto_table[i];

  return NULL;
}

/* Set the howto pointer for a RCE ELF reloc.  */
static void
rasc_elf_info_to_howto (bfd * abfd ATTRIBUTE_UNUSED,
			arelent * cache_ptr, Elf_Internal_Rela *dst)
{
  rasc_elf_howto_init();  /* Initialize howto table if needed */

  BFD_ASSERT (ELF32_R_TYPE (dst->r_info) < (unsigned int) R_RASC_max);

  cache_ptr->howto = rasc_elf_howto_table [ELF32_R_TYPE (dst->r_info)];
}

/* Function to set whether a module needs the -mrelocatable bit set.  */
static bfd_boolean
rasc_elf_set_private_flags (bfd *abfd, flagword flags)
{
  BFD_ASSERT (! elf_flags_init (abfd)
              || elf_elfheader (abfd)->e_flags == flags);

  elf_elfheader (abfd)->e_flags = flags;
  elf_flags_init (abfd) = TRUE;
  return TRUE;
}

/* Merge backend specific data from an object file to the output
   object file when linking.  */
static bfd_boolean
rasc_elf_merge_private_bfd_data (bfd *ibfd, bfd *obfd)
{
  flagword old_flags;
  flagword new_flags;

  /* Check if we have the same endianess */
  if (! _bfd_generic_verify_endian_match (ibfd, obfd))
    return FALSE;

  if (   bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return TRUE;

  new_flags = elf_elfheader (ibfd)->e_flags;
  old_flags = elf_elfheader (obfd)->e_flags;

  if (! elf_flags_init (obfd))  /* First call, no flags set */
    {
      elf_flags_init (obfd) = TRUE;
      elf_elfheader (obfd)->e_flags = new_flags;
    }
  else if (new_flags == old_flags)      /* Compatible flags are ok */
    ;
  else
    {
      /* FIXME */
    }

  return TRUE;
}

/* Don't pretend we can deal with unsupported relocs.  */

static bfd_reloc_status_type
rasc_elf_unsupported_reloc (bfd *abfd, arelent *reloc_entry,
			    asymbol * symbol ATTRIBUTE_UNUSED,
			    PTR data ATTRIBUTE_UNUSED,
			    asection * input_section ATTRIBUTE_UNUSED,
			    bfd * output_bfd ATTRIBUTE_UNUSED,
			    char ** error_message ATTRIBUTE_UNUSED)
{
  BFD_ASSERT (reloc_entry->howto != (reloc_howto_type *)0);

  _bfd_error_handler (_("%B: Relocation %s (%d) is not currently supported.\n"),
                      abfd,
                      reloc_entry->howto->name,
                      reloc_entry->howto->type);

  return bfd_reloc_notsupported;
}


/* The RELOCATE_SECTION function is called by the ELF backend linker
   to handle the relocations for a section.

   The relocs are always passed as Rela structures; if the section
   actually uses Rel structures, the r_addend field will always be
   zero.

   This function is responsible for adjust the section contents as
   necessary, and (if using Rela relocs and generating a
   relocatable output file) adjusting the reloc addend as
   necessary.

   This function does not have to worry about setting the reloc
   address or the reloc symbol index.

   LOCAL_SYMS is a pointer to the swapped in local symbols.

   LOCAL_SECTIONS is an array giving the section in the input file
   corresponding to the st_shndx field of each local symbol.

   The global hash table entry for the global symbols can be found
   via elf_sym_hashes (input_bfd).

   When generating relocatable output, this function must handle
   STB_LOCAL/STT_SECTION symbols specially.  The output symbol is
   going to be the section symbol corresponding to the output
   section, which means that the addend must be adjusted
   accordingly.  */

static bfd_boolean
rasc_elf_relocate_section (bfd *output_bfd, struct bfd_link_info *info,
			   bfd *input_bfd, asection *input_section,
			   bfd_byte *contents, Elf_Internal_Rela *relocs,
			   Elf_Internal_Sym *local_syms,
			   asection **local_sections)
{
  Elf_Internal_Shdr * symtab_hdr = & elf_tdata (input_bfd)->symtab_hdr;
  struct elf_link_hash_entry ** sym_hashes = elf_sym_hashes (input_bfd);
  Elf_Internal_Rela * rel = relocs;
  Elf_Internal_Rela * relend = relocs + input_section->reloc_count;
  bfd_boolean ret = TRUE;

#ifdef DEBUG
  fprintf (stderr,
           "rasc_elf_relocate_section called for %B section %s, %ld relocations%s\n",
           input_bfd,
           bfd_section_name(input_bfd, input_section),
           (long) input_section->reloc_count,
           (info->relocatable) ? " (relocatable)" : "");
#endif

  if (info->relocatable)
    return TRUE;

  rasc_elf_howto_init();  /* Initialize howto table if needed */

  for (; rel < relend; rel++)
    {
      enum elf_rasc_reloc_type    r_type = (enum elf_rasc_reloc_type) ELF32_R_TYPE (rel->r_info);
      bfd_vma                      offset = rel->r_offset;
      bfd_vma                      addend = rel->r_addend;
      bfd_reloc_status_type        r = bfd_reloc_other;
      asection *                   sec = (asection *) 0;
      reloc_howto_type *           howto;
      bfd_vma                      relocation;
      Elf_Internal_Sym *           sym = (Elf_Internal_Sym *) 0;
      unsigned long                r_symndx;
      struct elf_link_hash_entry * h = (struct elf_link_hash_entry *) 0;

      /* Unknown relocation handling */
      if ((unsigned) r_type >= (unsigned) R_RASC_max
          || ! rasc_elf_howto_table [(int)r_type])
        {
          _bfd_error_handler (_("%B: Unknown relocation type %d\n"),
                              input_bfd, (int) r_type);

          bfd_set_error (bfd_error_bad_value);
          ret = FALSE;
          continue;
        }

      howto = rasc_elf_howto_table [(int) r_type];
      r_symndx = ELF32_R_SYM (rel->r_info);

      /* Complain about known relocation that are not yet supported.  */
      if (howto->special_function == rasc_elf_unsupported_reloc)
        {
          _bfd_error_handler (_("%B: Relocation %s (%d) is not currently supported.\n"),
                              input_bfd, howto->name, (int)r_type);

          bfd_set_error (bfd_error_bad_value);
          ret = FALSE;
          continue;
        }

      if (r_symndx < symtab_hdr->sh_info)
        {
          sym = local_syms + r_symndx;
          sec = local_sections [r_symndx];
          relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
          addend = rel->r_addend;
        }
      else
        {
          bfd_boolean unresolved_reloc, warned;

          RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
                                   r_symndx, symtab_hdr, sym_hashes,
                                   h, sec, relocation,
                                   unresolved_reloc, warned);
        }

      /* RASC specific relocation preparation.
         The assembler has not added any ISA bias to the pc relative field.
         This way the linker relaxing can know what the true branch
         destinations are */
      switch(r_type)
      {
        case R_RASC_PC11BY1: addend -= -1023*1; break;
        case R_RASC_PC9BY1:  addend -= -255*1;  break;
        case R_RASC_PC7BY1:  addend -= -32*1;   break;
        case R_RASC_PC5BY1:  addend -= -29*1;   break;
        case R_RASC_PC3BY1:  addend -= 4;       break;
        case R_RASC_PC11BY2: addend -= -1023*2; break;
        case R_RASC_PC9BY2:  addend -= -255*2;  break;
        case R_RASC_PC7BY2:  addend -= -32*2;   break;
        case R_RASC_PC5BY2:  addend -= -29*2;   break;
        case R_RASC_PC3BY2:  addend -= 4;       break;
        default:                                break;
      }

#ifdef DEBUG
      fprintf (stderr, "\ttype = %s (%d), symbol index = %ld, offset = %ld, addend = %ld\n",
               howto->name, r_type, r_symndx, (long) offset, (long) addend);
#endif

      r = _bfd_final_link_relocate
        (howto, input_bfd, input_section, contents, offset, relocation, addend);

      if (r != bfd_reloc_ok)
        {
          ret = FALSE;

          switch (r)
            {
            default:
              break;

            case bfd_reloc_overflow:
              {
                const char * name;

                if (h != NULL)
                  name = h->root.root.string;
                else
                  {
                    name = bfd_elf_string_from_elf_section
                      (input_bfd, symtab_hdr->sh_link, sym->st_name);

                    if (name == NULL)
                      break;

                    if (* name == '\0')
                      name = bfd_section_name (input_bfd, sec);
                  }

                (*info->callbacks->reloc_overflow)
                  (info, (h ? &h->root : NULL), name, howto->name, (bfd_vma) 0,
                   input_bfd, input_section, offset);
              }
              break;
            }
        }
    }

#ifdef DEBUG
  fprintf (stderr, "\n");
#endif

  return ret;
}

/* This function handles relaxing. It is based on relaxing in elf-mn10200.c

   It relaxes PC relative immediate fields
     * br/bsr/lpc:24 -> br/bsr/lpc:16
     * br/bsr/lpc:16 -> br/bsr/lpc:8
*/

/* ISA specifics. Must keep in sync with rasc-opc.c */
#define ISA_SUFFIX_CODE_RR_8  (0)
#define ISA_SUFFIX_CODE_RR_16 (1)
#define ISA_SUFFIX_CODE_RR_24 (2)
#define ISA_RR_POS (6)

static bfd_boolean
rasc_elf_relax_section (bfd *abfd, asection *sec,
			struct bfd_link_info *link_info, bfd_boolean *again)
{
  Elf_Internal_Shdr *symtab_hdr;
  Elf_Internal_Rela *internal_relocs;
  Elf_Internal_Rela *irel, *irelend;
  bfd_byte *contents = NULL;
  Elf_Internal_Sym *isymbuf = NULL;

  /* Assume nothing changes.  */
  *again = FALSE;

  /* We don't have to do anything for a relocatable link, if
     this section does not have relocs, or if this is not a
     code section.  */
  if (link_info->relocatable
      || (sec->flags & SEC_RELOC) == 0
      || sec->reloc_count == 0
      || (sec->flags & SEC_CODE) == 0)
    return TRUE;

  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;

  /* Get a copy of the native relocations.  */
  internal_relocs = (_bfd_elf_link_read_relocs
                     (abfd, sec, (PTR) NULL, (Elf_Internal_Rela *) NULL,
                      link_info->keep_memory));
  if (internal_relocs == NULL)
    goto error_return;

  /* Walk through them looking for relaxing opportunities.  */
  irelend = internal_relocs + sec->reloc_count;
  for (irel = internal_relocs; irel < irelend; irel++)
  {
    bfd_vma symval;

    /* If this isn't something that can be relaxed, then ignore
       this reloc.  */
    if (ELF32_R_TYPE (irel->r_info) != (int) R_RASC_LPC_24 &&
        ELF32_R_TYPE (irel->r_info) != (int) R_RASC_LPC_16 &&
        ELF32_R_TYPE (irel->r_info) != (int) R_RASC_BR_BSR_24 &&
        ELF32_R_TYPE (irel->r_info) != (int) R_RASC_BR_BSR_16)
      continue;

    /* Get the section contents if we haven't done so already.  */
    if (contents == NULL)
      {
        /* Get cached copy if it exists.  */
        if (elf_section_data (sec)->this_hdr.contents != NULL)
          contents = elf_section_data (sec)->this_hdr.contents;
        else
          {
            if (!bfd_malloc_and_get_section (abfd, sec, &contents))
              goto error_return;
          }
      }

    /* Read this BFD's local symbols if we haven't done so already.  */
    if (isymbuf == NULL && symtab_hdr->sh_info != 0)
      {
        isymbuf = (Elf_Internal_Sym *) symtab_hdr->contents;
        if (isymbuf == NULL)
          isymbuf = bfd_elf_get_elf_syms (abfd, symtab_hdr,
                                          symtab_hdr->sh_info, 0,
                                          NULL, NULL, NULL);
        if (isymbuf == NULL)
          goto error_return;
      }

    /* Get the value of the symbol referred to by the reloc.  */
    if (ELF32_R_SYM (irel->r_info) < symtab_hdr->sh_info)
      {
        /* A local symbol.  */
        Elf_Internal_Sym *isym;
        asection *sym_sec;

        isym = isymbuf + ELF32_R_SYM (irel->r_info);
        if (isym->st_shndx == SHN_UNDEF)
          sym_sec = bfd_und_section_ptr;
        else if (isym->st_shndx == SHN_ABS)
          sym_sec = bfd_abs_section_ptr;
        else if (isym->st_shndx == SHN_COMMON)
          sym_sec = bfd_com_section_ptr;
        else
          sym_sec = bfd_section_from_elf_index (abfd, isym->st_shndx);
        symval = (isym->st_value
                  + sym_sec->output_section->vma
                  + sym_sec->output_offset);
      }
    else
      {
        unsigned long indx;
        struct elf_link_hash_entry *h;

        /* An external symbol.  */
        indx = ELF32_R_SYM (irel->r_info) - symtab_hdr->sh_info;
        h = elf_sym_hashes (abfd)[indx];
        BFD_ASSERT (h != NULL);
        if (h->root.type != bfd_link_hash_defined
            && h->root.type != bfd_link_hash_defweak)
          {
            /* This appears to be a reference to an undefined
               symbol.  Just ignore it--it will be caught by the
               regular reloc processing.  */
            continue;
          }

        symval = (h->root.u.def.value
                  + h->root.u.def.section->output_section->vma
                  + h->root.u.def.section->output_offset);
      }

    /* For simplicity of coding, we are going to modify the section
       contents, the section relocs, and the BFD symbol table.  We
       must tell the rest of the code not to free up this
       information.  It would be possible to instead create a table
       of changes which have to be made, as is done in coff-mips.c;
       that would be more work, but would require less memory when
       the linker is run.  */

    /* Try to turn a 24-bit pc-relative address load into 16-bit */
    if (ELF32_R_TYPE (irel->r_info) == (int) R_RASC_LPC_24)
    {
      bfd_vma value = symval;

      /* Deal with pc-relative gunk.  */
      value -= (sec->output_section->vma + sec->output_offset);
      //value -= (irel->r_offset + 2); // TODO: what about this?
      value -= irel->r_offset;
      value += irel->r_addend;

      /* See if the value will fit in 16 bits.  */
      /* TODO: The high value can be 0x7fff + 2 as the target will be
	 two bytes closer if we are able to relax.  But that requires
	 that there are no alignment between this location and the
	 symbol.  */
      if ((long) value < 0x7fff && (long) value > -0x8000)
      {
        unsigned char code;

        /* Get the opcode.  */
        code = bfd_get_8 (abfd, contents + irel->r_offset - 2); /* LPC */
        BFD_ASSERT(((code>>ISA_RR_POS) & 0x3) == ISA_SUFFIX_CODE_RR_24);

        /* Note that we've changed the relocs, section contents, etc.  */
        elf_section_data (sec)->relocs = internal_relocs;
        elf_section_data (sec)->this_hdr.contents = contents;
        symtab_hdr->contents = (unsigned char *) isymbuf;

        /* Fix the opcode.  */
        code &= ~(3 << ISA_RR_POS);
        code |= (ISA_SUFFIX_CODE_RR_16 << ISA_RR_POS);
        bfd_put_8 (abfd, code, contents + irel->r_offset - 2);

        /* Fix the relocation's type.  */
        irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irel->r_info),
                                     R_RASC_LPC_16);
                                     
        /* Delete one byte of data.  */
        if (!rasc_elf_relax_delete_bytes (abfd, sec,
                                             irel->r_offset + 1, 1))
          goto error_return;

        /* That will change things, so, we should relax again.
           Note that this is not required, and it may be slow.  */
        *again = TRUE;
      }
    }

    /* Try to turn a 16-bit pc-relative address load into 8-bit */
    if (ELF32_R_TYPE (irel->r_info) == (int) R_RASC_LPC_16)
    {
      bfd_vma value = symval;

      /* Deal with pc-relative gunk.  */
      value -= (sec->output_section->vma + sec->output_offset);
      //value -= (irel->r_offset + 2); // TODO: what about this?
      value -= irel->r_offset;
      value += irel->r_addend;

      /* See if the value will fit in 8 bits, note the high value is
         0x7f + 1 as the target will be one bytes closer if we are
         able to relax.  */
      if ((long) value < 0x80 && (long) value > -0x80)
      {
        unsigned char code;

        /* Get the opcode.  */
        code = bfd_get_8 (abfd, contents + irel->r_offset - 2); /* LPC */
        BFD_ASSERT(((code>>ISA_RR_POS) & 0x3) == ISA_SUFFIX_CODE_RR_16);

        /* Note that we've changed the relocs, section contents, etc.  */
        elf_section_data (sec)->relocs = internal_relocs;
        elf_section_data (sec)->this_hdr.contents = contents;
        symtab_hdr->contents = (unsigned char *) isymbuf;

        /* Fix the opcode.  */
        code &= ~(3 << ISA_RR_POS);
        code |= (ISA_SUFFIX_CODE_RR_8 << ISA_RR_POS);
        bfd_put_8 (abfd, code, contents + irel->r_offset - 2);

        /* Fix the relocation's type.  */
        irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irel->r_info),
                                     R_RASC_8_PCREL);

        /* Delete one byte of data.  */
        if (!rasc_elf_relax_delete_bytes (abfd, sec,
                                             irel->r_offset + 1, 1))
          goto error_return;

        /* That will change things, so, we should relax again.
           Note that this is not required, and it may be slow.  */
        *again = TRUE;
      }
    }

    /* Try to turn a 24-bit branch/call into 16-bit */
    if (ELF32_R_TYPE (irel->r_info) == (int) R_RASC_BR_BSR_24)
    {
      bfd_vma value = symval;

      /* Deal with pc-relative gunk.  */
      value -= (sec->output_section->vma + sec->output_offset);
      //value -= (irel->r_offset + 2); // TODO: what about this?
      value -= irel->r_offset;
      value += irel->r_addend;

      /* See if the value will fit in 16 bits, note the high value is
         0x7fff + 2 as the target will be two bytes closer if we are
         able to relax.  */
      if ((long) value < 0x8001 && (long) value > -0x8000)
      {
        unsigned char code;

        /* Get the opcode.  */
        code = bfd_get_8 (abfd, contents + irel->r_offset - 1); /* BR/BSR */
        BFD_ASSERT(((code>>ISA_RR_POS) & 0x3) == ISA_SUFFIX_CODE_RR_24);

        /* Note that we've changed the relocs, section contents, etc.  */
        elf_section_data (sec)->relocs = internal_relocs;
        elf_section_data (sec)->this_hdr.contents = contents;
        symtab_hdr->contents = (unsigned char *) isymbuf;

        /* Fix the opcode.  */
        code &= ~(3 << ISA_RR_POS);
        code |= (ISA_SUFFIX_CODE_RR_16 << ISA_RR_POS);
        bfd_put_8 (abfd, code, contents + irel->r_offset - 1);

        /* Fix the relocation's type.  */
        irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irel->r_info),
                                     R_RASC_BR_BSR_16);
                                     
        /* Delete one byte of data.  */
        if (!rasc_elf_relax_delete_bytes (abfd, sec,
                                             irel->r_offset + 1, 1))
          goto error_return;

        /* That will change things, so, we should relax again.
           Note that this is not required, and it may be slow.  */
        *again = TRUE;
      }
    }

    /* Try to turn a 16-bit branch/call into 8-bit */
    if (ELF32_R_TYPE (irel->r_info) == (int) R_RASC_BR_BSR_16)
    {
      bfd_vma value = symval;

      /* Deal with pc-relative gunk.  */
      value -= (sec->output_section->vma + sec->output_offset);
      //value -= (irel->r_offset + 2); // TODO: what about this?
      value -= irel->r_offset;
      value += irel->r_addend;

      /* See if the value will fit in 8 bits.  */
      /* TODO: The high value can be 0x7f + 2 as the target will be
	 one byte closer if we are able to relax.  But that requires
	 that there are no alignment between this location and the
	 symbol.  */
      if ((long) value < 0x7f && (long) value > -0x80)
      {
        unsigned char code;

        /* Get the opcode.  */
        code = bfd_get_8 (abfd, contents + irel->r_offset - 1); /* BR/BSR */
        BFD_ASSERT(((code>>ISA_RR_POS) & 0x3) == ISA_SUFFIX_CODE_RR_16);

        /* Note that we've changed the relocs, section contents, etc.  */
        elf_section_data (sec)->relocs = internal_relocs;
        elf_section_data (sec)->this_hdr.contents = contents;
        symtab_hdr->contents = (unsigned char *) isymbuf;

        /* Fix the opcode.  */
        code &= ~(3 << ISA_RR_POS);
        code |= (ISA_SUFFIX_CODE_RR_8 << ISA_RR_POS);
        bfd_put_8 (abfd, code, contents + irel->r_offset - 1);

        /* Fix the relocation's type.  */
        irel->r_info = ELF32_R_INFO (ELF32_R_SYM (irel->r_info),
                                     R_RASC_8_PCREL);

        /* Delete one byte of data.  */
        if (!rasc_elf_relax_delete_bytes (abfd, sec,
                                             irel->r_offset + 1, 1))
          goto error_return;

        /* That will change things, so, we should relax again.
           Note that this is not required, and it may be slow.  */
        *again = TRUE;
      }
    }

  } /* end for irel */

  if (isymbuf != NULL
      && symtab_hdr->contents != (unsigned char *) isymbuf)
    {
      if (! link_info->keep_memory)
        free (isymbuf);
      else
        {
          /* Cache the symbols for elf_link_input_bfd.  */
          symtab_hdr->contents = (unsigned char *) isymbuf;
        }
    }

  if (contents != NULL
      && elf_section_data (sec)->this_hdr.contents != contents)
    {
      if (! link_info->keep_memory)
        free (contents);
      else
        {
          /* Cache the section contents for elf_link_input_bfd.  */
          elf_section_data (sec)->this_hdr.contents = contents;
        }
    }

  if (internal_relocs != NULL
      && elf_section_data (sec)->relocs != internal_relocs)
    free (internal_relocs);

  return TRUE;

 error_return:
  if (isymbuf != NULL
      && symtab_hdr->contents != (unsigned char *) isymbuf)
    free (isymbuf);
  if (contents != NULL
      && elf_section_data (sec)->this_hdr.contents != contents)
    free (contents);
  if (internal_relocs != NULL
      && elf_section_data (sec)->relocs != internal_relocs)
    free (internal_relocs);

  return FALSE;
}

/* Delete some bytes from a section while relaxing.  */

static bfd_boolean
rasc_elf_relax_delete_bytes (bfd *abfd, asection *sec, bfd_vma addr, int count)
{
  Elf_Internal_Shdr *symtab_hdr;
  unsigned int sec_shndx;
  bfd_byte *contents;
  Elf_Internal_Rela *irel, *irelend;
  Elf_Internal_Rela *irelalign;
  bfd_vma toaddr;
  Elf_Internal_Sym *isym;
  Elf_Internal_Sym *isymend;
  struct elf_link_hash_entry **sym_hashes;
  struct elf_link_hash_entry **end_hashes;
  unsigned int symcount;

  sec_shndx = _bfd_elf_section_from_bfd_section (abfd, sec);

  contents = elf_section_data (sec)->this_hdr.contents;

  /* The deletion must stop at the next ALIGN reloc for an aligment
     power larger than the number of bytes we are deleting.  */

  irelalign = NULL;
  toaddr = sec->size;

  irel = elf_section_data (sec)->relocs;
  irelend = irel + sec->reloc_count;

  /* Actually delete the bytes.  */
  memmove (contents + addr, contents + addr + count,
           (size_t) (toaddr - addr - count));
  sec->size -= count;

  /* Adjust all the relocs.  */
  for (irel = elf_section_data (sec)->relocs; irel < irelend; irel++)
    {
      /* Get the new reloc address.  */
      if ((irel->r_offset > addr
           && irel->r_offset < toaddr))
        irel->r_offset -= count;
    }

  /* Adjust the local symbols defined in this section.  */
  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  isym = (Elf_Internal_Sym *) symtab_hdr->contents;
  for (isymend = isym + symtab_hdr->sh_info; isym < isymend; isym++)
    {
      if (isym->st_shndx == sec_shndx
          && isym->st_value > addr
          && isym->st_value < toaddr)
        isym->st_value -= count;
    }

  /* Now adjust the global symbols defined in this section.  */
  symcount = (symtab_hdr->sh_size / sizeof (Elf32_External_Sym)
              - symtab_hdr->sh_info);
  sym_hashes = elf_sym_hashes (abfd);
  end_hashes = sym_hashes + symcount;
  for (; sym_hashes < end_hashes; sym_hashes++)
    {
      struct elf_link_hash_entry *sym_hash = *sym_hashes;
      if ((sym_hash->root.type == bfd_link_hash_defined
           || sym_hash->root.type == bfd_link_hash_defweak)
          && sym_hash->root.u.def.section == sec
          && sym_hash->root.u.def.value > addr
          && sym_hash->root.u.def.value < toaddr)
        {
          sym_hash->root.u.def.value -= count;
        }
    }

  return TRUE;
}


/* Return the section that should be marked against GC for a given
   relocation.  */

static asection *
rasc_elf_gc_mark_hook (asection *sec,
		       struct bfd_link_info *info ATTRIBUTE_UNUSED,
		       Elf_Internal_Rela *rel, struct elf_link_hash_entry *h,
		       Elf_Internal_Sym *sym)
{
  if (h != NULL)
    {
      switch (ELF32_R_TYPE (rel->r_info))
        {
        case R_RASC_GNU_VTINHERIT:
        case R_RASC_GNU_VTENTRY:
          break;

        default:
          switch (h->root.type)
            {
            case bfd_link_hash_defined:
            case bfd_link_hash_defweak:
              return h->root.u.def.section;

            case bfd_link_hash_common:
              return h->root.u.c.p->section;

            default:
              break;
            }
        }
    }
  else
    return bfd_section_from_elf_index (sec->owner, sym->st_shndx);

  return NULL;
}

/* Update the got entry reference counts for the section being removed.  */

static bfd_boolean
rasc_elf_gc_sweep_hook (bfd * abfd ATTRIBUTE_UNUSED,
			struct bfd_link_info * info ATTRIBUTE_UNUSED,
			asection * sec ATTRIBUTE_UNUSED,
			const Elf_Internal_Rela * relocs ATTRIBUTE_UNUSED)
{
  return TRUE;
}

/* Look through the relocs for a section during the first phase.
   Since we don't do .gots or .plts, we just need to consider the
   virtual table relocs for gc.  */

static bfd_boolean
rasc_elf_check_relocs (bfd *abfd, struct bfd_link_info *info, asection *sec,
		       const Elf_Internal_Rela *relocs)
{
  Elf_Internal_Shdr * symtab_hdr;
  struct elf_link_hash_entry ** sym_hashes;
  struct elf_link_hash_entry ** sym_hashes_end;
  const Elf_Internal_Rela * rel;
  const Elf_Internal_Rela * rel_end;

  if (info->relocatable)
    return TRUE;

  symtab_hdr = & elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  sym_hashes_end = sym_hashes + symtab_hdr->sh_size / sizeof (Elf32_External_Sym);
  if (!elf_bad_symtab (abfd))
    sym_hashes_end -= symtab_hdr->sh_info;

  rel_end = relocs + sec->reloc_count;

  for (rel = relocs; rel < rel_end; rel++)
    {
      struct elf_link_hash_entry * h;
      unsigned long r_symndx;

      r_symndx = ELF32_R_SYM (rel->r_info);

      if (r_symndx < symtab_hdr->sh_info)
        h = NULL;
      else
        h = sym_hashes [r_symndx - symtab_hdr->sh_info];

      switch (ELF32_R_TYPE (rel->r_info))
        {
        /* This relocation describes the C++ object vtable hierarchy.
           Reconstruct it for later use during GC.  */
        case R_RASC_GNU_VTINHERIT:
          if (!bfd_elf_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
            return FALSE;
          break;

        /* This relocation describes which C++ vtable entries are actually
           used.  Record for later use during GC.  */
        case R_RASC_GNU_VTENTRY:
          if (!bfd_elf_gc_record_vtentry (abfd, sec, h, rel->r_addend))
            return FALSE;
          break;
        }
    }

  return TRUE;
}

static struct bfd_elf_special_section const rasc_elf_special_sections[]=
{
  { ".ctors",   6, -2, SHT_PROGBITS, SHF_ALLOC + SHF_WRITE },
  { ".dtors",   6, -2, SHT_PROGBITS, SHF_ALLOC + SHF_WRITE },
  { NULL,       0,  0, 0,            0 }
};

#define TARGET_LITTLE_SYM       bfd_elf32_rasc_little_vec
#define TARGET_LITTLE_NAME      "elf32-rasc-little"

#define ELF_ARCH                bfd_arch_rasc
#define ELF_MACHINE_CODE        EM_RASC
#define ELF_MAXPAGESIZE         1               /* no MMU */
#define elf_info_to_howto       rasc_elf_info_to_howto
#define elf_info_to_howto_rel   NULL

#define bfd_elf32_bfd_merge_private_bfd_data    rasc_elf_merge_private_bfd_data
#define bfd_elf32_bfd_set_private_flags         rasc_elf_set_private_flags
#define bfd_elf32_bfd_reloc_type_lookup         rasc_elf_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup         rasc_elf_reloc_name_lookup
#define bfd_elf32_bfd_relax_section             rasc_elf_relax_section
#define elf_backend_relocate_section            rasc_elf_relocate_section
#define elf_backend_gc_mark_hook                rasc_elf_gc_mark_hook
#define elf_backend_gc_sweep_hook               rasc_elf_gc_sweep_hook
#define elf_backend_check_relocs                rasc_elf_check_relocs
#define elf_backend_special_sections            rasc_elf_special_sections
#define elf_backend_can_gc_sections             1
#define elf_backend_rela_normal                 1

#include "elf32-target.h"
