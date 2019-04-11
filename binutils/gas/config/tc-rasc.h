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

#ifndef TC_RASC
#define TC_RASC 1

#ifndef OBJ_ELF
#error RASC support requires ELF object format
#endif

#include "write.h"                  /* For definition of fixS */

#define MAX_TRACKED_BYTES (64)

extern void        rasc_end              PARAMS ((void));
extern bfd_boolean rasc_fix_adjustable   PARAMS ((struct fix *));
extern long        rasc_relax_frag       PARAMS ((segT, fragS *, long));
extern long        md_pcrel_from_section PARAMS ((fixS *, segT));

#define TARGET_FORMAT                   "elf32-rasc-little"
#define TARGET_ARCH                     bfd_arch_rasc
#define TARGET_BYTES_BIG_ENDIAN         0
//#define tc_fix_adjustable(FIX)          rasc_fix_adjustable (FIX)
#define tc_fix_adjustable(FIX)          0 /* Don't bother to adjust relocs.  */
#define md_relax_frag                   rasc_relax_frag  /* Use a custom relaxation mechanism */
#define md_end                          rasc_end
#define md_number_to_chars              number_to_chars_littleendian
#define MD_PCREL_FROM_SECTION(FIX, SEC) md_pcrel_from_section (FIX, SEC)
#define MD_APPLY_SYM_VALUE(FIX)         0 /* md_apply_fix3 does not take symbol value */
#define IGNORE_NONSTANDARD_ESCAPES
#define PSEUDO_LCOMM_OPTIONAL_ALIGN     /* Some pseudo-op semantic extensions.  */
#define LISTING_HEADER                  "RASC GAS Version 4.0.0"
#define LISTING_LHS_CONT_LINES          4
#define NEED_FX_R_TYPE                  1
#define EXTERN_FORCE_RELOC              0 /* No shared lib support, no need to override symbols */
#define LOCAL_LABELS_FB                 1 /* Enable local label support.  */               
//#define DIFF_EXPR_OK 


#define BADINSTR (0xeeee)
 
/* Simplified API for stand-alone use */
/* Return assembled instruction or BADINSTR on failure */
unsigned short assemble(const char *line);

#define tc_frob_label(S) rasc_frob_label (S)

extern void rasc_frob_label (symbolS *);

#endif /* TC_RASC */
