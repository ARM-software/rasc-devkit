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

#ifndef _RASC_DIS_H
#define _RASC_DIS_H

#define MNE_LIST	\
    MNE_(UNUSED),	\
    MNE_(adc),		\
    MNE_(add),		\
    MNE_(and),		\
    MNE_(andn),		\
    MNE_(asr),		\
    MNE_(beq),		\
    MNE_(bge),		\
    MNE_(bhs),		\
    MNE_(ble),		\
    MNE_(blo),		\
    MNE_(bls),		\
    MNE_(blt),		\
    MNE_(bne),		\
    MNE_(br),		\
    MNE_(bsr),		\
    MNE_(bteq),		\
    MNE_(btne),		\
    MNE_(clri),		\
    MNE_(dbne),		\
    MNE_(dbpl),		\
    MNE_(jmp),		\
    MNE_(jsr),		\
    MNE_(ld),		\
    MNE_(ldb),		\
    MNE_(ldh),		\
    MNE_(lpc),		\
    MNE_(lsl),		\
    MNE_(lsr),		\
    MNE_(mfcr),		\
    MNE_(mfsr),		\
    MNE_(mov),		\
    MNE_(movh),		\
    MNE_(movn),		\
    MNE_(movw),		\
    MNE_(mtcr),		\
    MNE_(mtsr),		\
    MNE_(mul),		\
    MNE_(not),		\
    MNE_(or),		\
    MNE_(pop),		\
    MNE_(popr),		\
    MNE_(push),		\
    MNE_(rrx),		\
    MNE_(rsb),		\
    MNE_(rte),		\
    MNE_(sbc),		\
    MNE_(seti),		\
    MNE_(sextb),	\
    MNE_(sexth),	\
    MNE_(st),		\
    MNE_(stb),		\
    MNE_(sth),		\
    MNE_(sub),		\
    MNE_(vadc),		\
    MNE_(vadcb),	\
    MNE_(vadcss),	\
    MNE_(vadcssb),	\
    MNE_(vadcus),	\
    MNE_(vadcusb),	\
    MNE_(vadd),		\
    MNE_(vadd1),	\
    MNE_(vadd1b),	\
    MNE_(vaddb),	\
    MNE_(vaddss),	\
    MNE_(vaddssb),	\
    MNE_(vaddus),	\
    MNE_(vaddusb),	\
    MNE_(vand),		\
    MNE_(vandb),	\
    MNE_(vandn),	\
    MNE_(vandnb),	\
    MNE_(vasr),		\
    MNE_(vasrb),	\
    MNE_(vclip),	\
    MNE_(vclipb),	\
    MNE_(vdint),	\
    MNE_(vend),		\
    MNE_(vfld),		\
    MNE_(vfldb),	\
    MNE_(vfst),		\
    MNE_(vfstb),	\
    MNE_(vint),		\
    MNE_(vld),		\
    MNE_(vldb),		\
    MNE_(vlrot),	\
    MNE_(vlsl),		\
    MNE_(vlslb),	\
    MNE_(vlsr),		\
    MNE_(vlsrb),	\
    MNE_(vmac),		\
    MNE_(vmacb),	\
    MNE_(vmacl),	\
    MNE_(vmaclb),	\
    MNE_(vmacls),	\
    MNE_(vmaclsb),	\
    MNE_(vmaclss),	\
    MNE_(vmaclssb),	\
    MNE_(vmaclssc),	\
    MNE_(vmaclsscb),    \
    MNE_(vmaclus),	\
    MNE_(vmaclusb),	\
    MNE_(vmaclusc),	\
    MNE_(vmacluscb),    \
    MNE_(vmacss),	\
    MNE_(vmacssb),	\
    MNE_(vmacus),	\
    MNE_(vmacusb),	\
    MNE_(vmax),		\
    MNE_(vmaxb),	\
    MNE_(vmaxs),	\
    MNE_(vmaxsb),	\
    MNE_(vmin),		\
    MNE_(vminb),	\
    MNE_(vmins),	\
    MNE_(vminsb),	\
    MNE_(vmir),		\
    MNE_(vmov),		\
    MNE_(vmovb),	\
    MNE_(vmul),		\
    MNE_(vmulb),	\
    MNE_(vmull),	\
    MNE_(vmullb),	\
    MNE_(vmulls),	\
    MNE_(vmullsb),	\
    MNE_(vneg),		\
    MNE_(vnegb),	\
    MNE_(vnot),		\
    MNE_(vnotb),	\
    MNE_(vor),		\
    MNE_(vorb),		\
    MNE_(vrep),		\
    MNE_(vrot),		\
    MNE_(vsat),		\
    MNE_(vsatb),	\
    MNE_(vsbc),		\
    MNE_(vsbcb),	\
    MNE_(vsbcss),	\
    MNE_(vsbcssb),	\
    MNE_(vsbcus),	\
    MNE_(vsbcusb),	\
    MNE_(vsbm),		\
    MNE_(vsel0),	\
    MNE_(vsel1),	\
    MNE_(vsel2),	\
    MNE_(vsel3),	\
    MNE_(vseq),		\
    MNE_(vseqb),	\
    MNE_(vsext),	\
    MNE_(vsge),		\
    MNE_(vsgeb),	\
    MNE_(vsgt),		\
    MNE_(vsgtb),	\
    MNE_(vshi),		\
    MNE_(vshib),	\
    MNE_(vshs),		\
    MNE_(vshsb),	\
    MNE_(vsle),		\
    MNE_(vsleb),	\
    MNE_(vslo),		\
    MNE_(vslob),	\
    MNE_(vsls),		\
    MNE_(vslsb),	\
    MNE_(vslt),		\
    MNE_(vsltb),	\
    MNE_(vsne),		\
    MNE_(vsneb),	\
    MNE_(vst),		\
    MNE_(vstb),		\
    MNE_(vsteq),	\
    MNE_(vsteqb),	\
    MNE_(vstne),	\
    MNE_(vstneb),	\
    MNE_(vsub),		\
    MNE_(vsubb),	\
    MNE_(vsubss),	\
    MNE_(vsubssb),	\
    MNE_(vsubus),	\
    MNE_(vsubusb),	\
    MNE_(vsync),	\
    MNE_(vtrans),	\
    MNE_(vxor),		\
    MNE_(vxorb),	\
    MNE_(vzext),	\
    MNE_(wait),		\
    MNE_(xor),		\
    MNE_(zextb),	\
    MNE_(zexth)

enum mne_idx
{
#define MNE_(X) MNE_ ## X
  MNE_LIST
#undef MNE_
};

struct rasc_idecode
{
  char mne[11];
  enum mne_idx mne_idx;
  int rd;              /* -1 if there was no destination register           */
  int rn;              /* -1 if there was no source register                */
  int rm;              /* -1 if there was no second source register         */
  int ro;              /* -1 if there was no third source register          */
  int vd;              /* -1 if there was no SIMD destination register      */
  int vn;              /* -1 if there was no SIMD source register           */
  int vm;              /* -1 if there was no second SIMD source register    */
  int vo;              /* -1 if there was no third SIMD source register     */
  int register_list;   /* 1 if it has a register list                       */
  int post_increment;  /* 1 if it address register should be incremented    */
  int imm_present;     /* 1 if an immediate field is present                */
  int imm_val;         /* value of immediate field                          */
  int range_present;   /* 1 if a branch range is present                    */
  int range_val;       /* value of branch range                             */
  int range_is_icnt;   /* 1 if the range has instruction-counting semantics */
  int range_is_vskip;  /* 1 if the range field has vskip format             */
};

/* Disassemble single instruction into buffer */
/* Return the no. of bytes consumed */
/* Simplified API, used by rascdisasm */
int rasc_disasm(int addr, const unsigned char *ibytes, int avail_bytes, char buf[512]);

/* Instruction decode function for RASC standalone use */
/* Special interface for RASC emulator.                */
/* Return no. of bytes decoded, or 0 on failure        */
int rasc_idecode(const unsigned char *ibytes, int avail_bytes,
  struct rasc_idecode *decode);

/* Check the length of suffixes */
/* Special interface for instruction decoder FLI simulation model */
void rasc_get_length_of_suffixes(unsigned instr, int *p_imm_suffix_len,
  int *p_branch_suffix_len);

#endif /* _RASC_DIS_H */
