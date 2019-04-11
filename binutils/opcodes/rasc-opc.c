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

#include <assert.h>
#include <string.h>
#include "rasc-opc.h"
#include <stdio.h>
#include <stdlib.h>

/* Define to enable consistency checks during normal operations. 
 * This should only be done when editing the idescs[]. */
/* #define DEBUG_IDESC_PARAMS */

/****************************************************************************/
/* idesc table - ISA binary format description */

struct idesc
{
  char *maskstr;
  int factor;           /* factor CPU applies to immediate constants */
  int bias;             /* no. CPU adds to PC before adding 2*range  */
  const char *fmt;
  const enum mne_idx mnes[8];
};

#define INF (0x10000000)

/*
 *  0 = bit must be 0 to match the instruction
 *  1 = bit must be 1 to match the instruction
 *  x = don't care
 *  AAAA = Register Rd, can be R15
 *  EEEE = Register Rd with implied opcode bit ==(Rd>Rn)
 *  BBBB = Register Rn, can be R15
 *  CCCC = Register Rm, can be R15
 *  DDDD = Register Ro, can be R15 
 *  aaaa = same as AAAA, cannot be R15
 *  eeee = same as EEEE, cannot be R15
 *  bbbb = same as BBBB, cannot be R15
 *  cccc = same as CCCC, cannot be R15
 *  dddd = same as DDDD, cannot be R15
 *  qqqq = same as AAAA, cannot be R14 or R15
 *  rrrr = same as BBBB, cannot be R14 or R15
 *  ssss = same as CCCC, cannot be R14 or R15
 *  tttt = same as DDDD, cannot be R14 or R15
 *  UUUU = Register Vd, can be R15
 *  VVVV = Register Vn, can be R15
 *  WWWW = Register Vm, can be R15
 *  XXXX = Register Vo, can be R15 
 *  iiii = immediate (non PC relative)
 *  mmmm = immediate (non PC relative), potentially with suffix codes
 *  pppp = op code
 *  jjjj = Second immediate (V-instruction skip field)
 *  RRRR = branch range (PC relative immediate field)
 * 
 *  []   - general addressing syntax with brackets
 *  [sp] - addressing syntax with SP encoded implicitly in instruction 
 *  -    - register list ending with r14
 *  sp,  - sp,Rn
 *  ,sp  - Rn,sp
 */

static struct idesc idescs[] =
{ 
  // Branch range
  { "00pppRRRmmmmaaaa", 1,4,     0, {MNE_beq, MNE_bne, MNE_blt, MNE_bge, MNE_blo, MNE_bhs, MNE_bteq, MNE_btne } }, // Rd,imm,range3
  { "00pRRRRRaaaa1111", 1,-29,   0, {MNE_dbne, MNE_dbpl} },                  // Rd,range5
  { "010ppRRRBBBBaaaa", 1,4,     0, {MNE_bls, MNE_blo, MNE_ble, MNE_blt }  },    // Rd,Rn,range3
  { "0110pRRRRRRRAAAA", 1,-32,    0, {MNE_beq, MNE_bne } },                   // Rd,0,range7
  { "01110RRRbbbbeeee", 1,4,      0, {MNE_beq, MNE_bne } },                   // Rd,Rn,range3 (op = (Rd>Rn)
  { "0111000paaaa1111", 1,0, "-",   {MNE_pop,MNE_popr} },  // Rfirst-R14
  { "01110010aaaa1111", 1,0, "-",   {MNE_push} },        // Rfirst-R14
  //"011101xxxxxx1111" is free
  { "011110RR",         1,INF,    0,  {MNE_br}   },                          // range2
  { "011111RR",         1,INF,    0,  {MNE_bsr}  },                          // range2
  // Load-store range
  { "10p0mmmmaaaaBBBB", 4,0, "[]",   {MNE_ld,  MNE_st  } },          // Rd,[Rn,imm]
  { "10p10mmmaaaaBBBB", 2,0, "[]",   {MNE_ldh, MNE_sth } },          // Rd,[Rn,imm]
  { "10p11mmmaaaaBBBB", 1,0, "[]",   {MNE_ldb, MNE_stb } },          // Rd,[Rn,imm]
  // ALU range
  { "11000pppmmmmaaaa", 1,0, 0, {MNE_add, MNE_sub, MNE_adc,   MNE_sbc,  MNE_and, MNE_or,  MNE_xor,     MNE_andn   } },  // Rd,imm4 
  { "11000pmmmmmm1111", 4,0, "sp,", {MNE_add, MNE_sub } },                                                  // sp,imm6 (signed)  
  { "11001pppmmmmaaaa", 1,0, 0, {MNE_rsb, MNE_lsl, MNE_lsr,   MNE_asr, MNE_UNUSED,MNE_UNUSED,MNE_UNUSED,MNE_UNUSED } }, // Rd,imm4 
  { "11001xRRAAAA1111", 1,INF,0,     {MNE_lpc} },                                                         // Rd,range2
  { "11010pppbbbbaaaa", 1,0, 0, {MNE_add, MNE_sub, MNE_adc,   MNE_sbc,  MNE_and, MNE_or,  MNE_xor,   MNE_andn   } },    // Rd,Rn
  { "1101000paaaa1111", 1,0, 0, {MNE_jmp, MNE_jsr} },                                                       // Rd
  { "1101001pmmmm1111", 1,0, 0, {MNE_jmp, MNE_jsr} },                                                       // Rd
  { "1101000p11111111", 1,0, 0, {MNE_rte, MNE_wait} },
  { "110101ppaaaa1111", 1,0, 0, {MNE_mfsr, MNE_mtsr, MNE_clri, MNE_seti} },                                     // Rd
  
  { "11010ppp1111aaaa", 1,0, 0, {MNE_not, MNE_rrx, MNE_sextb, MNE_zextb,MNE_sexth,MNE_zexth,MNE_UNUSED,MNE_UNUSED } },   // Rd
  { "11011pppbbbbaaaa", 1,0, 0, {MNE_rsb, MNE_lsl, MNE_lsr,   MNE_asr,  MNE_mul,MNE_UNUSED,MNE_UNUSED,MNE_UNUSED } },  // Rd,Rn
  { "11011mmmaaaa1111", 1,0, 0, {MNE_mtcr}       },                                                       // Cn,Rn
  { "11011mmm1111aaaa", 1,0, 0, {MNE_mfcr}       },                                                       // Rd,Cn
  
  { "111000ppBBBBAAAA", 1,0, 0, {MNE_mov, MNE_movh, MNE_movw,MNE_UNUSED } },                                  // Rd,Rn
  { "1110010pmmmmAAAA", 1,0, 0, {MNE_mov, MNE_movn  } },                                                    // Rd,imm4

  // VRASC Load/Moves
 
  { "1111VVVViii0UUUU00000000",         1,0,"v[]", {MNE_vrep                  } }, // Vd,Vn[imm]
  { "1111VVVViii1AAAA00000000",         1,0,"v[]", {MNE_vmov                  } }, // Rd,Vn[imm]

  // vld/vldb/vst/vstb are not implemented in the hardware (yet?)
  // { "111100ppBBBBUUUU00001000",         1,0,"[]", {MNE_vld,MNE_vst,MNE_vldb,MNE_vstb } },   // Vd,[Rn]  
  // { "111101ppBBBBUUUU00001000",         1,0,"[++]", {MNE_vld,MNE_vst,MNE_vldb,MNE_vstb } }, // Vd,[Rn++]

  { "111110ppBBBBUUUU00001000",         1,0,"[]", {MNE_vfld,MNE_vfst,MNE_vfldb,MNE_vfstb } },   // Vd,[Rn]  
  { "111111ppBBBBUUUU00001000",         1,0,"[++]", {MNE_vfld,MNE_vfst,MNE_vfldb,MNE_vfstb } }, // Vd,[Rn++]

  { "11111010BBBBqqqq01001000",         1,0,"[]", {MNE_vfst } }, // Rd,[Rn]
  // 8-bit versions of other vrasc instructions are implemented by adding 'b'
  // to the name.  It is not possible for vfst, as vfstb means something
  // different (but this instruction is so uncommon, so we are lazy and
  // use the 16-bit version instead...
  // { "11111010BBBB011101001000iiiiiiii", 1,0,"[]", {MNE_vfst } }, // imm8,[Rn]
  { "11111010BBBB111101001000iiiiiiiiiiiiiiii", 1,0,"[]", {MNE_vfst } }, // imm16,[Rn]

  { "11111110BBBBqqqq01001000",         1,0,"[++]", {MNE_vfst } }, // Rd,[Rn++]
  // 8-bit versions of other vrasc instructions are implemented by adding 'b'
  // to the name.  It is not possible for vfst, as vfstb means something
  // different (but this instruction is so uncommon, so we are lazy and
  // use the 16-bit version instead...
  // { "11111110BBBB011101001000iiiiiiii", 1,0,"[++]", {MNE_vfst } }, // imm8,[Rn++]
  { "11111110BBBB111101001000iiiiiiiiiiiiiiii", 1,0,"[++]", {MNE_vfst } }, // imm16,[Rn++]

  { "11111011BBBBqqqq01001000",         1,0,"[]", {MNE_vfstb } }, // Rd,[Rn]
  { "11111011BBBB011101001000iiiiiiii", 1,0,"[]", {MNE_vfstb } }, // imm8,[Rn]
  // 16-bit vfstb does not make any sense; it does exactly the same thing
  // as the 8-bit version.
  // { "11111011BBBB111101001000iiiiiiiiiiiiiiii", 1,0,"[]", {MNE_vfstb } }, // imm16,[Rn]

  { "11111111BBBBqqqq01001000",         1,0,"[++]", {MNE_vfstb } }, // Rd,[Rn++]
  { "11111111BBBB011101001000iiiiiiii", 1,0,"[++]", {MNE_vfstb } }, // imm8,[Rn++]
  // 16-bit vfstb does not make any sense; it does exactly the same thing
  // as the 8-bit version.
  // { "11111111BBBB111101001000iiiiiiiiiiiiiiii", 1,0,"[++]", {MNE_vfstb } }, // imm16,[Rn++]

  { "1111VVVVUUUUWWWW00010000",                         1,0,0, {MNE_vclip} },  // Vd,Vn,Vm
  { "1111VVVVUUUUssss01010000",                         1,0,0, {MNE_vclip} },  // Vd,Vn,Rm
  { "1111VVVVUUUU011101010000iiiiiiii",                 1,0,0, {MNE_vclipb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU111101010000iiiiiiiiiiiiiiii",         1,0,0, {MNE_vclip} },  // Vd,Vn,imm16
  
  // VRASC skips
  // The U and V fields of vsbm are actually used together to encode an 8-bit lane mask
  { "1111UUUUjjjjVVVV00011000",        1,0,0, {MNE_vsbm} },                        // #16*U+V,dest
  { "1111UUUUjjjjVVVV001pp000",        1,0,0, {MNE_vseq,MNE_vsteq,MNE_vsne,MNE_vstne} }, // Vd,Vn,dest
  { "1111UUUUjjjjrrrr011pp000",        1,0,0, {MNE_vseq,MNE_vsteq,MNE_vsne,MNE_vstne} }, // Vd,Rn,dest
  { "1111UUUUjjjj0111011pp000iiiiiiii",1,0,0, {MNE_vseqb,MNE_vsteqb,MNE_vsneb,MNE_vstneb} }, // Vd,imm8,dest
  { "1111UUUUjjjj1111011pp000iiiiiiiiiiiiiiii",1,0,0, {MNE_vseq,MNE_vsteq,MNE_vsne,MNE_vstne} }, // Vd,imm16,dest
  { "1111UUUUjjjjVVVV00ppp001",        1,0,0, {MNE_vslt,MNE_vsgt,MNE_vslo,MNE_vshi,MNE_vsle,MNE_vsge,MNE_vsls,MNE_vshs} }, // Vd,Vn,dest
  { "1111UUUUjjjjrrrr01ppp001",        1,0,0, {MNE_vslt,MNE_vsgt,MNE_vslo,MNE_vshi,MNE_vsle,MNE_vsge,MNE_vsls,MNE_vshs} }, // Vd,Rn,dest
  { "1111UUUUjjjj011101ppp001iiiiiiii",1,0,0, {MNE_vsltb,MNE_vsgtb,MNE_vslob,MNE_vshib,MNE_vsleb,MNE_vsgeb,MNE_vslsb,MNE_vshsb} }, // Vd,imm8,dest
  { "1111UUUUjjjj111101ppp001iiiiiiiiiiiiiiii",1,0,0, {MNE_vslt,MNE_vsgt,MNE_vslo,MNE_vshi,MNE_vsle,MNE_vsge,MNE_vsls,MNE_vshs} }, // Vd,imm16,dest

  // VRASC Add/Sub        
  { "1111VVVVUUUUWWWW00ppp010",        1,0,0, {MNE_vadd,MNE_vadcss,MNE_vaddus,MNE_vmax,MNE_vaddss,MNE_vadcus,MNE_vadc,MNE_vmaxs} }, // Vd,Vn,Vm
  { "1111VVVVUUUUssss01ppp010",        1,0,0, {MNE_vadd,MNE_vadcss,MNE_vaddus,MNE_vmax,MNE_vaddss,MNE_vadcus,MNE_vadc,MNE_vmaxs} }, // Vd,Vn,Rm
  { "1111VVVVUUUU011101ppp010iiiiiiii",1,0,0, {MNE_vaddb,MNE_vadcssb,MNE_vaddusb,MNE_vmaxb,MNE_vaddssb,MNE_vadcusb,MNE_vadcb,MNE_vmaxsb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU111101ppp010iiiiiiiiiiiiiiii",1,0,0, {MNE_vadd,MNE_vadcss,MNE_vaddus,MNE_vmax,MNE_vaddss,MNE_vadcus,MNE_vadc,MNE_vmaxs} }, // Vd,Vn,imm16
  { "1111VVVVUUUUWWWW00ppp011",        1,0,0, {MNE_vsub,MNE_vsbcss,MNE_vsubus,MNE_vmin,MNE_vsubss,MNE_vsbcus,MNE_vsbc,MNE_vmins} }, // Vd,Vn,Vm
  { "1111VVVVUUUUssss01ppp011",        1,0,0, {MNE_vsub,MNE_vsbcss,MNE_vsubus,MNE_vmin,MNE_vsubss,MNE_vsbcus,MNE_vsbc,MNE_vmins} }, // Vd,Vn,Rm
  { "1111VVVVUUUU011101ppp011iiiiiiii",1,0,0, {MNE_vsubb,MNE_vsbcssb,MNE_vsubusb,MNE_vminb,MNE_vsubssb,MNE_vsbcusb,MNE_vsbcb,MNE_vminsb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU111101ppp011iiiiiiiiiiiiiiii",1,0,0, {MNE_vsub,MNE_vsbcss,MNE_vsubus,MNE_vmin,MNE_vsubss,MNE_vsbcus,MNE_vsbc,MNE_vmins} }, // Vd,Vn,imm16

  // VRASC Logic/Shifts/Lane switches      
  { "1111VVVVUUUUWWWW000pp100",        1,0,0, {MNE_vand,MNE_vxor,MNE_vor,MNE_vandn} }, // Vd,Vn,Vm
  { "1111VVVVUUUUssss010pp100",        1,0,0, {MNE_vand,MNE_vxor,MNE_vor,MNE_vandn} }, // Vd,Vn,Rm
  { "1111VVVVUUUU0111010pp100iiiiiiii",1,0,0, {MNE_vandb,MNE_vxorb,MNE_vorb,MNE_vandnb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU1111010pp100iiiiiiiiiiiiiiii",1,0,0, {MNE_vand,MNE_vxor,MNE_vor,MNE_vandn} }, // Vd,Vn,imm16
  { "1111WWWWUUUUXXXX0010p100VVVVxxxx",1,0,0, {MNE_vint,MNE_vdint} }, // Vd,Vn,Vm,Vo

  { "1111VVVVUUUUWWWW00110100",        1,0,0, {MNE_vadd1} }, // Vd,Vn,Vm
  { "1111VVVVUUUUssss01110100",        1,0,0, {MNE_vadd1} }, // Vd,Vn,Rm
  { "1111VVVVUUUU011101110100iiiiiiii",        1,0,0, {MNE_vadd1b} }, // Vd,Vn,imm8
  { "1111VVVVUUUU111101110100iiiiiiiiiiiiiiii",1,0,0, {MNE_vadd1} }, // Vd,Vn,imm16

  { "1111UUUU00ppVVVV00111100",         1,0,0, {MNE_vneg, MNE_vsext, MNE_vnot, MNE_vzext  } }, // Vd,Vn
  { "1111UUUU000prrrr01111100",         1,0,0, {MNE_vneg, MNE_vnot            } },           // Vd,Rn               
  { "1111UUUU000p011101111100iiiiiiii", 1,0,0, {MNE_vnegb, MNE_vnotb          } },           // Vd,imm8
  { "1111UUUU000p111101111100iiiiiiiiiiiiiiii", 1,0,0, {MNE_vneg, MNE_vnot    } },           // Vd,imm16
  { "111100000100000p00111100",         1,0,0, {MNE_vtrans, MNE_vend} }, // no operands
  { "111100000100001000111100",         1,0,0, {MNE_vsync} }, // no operands
                       
  { "1111UUUU0101VVVV00111100",         1,0,0, {MNE_vmov                    } }, // Vd,Vn
  { "1111UUUU0101rrrr01111100",         1,0,0, {MNE_vmov                    } }, // Vd,Rn
  { "1111UUUU0101011101111100iiiiiiii", 1,0,0, {MNE_vmovb                   } }, // Vd,imm8
  { "1111UUUU0101111101111100iiiiiiiiiiiiiiii", 1,0,0, {MNE_vmov            } }, // Vd,imm16
                       
  { "1111UUUU0110VVVV00111100",         1,0,0, {MNE_vsat} }, // Vd,Vn
  { "1111UUUU0110rrrr01111100",         1,0,0, {MNE_vsat} }, // Vd,Rn
  { "1111UUUU0110011101111100iiiiiiii", 1,0,0, {MNE_vsatb} }, // Vd,Rn    
  { "1111UUUU0110111101111100iiiiiiiiiiiiiiii", 1,0,0, {MNE_vsat} }, // Vd,Rn    
  { "1111UUUU0111VVVV00111100",         1,0,0, {MNE_vmir} }, // Vd,Vn
  
  { "1111VVVVUUUUWWWW00000101",        1,0,0,     {MNE_vlsr} }, // Vd,Vn,Vm
  { "1111VVVVUUUUWWWW00001101",        1,0,0,     {MNE_vlsl} }, // Vd,Vn,Vm
  { "1111VVVVUUUUWWWW00010101",        1,0,0,     {MNE_vasr} }, // Vd,Vn,Vm
  { "11110iiiUUUUVVVV00011101",        1,0,"V[]", {MNE_vmov} }, // Vd[s],vn
  { "1111VVVVUUUUssss01000101",        1,0,0,     {MNE_vlsr} }, // Vd,Vn,Rm
  { "1111VVVVUUUUssss01001101",        1,0,0,     {MNE_vlsl} }, // Vd,Vn,Rm
  { "1111VVVVUUUUssss01010101",        1,0,0,     {MNE_vasr} }, // Vd,Vn,Rm
  { "11110iiiUUUUrrrr01011101",        1,0,"V[]", {MNE_vmov} }, // Vd[s],Rm
  { "1111VVVVUUUU011101000101iiiiiiii",1,0,0,     {MNE_vlsrb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU011101001101iiiiiiii",1,0,0,     {MNE_vlslb} }, // Vd,Vn,imm8
  { "1111VVVVUUUU011101010101iiiiiiii",1,0,0,     {MNE_vasrb} }, // Vd,Vn,imm8
  { "11110iiiUUUU011101011101jjjjjjjj",1,0,"V[]", {MNE_vmovb} }, // Vd[s],imm8
  { "1111VVVVUUUU111101000101iiiiiiiiiiiiiiii",1,0,0,     {MNE_vlsr} }, // Vd,Vn,imm16
  { "1111VVVVUUUU111101001101iiiiiiiiiiiiiiii",1,0,0,     {MNE_vlsl} }, // Vd,Vn,imm16
  { "1111VVVVUUUU111101010101iiiiiiiiiiiiiiii",1,0,0,     {MNE_vasr} }, // Vd,Vn,imm16
  { "11110iiiUUUU111101011101jjjjjjjjjjjjjjjj",1,0,"V[]", {MNE_vmov} }, // Vd[s],imm16
  
  { "1111VVVVWWWWUUUU001pp101",        1,0,0, {MNE_vsel0,MNE_vsel2,MNE_vsel1,MNE_vsel3} }, // Vd,Vn,Vm

  { "1111VVVViiiiWWWW000p0111UUUUxxxx", 1,0,0, {MNE_vlrot,MNE_vrot} }, // Vd,Vn,Vm,imm4
  { "1111VVVVDDDDWWWW000p1111UUUUxxxx", 1,0,0, {MNE_vlrot,MNE_vrot} }, // Vd,Vn,Vm,Ro

// VRASC Muls           
  { "1111UUUUVVVVWWWW000pp110",                1,0,0, {MNE_vmul,MNE_vmacss,MNE_vmac,MNE_vmacus} }, // Vd,Vn,Vm
  { "1111UUUUVVVVssss010pp110",                1,0,0, {MNE_vmul,MNE_vmacss,MNE_vmac,MNE_vmacus} }, // Vd,Vn,Rm
  { "1111UUUUVVVV0111010pp110iiiiiiii",        1,0,0, {MNE_vmulb,MNE_vmacssb,MNE_vmacb,MNE_vmacusb} }, // Vd,Vn,imm8
  { "1111UUUUVVVV1111010pp110iiiiiiiiiiiiiiii",1,0,0, {MNE_vmul,MNE_vmacss,MNE_vmac,MNE_vmacus} }, // Vd,Vn,imm16
  { "1111UUUUWWWWXXXX001p0110VVVVxxxx",        1,0,0, {MNE_vmull,MNE_vmulls}}, // Vd,Vn,Vm,Vo
  { "1111UUUUWWWWXXXX001p1110VVVV0xxx",        1,0,0, {MNE_vmacl,MNE_vmacls}}, // Vd,Vn,Vm,Vo
  { "1111UUUUWWWWXXXX001p1110VVVV10xx",        1,0,0, {MNE_vmaclus,MNE_vmaclss}}, // Vd,Vn,Vm,Vo
  { "1111UUUUWWWWXXXX001p1110VVVV11xx",        1,0,0, {MNE_vmaclusc,MNE_vmaclssc}}, // Vd,Vn,Vm,Vo
  
  { "1111UUUUWWWWtttt011p0110VVVVxxxx",        1,0,0, {MNE_vmull,MNE_vmulls}}, // Vd,Vn,Vm,Ro
  { "1111UUUUWWWWtttt011p1110VVVV0xxx",        1,0,0, {MNE_vmacl,MNE_vmacls}}, // Vd,Vn,Vm,Ro
  { "1111UUUUWWWWtttt011p1110VVVV10xx",        1,0,0, {MNE_vmaclus,MNE_vmaclss}}, // Vd,Vn,Vm,Ro
  { "1111UUUUWWWWtttt011p1110VVVV11xx",        1,0,0, {MNE_vmaclusc,MNE_vmaclssc}}, // Vd,Vn,Vm,Ro
  
  { "1111UUUUWWWW0111011p0110iiiiiiiiVVVVxxxx",1,0,0, {MNE_vmullb,MNE_vmullsb}}, // Vd,Vn,Vm,imm8
  { "1111UUUUWWWW0111011p1110iiiiiiiiVVVV0xxx",1,0,0, {MNE_vmaclb,MNE_vmaclsb}}, // Vd,Vn,Vm,imm8
  { "1111UUUUWWWW0111011p1110iiiiiiiiVVVV10xx",1,0,0, {MNE_vmaclusb,MNE_vmaclssb}}, // Vd,Vn,Vm,imm8
  { "1111UUUUWWWW0111011p1110iiiiiiiiVVVV11xx",1,0,0, {MNE_vmacluscb,MNE_vmaclsscb}}, // Vd,Vn,Vm,imm8
  
  { "1111UUUUWWWW1111011p0110iiiiiiiiiiiiiiiiVVVVxxxx",1,0,0, {MNE_vmull,MNE_vmulls}}, // Vd,Vn,Vm,imm16
  { "1111UUUUWWWW1111011p1110iiiiiiiiiiiiiiiiVVVV0xxx",1,0,0, {MNE_vmacl,MNE_vmacls}}, // Vd,Vn,Vm,imm16
  { "1111UUUUWWWW1111011p1110iiiiiiiiiiiiiiiiVVVV10xx",1,0,0, {MNE_vmaclus,MNE_vmaclss}}, // Vd,Vn,Vm,imm16
  { "1111UUUUWWWW1111011p1110iiiiiiiiiiiiiiiiVVVV11xx",1,0,0, {MNE_vmaclusc,MNE_vmaclssc}} // Vd,Vn,Vm,imm16
};

/* Suffix codes in imm format. Set to -1 to disable */
/*                                     imm_size: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 */
  static const int imm_suffix8_codes[17]    = { -1,-1,-1, 4,12,-1,60,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int imm_suffix16_codes[17]   = { -1,-1,-1, 5,13,-1,61,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int imm_suffix24_codes[17]   = { -1,-1,-1, 6,14,-1,62,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int imm_suffix32_codes[17]   = { -1,-1,-1, 7,15,-1,63,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };

/* Suffix codes in branch target. Set to -1 to disable */
/*                                     range_size: 0  1  2  3  4  5  6   7  8  9 10 11 12 13 14 15 16 */
  static const int branch_suffix8_codes[17] = { -1,-1, 0, 6,-1,30,-1,126,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int branch_suffix16_codes[17]= { -1,-1, 1, 7,-1,31,-1,127,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int branch_suffix24_codes[17]= { -1,-1, 2,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };
  static const int branch_suffix32_codes[17]= { -1,-1, 3,-1,-1,-1,-1, -1,-1,-1,-1,-1,-1,-1,-1,-1,-1 };

#define FIELD_MAX (16) /* max index in the above tables */

/****************************************************************************
 * idesc table interpretation
 */

typedef unsigned long long mask_t; /* TODO: Politically correct 64-bit type */

static __inline int lowbit(mask_t mask)
{
  int lowbit= 0;
  if (!mask)
    return -1;
    
  while (!(mask & 1))
  {
    mask >>= 1;
    lowbit++;
  }
  return lowbit;
}

static __inline int fieldsize(mask_t mask)
{
  int size= 0;
  while (mask)
  {
    if (mask & 1)
      size++;
    mask >>= 1;
  }
  return size;
}

static int idesc_get_syntax(const struct idesc *idesc)
{  
  if (!idesc->fmt || !strcmp(idesc->fmt,"")) return FMT_PLAIN;  
  else if (!strcmp(idesc->fmt,"[]")) return FMT_BRACKET;
  else if (!strcmp(idesc->fmt,"[sp]")) return FMT_BRACKET;
  else if (!strcmp(idesc->fmt,"[++]")) return FMT_BRACKET_PLUS_PLUS;
  else if (!strcmp(idesc->fmt,"-")) return FMT_LIST;
  else if (!strcmp(idesc->fmt,"v[]")) return FMT_INDEX;
  else if (!strcmp(idesc->fmt,"V[]")) return FMT_INDEX0;
  else if (!strcmp(idesc->fmt,"sp,")) return FMT_PLAIN;  
  else if (!strcmp(idesc->fmt,",sp")) return FMT_PLAIN;  
  else
  {    
    assert(0); /* unrecognized fmt */
  } 
  return FMT_PLAIN;  
}

/****************************************************************************/

#define FIELD_0   (0)
#define FIELD_1   (1)
#define FIELD_p   (2)
#define FIELD_a   (3)
#define FIELD_e   (4)
#define FIELD_b   (5)
#define FIELD_c   (6)
#define FIELD_d   (7)
#define FIELD_A   (8)
#define FIELD_E   (9)
#define FIELD_B   (10)
#define FIELD_C   (11)
#define FIELD_D   (12)
#define FIELD_q   (13)
#define FIELD_r   (14)
#define FIELD_s   (15)
#define FIELD_t   (16)
#define FIELD_i   (17)
#define FIELD_m   (18)
#define FIELD_j   (19)
#define FIELD_R   (20)
#define FIELD_U   (21)
#define FIELD_V   (22)
#define FIELD_W   (23)
#define FIELD_X   (24)
#define FIELD_x   (25)
#define FIELD_CNT (26)

#define mask_0 (field_masks[FIELD_0])
#define mask_1 (field_masks[FIELD_1])
#define p_mask (field_masks[FIELD_p])
#define a_mask (field_masks[FIELD_a])
#define e_mask (field_masks[FIELD_e])
#define b_mask (field_masks[FIELD_b])
#define c_mask (field_masks[FIELD_c])
#define d_mask (field_masks[FIELD_d])
#define A_mask (field_masks[FIELD_A])
#define E_mask (field_masks[FIELD_E])
#define B_mask (field_masks[FIELD_B])
#define C_mask (field_masks[FIELD_C])
#define D_mask (field_masks[FIELD_D])
#define q_mask (field_masks[FIELD_q])
#define r_mask (field_masks[FIELD_r])
#define s_mask (field_masks[FIELD_s])
#define t_mask (field_masks[FIELD_t])
#define i_mask (field_masks[FIELD_i])
#define m_mask (field_masks[FIELD_m])
#define j_mask (field_masks[FIELD_j])
#define R_mask (field_masks[FIELD_R])
#define U_mask (field_masks[FIELD_U])
#define V_mask (field_masks[FIELD_V])
#define W_mask (field_masks[FIELD_W])
#define X_mask (field_masks[FIELD_X])
#define x_mask (field_masks[FIELD_x])

/* Decode idesc into idesc_params */
static void decode_idesc(const struct idesc *idesc, struct idesc_params *res)
{
  const char *maskstr= idesc->maskstr;
  mask_t field_masks[FIELD_CNT]= {0};
  unsigned char field_lut[256];
  int i;
  struct idesc_imm_field m,R;

  for (i= 0; i<256; i++) field_lut[i]= FIELD_CNT;
  field_lut[(unsigned) (unsigned char) '0']= FIELD_0;
  field_lut[(unsigned) (unsigned char) '1']= FIELD_1;
  field_lut[(unsigned) (unsigned char) 'p']= FIELD_p;
  field_lut[(unsigned) (unsigned char) 'a']= FIELD_a;
  field_lut[(unsigned) (unsigned char) 'e']= FIELD_e;
  field_lut[(unsigned) (unsigned char) 'b']= FIELD_b;
  field_lut[(unsigned) (unsigned char) 'c']= FIELD_c;
  field_lut[(unsigned) (unsigned char) 'd']= FIELD_d;
  field_lut[(unsigned) (unsigned char) 'A']= FIELD_A;
  field_lut[(unsigned) (unsigned char) 'E']= FIELD_E;
  field_lut[(unsigned) (unsigned char) 'B']= FIELD_B;
  field_lut[(unsigned) (unsigned char) 'C']= FIELD_C;
  field_lut[(unsigned) (unsigned char) 'D']= FIELD_D;
  field_lut[(unsigned) (unsigned char) 'q']= FIELD_q;
  field_lut[(unsigned) (unsigned char) 'r']= FIELD_r;
  field_lut[(unsigned) (unsigned char) 's']= FIELD_s;
  field_lut[(unsigned) (unsigned char) 't']= FIELD_t;
  field_lut[(unsigned) (unsigned char) 'i']= FIELD_i;
  field_lut[(unsigned) (unsigned char) 'm']= FIELD_m;
  field_lut[(unsigned) (unsigned char) 'j']= FIELD_j;
  field_lut[(unsigned) (unsigned char) 'R']= FIELD_R;
  field_lut[(unsigned) (unsigned char) 'U']= FIELD_U;
  field_lut[(unsigned) (unsigned char) 'V']= FIELD_V;
  field_lut[(unsigned) (unsigned char) 'W']= FIELD_W;
  field_lut[(unsigned) (unsigned char) 'X']= FIELD_X;
  field_lut[(unsigned) (unsigned char) 'x']= FIELD_x;
    
  for (i= 0; maskstr[i]; i++)
  {
    int field= field_lut[(unsigned) (unsigned char) maskstr[i]];
    assert(field >= 0 && field < FIELD_CNT); /* valid character in maskstr */
    field_masks[field] |= ((mask_t) 1) << i;
  }

  // Check that we don't have multiple varieties of the same field
  assert((A_mask!=0)+(a_mask!=0)+(q_mask!=0)+(E_mask!=0)+(e_mask!=0)+(U_mask!=0)<=1);
  assert((B_mask!=0)+(b_mask!=0)+(r_mask!=0)+(V_mask!=0)<=1);
  assert((C_mask!=0)+(c_mask!=0)+(s_mask!=0)+(W_mask!=0)<=1);
  assert((D_mask!=0)+(d_mask!=0)+(t_mask!=0)+(X_mask!=0)<=1);
  assert((i_mask!=0)+(m_mask!=0)<=1);
  assert((j_mask!=0)+(R_mask!=0)<=1);

  res->ibytes= i>>3;
  res->ival= mask_1;
  res->imask= (mask_0 | mask_1);                         
  res->op_pos= lowbit(p_mask);
  res->op_size= fieldsize(p_mask);  

  res->rd.pos= lowbit(A_mask | E_mask | a_mask | e_mask | q_mask | U_mask);
  if (A_mask | E_mask)      res->rd.rfc= RFC_RREG;
  else if (a_mask | e_mask) res->rd.rfc= RFC_R0_TO_R14;
  else if (q_mask)          res->rd.rfc= RFC_R0_TO_R13;
  else if (U_mask)          res->rd.rfc= RFC_VREG;
  else                      res->rd.rfc= RFC_NONE;

  res->rn.pos= lowbit(B_mask | b_mask | r_mask | V_mask);
  if (B_mask)      res->rn.rfc= RFC_RREG;
  else if (b_mask) res->rn.rfc= RFC_R0_TO_R14;
  else if (r_mask) res->rn.rfc= RFC_R0_TO_R13;
  else if (V_mask) res->rn.rfc= RFC_VREG;
  else             res->rn.rfc= RFC_NONE;

  res->rm.pos= lowbit(C_mask | c_mask | s_mask | W_mask);
  if (C_mask)      res->rm.rfc= RFC_RREG;
  else if (c_mask) res->rm.rfc= RFC_R0_TO_R14;
  else if (s_mask) res->rm.rfc= RFC_R0_TO_R13;
  else if (W_mask) res->rm.rfc= RFC_VREG;
  else             res->rm.rfc= RFC_NONE;

  res->ro.pos= lowbit(D_mask | d_mask | t_mask | X_mask);
  if (D_mask)      res->ro.rfc= RFC_RREG;
  else if (d_mask) res->ro.rfc= RFC_R0_TO_R14;
  else if (t_mask) res->ro.rfc= RFC_R0_TO_R13;
  else if (X_mask) res->ro.rfc= RFC_VREG;
  else             res->ro.rfc= RFC_NONE;

  res->rd_carries_opcode= (E_mask != 0 || e_mask != 0);

  m.pos= lowbit(i_mask | m_mask);
  m.size= fieldsize(i_mask | m_mask);
  m.pcrel= 0;
  m.factor= idesc->factor;  /* implicit factor applied to bit field */
  m.bias= 0;                /* implicit term applied to bit field   */
  m.suffixable= (m_mask != 0); /* suffixable if imm_suffix tables say so */

  R.pos= lowbit(j_mask | R_mask);
  R.size= fieldsize(j_mask | R_mask);
  if (j_mask)
  {
    R.pcrel= 0;
    R.factor= 1;
    R.bias= 0;
    R.suffixable= 0;
  }
  else
  {
    R.pcrel= 1;
    R.factor= 1;                     /* implicit factor applied to bit field */
    R.bias= idesc->bias;             /* implicit term applied to bit field   */
    R.suffixable= 1;                 /* suffixable if branch_suffix tables say so */
  }

  /* Map idesc m/R fields to imm[0] and imm[1] so they can be conviniently 
     matched to RHS while assembling. Range is assumed to appear last in
     the syntax of the instruction */
  if (i_mask || m_mask)
  {
    res->imm[0]= m;
    res->imm[1]= R;
  }
  else
  {
    res->imm[0]= R;
    res->imm[1]= m; /* will be no operand */
  }

  /* Total opcode is made up of instruction opcode field + optional Rd>Rn implied bit */ 
  res->op_cnt= 1<< (res->op_size + res->rd_carries_opcode);
  res->mnes= idesc->mnes; /* keep a pointer to the table holding mnemonic for each opcode */

  /* Detect implied sp as Rd in instruction */
  if (idesc->fmt && !strcmp(idesc->fmt,"sp,"))
    res->rd.rfc= RFC_IMPLIED_R15;
  
  /* Detect implied sp as Rn in instruction */
  if (idesc->fmt && (!strcmp(idesc->fmt,"[sp]") || !strcmp(idesc->fmt,",sp")))
    res->rn.rfc= RFC_IMPLIED_R15;
  
  /* Check the syntax of the instruction */
  res->format= idesc_get_syntax(idesc);
}

/****************************************************************************/
/* idesc_params table (iptable) */
/* Table holding an idesc_params for each idesc */
static struct idesc_params *g_iptable= 0;

/* Get the decoded idesc_params table. Return no. of entries in ipcnt */
const struct idesc_params *isa_get_iptable(int *ipcnt)
{
  const int cnt= sizeof(idescs)/sizeof(*idescs);
  
  /* Build the g_iptable table, if not already done */
  if (!g_iptable)
  {
    int i;
    g_iptable= (struct idesc_params *) malloc(sizeof(struct idesc_params)*cnt);
    assert(g_iptable);
    for (i= 0; i<cnt; i++)
      decode_idesc(&idescs[i], &g_iptable[i]);
  }

  *ipcnt= cnt;
  return g_iptable;
}

/****************************************************************************/

/* Get the first non-pcrel field */
/* (Kind of a kludge) */
const struct idesc_imm_field *get_non_pcrel_field(const struct idesc_params *iparams)
{
  int i;
  for (i= 0; i<RASC_IMMS; i++)  
    if (iparams->imm[i].pos >= 0 && iparams->imm[i].pcrel == 0)
      return &iparams->imm[i];
  return 0;
}

/* Get the first pcrel field */
/* (Kind of a kludge) */
const struct idesc_imm_field *get_pcrel_field(const struct idesc_params *iparams)
{
  int i;
  for (i= 0; i<RASC_IMMS; i++)  
    if (iparams->imm[i].pos >= 0 && iparams->imm[i].pcrel == 1)
      return &iparams->imm[i];

  /* The kludge is getting worse:
     if we have two immediate fields, we return the second one here even if it
     isn't flagged as PC-relative. We do this to convey the destination-field of
     a VRASC skip instruction to the disassembler, since these are not flagged as
     PC relative for now. */
  if (iparams->imm[0].pos >= 0 &&
      iparams->imm[1].pos >= 0)
    return &iparams->imm[1];

  return 0;
}

/****************************************************************************/
/* Immediate field suffixes */

/* Does the code correspond to a suffix? */
/* In that case return the no. of suffix bytes, otherwise 0 */
int isa_get_imm_suffix_bytes(const struct idesc_imm_field *imm, int code)
{      
  int field_size= imm->size;

  if (code == -1)
    return 0; /* magic no-suffix code: cannot be used for suffixes */

  if (!imm->suffixable)
    return 0; /* particular type of does not use suffixes */

  assert(field_size>0 && field_size<=FIELD_MAX);                
  if      (code==imm_suffix8_codes[field_size] ) return 1;
  else if (code==imm_suffix16_codes[field_size]) return 2;
  else if (code==imm_suffix24_codes[field_size]) return 3;
  else if (code==imm_suffix32_codes[field_size]) return 4;
  return 0;
}

/* Get a specific imm suffix code. Return -1 if there is none. */
int isa_get_imm_suffix_code(int field_size, int bytes)
{
  assert(field_size>0 && field_size<=FIELD_MAX);                
  if      (bytes==1) return imm_suffix8_codes[field_size];
  else if (bytes==2) return imm_suffix16_codes[field_size];
  else if (bytes==3) return imm_suffix24_codes[field_size];
  else if (bytes==4) return imm_suffix32_codes[field_size];  
  assert(0);
  return -1;
}

/* Check if value fits without immediate prefix or suffix */
int isa_imm_fits_value(const struct idesc_imm_field *imm, int val)
{
  int code= val - imm->bias; /* implicit term added to bit field */
  if (code % imm->factor)
    return 0;        /* odd value in factor field? */
  code /= imm->factor;

  if (code<0 || code >= (1<<imm->size))
    return 0;
  
  if (!imm->suffixable)
    return 1; /* Not suffixable => no need to clash check with suffix codes */
   
  /* Check for clash with suffix code */
  assert(imm->size>0 && imm->size<=FIELD_MAX);
  if (code==imm_suffix8_codes [imm->size] ||
      code==imm_suffix16_codes[imm->size] ||
      code==imm_suffix24_codes[imm->size] ||
      code==imm_suffix32_codes[imm->size] ) return 0; /* clash */      
  else return 1; /* fits! */
}

/* Return the length of a suffix mode length that fits an unsigned constant of 'bytes' size */
/* Write the code used for that into *suffix_code */
/* Return 0 if there is no such mode */
static int isa_get_smallest_imm_suffix_len(const struct idesc_imm_field *imm, int bytes, int *suffix_code)
{
  assert(imm->size>0 && imm->size<=FIELD_MAX); 
  {  
    int codes[4]= { 
      imm_suffix8_codes [imm->size],
      imm_suffix16_codes[imm->size],
      imm_suffix24_codes[imm->size],
      imm_suffix32_codes[imm->size]   
    };
    int sizes[4]= { 1, 2, 3, 4 };
    int t;
    for (t= 0; t<4; t++)
      if (codes[t] >= 0 && bytes <= sizes[t])
      {
        *suffix_code= codes[t];
        return sizes[t];
      }
  }
  return 0; /* no appropriate suffix mode available */  
}

/* Return the length of a prefix mode that fits an unsigned constant of 'bytes' size */
static int isa_get_smallest_imm_prefix_len(const struct idesc_imm_field *imm, int bytes)
{
  if (imm->pcrel == 0)
    return 0; /* imm prefix not supported for pcrel fields */

  /* No suffix support? assume there is prefix support in that case */
  if (imm_suffix8_codes [4] == -1 &&
      imm_suffix16_codes[4] == -1 &&
      imm_suffix24_codes[4] == -1 &&
      imm_suffix32_codes[4] == -1)
  {
    if (bytes<=2)
      return 2;
  }    
  return 0; /* no appropriate mode */ 
}

/* Return the length of an appendix mode that fits an unsigned constant of 'bytes' size */
/* Also return whether a suffix (rather than prefix) was chosen, and in that case its code */
int isa_select_appendix(const struct idesc_imm_field *imm, int bytes, int *is_suffix, int *suffix_code)
{
  int len= isa_get_smallest_imm_suffix_len(imm, bytes, /*out*/ suffix_code);
  
  if (len)
  {
    *is_suffix= 1; /* suffix rather than prefix */
    return len;
  }    
  
  /* No suffix mode present.  Try immediate prefix. */    
  len= isa_get_smallest_imm_prefix_len(imm, bytes);
  if (len)
  {
    *is_suffix= 0; /* prefix rather than suffix */
    return len;
  }    
  return 0;
}

/****************************************************************************/
/* Branch target field suffixes */

/* Does the code correspond to a suffix? */
/* In that case return the no. of suffix bytes, otherwise 0 */
int isa_get_branch_suffix_bytes(int field_size, int code)
{
  if (code == -1)
    return 0; 

  assert(field_size>0 && field_size<=FIELD_MAX);                
  if      (code==branch_suffix8_codes[field_size] ) return 1;
  else if (code==branch_suffix16_codes[field_size]) return 2;
  else if (code==branch_suffix24_codes[field_size]) return 3;
  else if (code==branch_suffix32_codes[field_size]) return 4;
  return 0;
}
      
/* Get a specific branch suffix code. Return -1 if there is none. */
int isa_get_branch_suffix_code(int field_size, int bytes)
{
  assert(field_size>0 && field_size<=FIELD_MAX);                
  if      (bytes==1) return branch_suffix8_codes[field_size];
  else if (bytes==2) return branch_suffix16_codes[field_size];
  else if (bytes==3) return branch_suffix24_codes[field_size];
  else if (bytes==4) return branch_suffix32_codes[field_size];  
  assert(0);
  return -1;
}

/* Return 1 if BCC3 branches have instruction-counting semantics */
int isa_bcc3_icnt_semantics(void)
{
  return 1;
}

/* See if branch suffixes are relative to the _end_ of the suffix rather than
   the _start_ */
int isa_is_branch_suffix_rel_to_end(void)
{
  return 0;
}

/******************************************************************************/
/* Other ISA parameters */

/* No. of bytes instruction granularity */
int isa_get_bytes_per_unit(void)
{
  return 1;
}

/******************************************************************************/
/* Decoding functions */

/* Extract bit field or return default_val if mask is 0 or pos<0 */
static __inline int extract_bitfield(unsigned instr, int pos, unsigned size, int default_val)
{
  if (size>0 && pos>=0)
    return (instr >> pos) & ((1<<size)-1);
  return default_val;
}

/* Find idesc entry that corresonds to instr  */ 
/* Return pointer to idesc_params or NULL on failure. */
const struct idesc_params *isa_find_iparams(unsigned long long instr)
{
  int cnt, i;
#ifdef DEBUG_IDESC_PARAMS
  int match = -1;
#endif
  static int *iptable_order = NULL;
  const struct idesc_params *iptable= isa_get_iptable(&cnt);
  const struct idesc_params *found_iparams= NULL;

  if (iptable_order == NULL)
    {
      iptable_order = (int*)malloc(cnt * sizeof(int));
      for (i = 0; i < cnt; i++)
	iptable_order[i] = i;
    }

  /* Match instruction against idesc_params table */
  for (i= 0; i<cnt; i++)
  {
    const struct idesc_params *params= &iptable[iptable_order[i]];
    if ((instr & params->imask) == params->ival)
    {
      int rd, rn, rm, ro;

      rd = extract_bitfield(instr, params->rd.pos, REG_BITS, -1);
      if (params->rd.rfc == RFC_R0_TO_R14 && rd>14)
	continue; /* Rd can not be r15 */
      if (params->rd.rfc == RFC_R0_TO_R13 && rd>13)
	continue; /* Rd can not be r14 or r15 */

      rn = extract_bitfield(instr, params->rn.pos, REG_BITS, -1);
      if (params->rn.rfc == RFC_R0_TO_R14 && rn>14)
	continue; /* Rn can not be r15 */
      if (params->rn.rfc == RFC_R0_TO_R13 && rn>13)
	continue; /* Rn can not be r14 or r15 */

      rm = extract_bitfield(instr, params->rm.pos, REG_BITS, -1);
      if (params->rm.rfc == RFC_R0_TO_R14 && rm>14)
	continue; /* Rm can not be r15 */
      if (params->rm.rfc == RFC_R0_TO_R13 && rm>13)
	continue; /* Rm can not be r14 or r15 */

      ro = extract_bitfield(instr, params->ro.pos, REG_BITS, -1);
      if (params->ro.rfc == RFC_R0_TO_R14 && ro>14)
	continue; /* Ro can not be r15 */
      if (params->ro.rfc == RFC_R0_TO_R13 && ro>13)
	continue; /* Ro can not be r14 or r15 */

#ifndef DEBUG_IDESC_PARAMS
      /* Move the found instuction earlier into the search order, so that
       * common instructions will bubble up to be searched before non-common
       * instructions. */
      if (i > 0)
	{
	  int tmp = iptable_order[i - 1];
	  iptable_order[i - 1] = iptable_order[i];
	  iptable_order[i] = tmp;
	}
      return params;
#else
      found_iparams= params;
      if (match!=-1)
      {
        fprintf(stderr,"Instruction description ambiguous for %04llx\n",instr);
        fprintf(stderr,"Matches both %d and %d\n",
		iptable[match].mnes[0],iptable[i].mnes[0]);
        exit(1);
      }
      match= i;
#endif
    }
  }
  return found_iparams;
}
