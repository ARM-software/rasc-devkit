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

/* This file holds definitions specific to the RASC ELF ABI. */
#ifndef _ELF_RASC_H
#define _ELF_RASC_H

#include "elf/reloc-macros.h"

/* Relocations.  */
START_RELOC_NUMBERS (elf_rasc_reloc_type)
  RELOC_NUMBER (R_RASC_NONE, 0)               /* No reloc */
  RELOC_NUMBER (R_RASC_32, 1)                 /* Absolulte 32-bit reloc */
  RELOC_NUMBER (R_RASC_24, 2)                 /* Absolulte 24-bit reloc */
  RELOC_NUMBER (R_RASC_16, 3)                 /* Absolulte 16-bit reloc */
  RELOC_NUMBER (R_RASC_8, 4)                  /* Absolulte 8-bit reloc */
  RELOC_NUMBER (R_RASC_32_PCREL, 5)           /* PC-relative 32-bit reloc, 8-bit granular */
  RELOC_NUMBER (R_RASC_24_PCREL, 6)           /* PC-relative 24-bit reloc, 8-bit granular */
  RELOC_NUMBER (R_RASC_16_PCREL, 7)           /* PC-relative 16-bit reloc, 8-bit granular */
  RELOC_NUMBER (R_RASC_8_PCREL, 8)            /* PC-relative 8-bit reloc, 8-bit granular */
  RELOC_NUMBER (R_RASC_IMM16_HIGH12, 9)       /* Immediate prefix instruction */
  RELOC_NUMBER (R_RASC_IMM16_LOW3_POS5,10)    /* Low 3 bits of 16-bit reloc */
  RELOC_NUMBER (R_RASC_IMM16_LOW4_POS4,11)    /* Low 4 bits of 16-bit reloc */
  RELOC_NUMBER (R_RASC_IMM16_LOW4_POS8,12)    /* Low 4 bits of 16-bit reloc */
  RELOC_NUMBER (R_RASC_IMM16_LOW6_POS6,13)    /* Low 6 bits of 16-bit reloc */
  RELOC_NUMBER (R_RASC_IMM16BY2_LOW3_POS5,14) /* Low 3 bits of 16-bit 2-shifted reloc */
  RELOC_NUMBER (R_RASC_IMM16BY2_LOW4_POS4,15) /* Low 4 bits of 16-bit 2-shifted reloc */
  RELOC_NUMBER (R_RASC_IMM16BY4_LOW4_POS4,16) /* Low 4 bits of 16-bit 4-shifted reloc */
  RELOC_NUMBER (R_RASC_IMM16BY4_LOW6_POS6,17) /* Low 6 bits of 16-bit 4-shifted reloc */
  RELOC_NUMBER (R_RASC_PC11BY1, 18)           /* 11-bit PC relative field, 8-bit granular */
  RELOC_NUMBER (R_RASC_PC9BY1, 19)            /* 9-bit PC relative field, 8-bit granular */
  RELOC_NUMBER (R_RASC_PC7BY1, 20)            /* 7-bit PC relative field, 8-bit granular */
  RELOC_NUMBER (R_RASC_PC5BY1, 21)            /* 5-bit PC relative field, 8-bit granular */
  RELOC_NUMBER (R_RASC_PC3BY1, 22)            /* 3-bit PC relative field, 8-bit granular */
  RELOC_NUMBER (R_RASC_PC11BY2, 23)           /* 11-bit PC relative field, 16-bit granular */
  RELOC_NUMBER (R_RASC_PC9BY2, 24)            /* 9-bit PC relative field, 16-bit granular */
  RELOC_NUMBER (R_RASC_PC7BY2, 25)            /* 7-bit PC relative field, 16-bit granular */
  RELOC_NUMBER (R_RASC_PC5BY2, 26)            /* 5-bit PC relative field, 16-bit granular */
  RELOC_NUMBER (R_RASC_PC3BY2, 27)            /* 3-bit PC relative field, 16-bit granular */
  RELOC_NUMBER (R_RASC_LPC_24, 28)            /* Relaxable lpc, 24-bits displacement */
  RELOC_NUMBER (R_RASC_LPC_16, 29)            /* Relaxable lpc, 16-bits displacement */
  RELOC_NUMBER (R_RASC_BR_BSR_24, 30)         /* Relaxable br/bsr, 24-bits displacement */
  RELOC_NUMBER (R_RASC_BR_BSR_16, 31)         /* Relaxable br/bsr, 16-bits displacement */
  RELOC_NUMBER (R_RASC_GNU_VTINHERIT, 32)     /* For C++ vtable garbage collection */
  RELOC_NUMBER (R_RASC_GNU_VTENTRY, 33)       /* For C++ vtable garbage collection */
  RELOC_NUMBER (R_RASC_RELATIVE, 34)          /* Adjust by program base */  
  RELOC_NUMBER (R_RASC_COPY, 35)              /* Copy symbol at runtime */ 
  RELOC_NUMBER (R_RASC_GLOB_DAT, 36)          /* Create GOT entry */ 
  RELOC_NUMBER (R_RASC_JUMP_SLOT, 37)         /* Create PLT entry */  
END_RELOC_NUMBERS (R_RASC_max)

/* Section Attributes.  */
#define SHF_RASC_NOREAD	0x80000000

#endif /* _ELF_RASC_H */
