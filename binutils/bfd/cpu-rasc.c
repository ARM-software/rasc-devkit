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

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

const bfd_arch_info_type bfd_rasc_arch =
{
  32,		  		/* 32 bits in a word */
  32,		  		/* 32 bits in an address */
  8,		  		/* 8 bits in a byte */
  bfd_arch_rasc, 		/* Architecture */
  0,		  		/* Machine number - 0 for now */
  "RASC",	  		/* Architecture name */
  "RASC",	  		/* Printable name */
  3,		  		/* Section align power */
  TRUE,		  		/* Is this the default architecture ? */
  bfd_default_compatible,	/* Architecture comparison function */
  bfd_default_scan,	   	/* String to architecture conversion */
  NULL			   	/* Next in list */
};
