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

#define NO_IMPLICIT_EXTERN_C

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef TARGET_VERSION
#define TARGET_VERSION fputs (" (rasc/ELF)", stderr);

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#undef ASM_SPEC
#define ASM_SPEC \
 "%{v}"

#if 0  /* Disabled untile the bugs have been fixed... */
#undef LINK_SPEC
#define LINK_SPEC "%{!mno-relax:%{!r:--relax}}"
#endif

#undef LIB_SPEC
#define LIB_SPEC "-lc -lsim -lc"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "crt0%O%s crti%O%s crtbegin%O%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"  

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"."
