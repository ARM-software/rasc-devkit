/* Memory statistics helpers.
   Copyright (C) 2004, 2007
   Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_STATISTICS
#define GCC_STATISTICS
#ifdef GATHER_STATISTICS
#define MEM_STAT_DECL , const char * ARG_UNUSED (_loc_name), int ARG_UNUSED (_loc_line), const char * ARG_UNUSED (_loc_function)
#define PASS_MEM_STAT , _loc_name, _loc_line,  _loc_function
#define MEM_STAT_INFO , __FILE__, __LINE__, __FUNCTION__
#else
#define MEM_STAT_DECL
#define PASS_MEM_STAT
#define MEM_STAT_INFO
#endif
#endif
