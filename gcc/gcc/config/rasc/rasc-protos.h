/* Prototypes for exported functions defined in rasc.c  */
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

#ifndef __RASC_PROTOS_H__
#define __RASC_PROTOS_H__

/* Functions within rasc.c that we reference.  */
#ifdef RTX_CODE
extern void rasc_split_operand (rtx, rtx *, enum machine_mode);
const char* vrasc_get_op(int);
extern char *rasc_emit_call (int, rtx *, int);
extern char *rasc_emit_builtin1 (rtx *);
extern char *rasc_emit_builtin1s (rtx *);
extern char *rasc_emit_builtin2 (rtx *);
extern char *rasc_emit_builtin3 (rtx *);
extern char *rasc_emit_builtin4 (rtx *);
extern char *rasc_emit_builtin_ld (rtx *);
extern char *rasc_emit_builtin_st (rtx *);
extern char *rasc_emit_builtin_vrasc_skip (rtx *);
extern char *rasc_emit_builtin_vrasc_generic2 (rtx *);
extern char *rasc_emit_builtin_vrasc_generic3 (rtx *);

void rasc_expand_vector_init (rtx target, rtx vals);
int rasc_expand_block_move (rtx *operands);
int rasc_expand_block_set (rtx *operands);

#ifdef TREE_CODE
extern void rasc_init_cumulative_args (CUMULATIVE_ARGS *);
#endif /* TREE_CODE */

extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern rtx rasc_return_addr_rtx (int, rtx);
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void rasc_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
				       tree, int);
extern rtx rasc_function_arg (CUMULATIVE_ARGS *, enum machine_mode,
			      tree, int);
#endif /* TREE_CODE */

extern void override_options (void);
extern int rasc_return_allowed (void);
extern char *rasc_emit_return_internal (void);
extern void rasc_expand_epilogue (bool);
extern void rasc_expand_prologue (void);
extern int rasc_initial_elimination_offset (int from, int to);

#endif /* !__RASC_PROTOS_H__ */
