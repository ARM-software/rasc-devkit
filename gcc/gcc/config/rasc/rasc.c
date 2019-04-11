/* Output routines for RASC processor.  */
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "recog.h"
#include "output.h"
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "function.h"
#include "toplev.h"
#include "optabs.h"
#include "libfuncs.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "tree-gimple.h"
#include "integrate.h"

struct rasc_stack_layout
{
  int arg_size;                 /* Stdarg spills (bytes).  */
  int reg_size;                 /* Non-volatile reg saves (bytes).  */
  int local_size;               /* Locals.  */
  int outbound_size;            /* Arg overflow on calls out.  */
  int first_reg;                /* First register to push */
};

/* Define the structure for the machine field in struct function.  */
struct machine_function GTY(())
{
  int lr_needed;
  int n_varargs;
  int popr_regs;                /* Regs to popr, or sp if rts.  */
  int return_allowed;
};

/* Map hard register number to register class */
const enum reg_class rasc_regno_to_class[FIRST_PSEUDO_REGISTER] =
{
  GR_REGS,      GR_REGS,        GR_REGS,        GR_REGS,
  GR_REGS,      GR_REGS,        GR_REGS,        GR_REGS,
  GR_REGS,      GR_REGS,        GR_REGS,        GR_REGS,
  GR_REGS,      GR_REGS,        GR_REGS,        SP_REGS,
  AR_REGS
};

static struct machine_function * rasc_init_machine_status (void);
static void printx (FILE *, signed int);
static section *rasc_select_rtx_section (enum machine_mode, rtx,
                                         unsigned HOST_WIDE_INT);
static bool rasc_rtx_costs (rtx, int, int, int *);
static int rasc_address_cost (rtx x);
static bool rasc_return_in_memory (tree, tree);

static bool rasc_function_ok_for_sibcall (tree, tree);
static void rasc_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode,
                                         tree, int *, int);
static int rasc_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                                   tree type, bool named);
static unsigned HOST_WIDE_INT rasc_shift_truncation_mask (enum machine_mode);
static unsigned int rasc_section_type_flags (tree, const char*, int);


/* These hooks specify assembly directives for creating certain kinds
   of integer object.  */

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"

#undef  TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION  rasc_select_rtx_section

#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS rasc_section_type_flags

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS rasc_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST rasc_address_cost

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK rasc_shift_truncation_mask

#undef  TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS hook_bool_tree_true
#undef  TARGET_PROMOTE_FUNCTION_RETURN
#define TARGET_PROMOTE_FUNCTION_RETURN hook_bool_tree_true
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_tree_true

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY rasc_return_in_memory

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS rasc_setup_incoming_varargs

#undef  TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG hook_bool_tree_true
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES rasc_arg_partial_bytes

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET 0
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 65535

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL rasc_function_ok_for_sibcall

struct gcc_target targetm = TARGET_INITIALIZER;

/* This is just like the standard true_regnum() function except that it
   works even when reg_renumber is not initialized.  */

static int
xt_true_regnum (rtx x)
{
  if (GET_CODE (x) == REG)
    {
      if (reg_renumber
          && REGNO (x) >= FIRST_PSEUDO_REGISTER
          && reg_renumber[REGNO (x)] >= 0)
        return reg_renumber[REGNO (x)];
      return REGNO (x);
    }
  if (GET_CODE (x) == SUBREG)
    {
      int base = xt_true_regnum (SUBREG_REG (x));
      if (base >= 0 && base < FIRST_PSEUDO_REGISTER)
        return base + subreg_regno_offset (REGNO (SUBREG_REG (x)),
                                           GET_MODE (SUBREG_REG (x)),
                                           SUBREG_BYTE (x), GET_MODE (x));
    }
  return -1;
}


/* Split SRC_OP into DST_OP[0,1].  MODE is for the output, 
   i.e., the input operand is twice as big as MODE.  */

void
rasc_split_operand (rtx src_op, rtx dst_op[2], enum machine_mode mode)
{
  switch (GET_CODE (src_op))
    {
    case REG:
      dst_op[1] = gen_rtx_REG (mode, REGNO (src_op) + 1);
      dst_op[0] = gen_rtx_REG (mode, REGNO (src_op));
      break;

    case MEM:
      dst_op[1] = adjust_address (src_op, mode, GET_MODE_SIZE (mode));
      dst_op[0] = adjust_address (src_op, mode, 0);
      break;

    case CONST_INT:
    case CONST_DOUBLE:
      split_double (src_op, &dst_op[0], &dst_op[1]);
      break;

    default:
      gcc_unreachable ();
    }
}

static struct machine_function *
rasc_init_machine_status (void)
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

char *
rasc_emit_call (int callop, rtx *operands, int is_noreturn)
{
  static char result[64];
  rtx tgt = operands[callop];

  if (GET_CODE (tgt) == CONST_INT)
    sprintf (result, "%s\t0x%lx", is_noreturn ? "jmp": "jsr", INTVAL (tgt));
  else if (register_operand (tgt, VOIDmode))
    sprintf (result, "%s\t%%%d", is_noreturn ? "jmp": "jsr", callop);
  else
    sprintf (result, "%s\t#%%%d", is_noreturn ? "br": "bsr", callop);

  return result;
}


/* Argument support functions.  */

/* Initialize CUMULATIVE_ARGS for a function.  */

void
rasc_init_cumulative_args (CUMULATIVE_ARGS *cum)
{
  cum->reg_count = 0;
}

static int
rasc_function_arg_size( enum machine_mode mode, tree type)
{
  int size;
  if (mode != BLKmode)
    size = GET_MODE_SIZE (mode);
  else 
    size = int_size_in_bytes (type);
  return (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* Return an RTL expression containing the register for the given mode,
   or 0 if the argument is to be passed on the stack.   */

rtx
rasc_function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode, tree type,
                   int named ATTRIBUTE_UNUSED)
{
  if (mode == VOIDmode || targetm.calls.must_pass_in_stack (mode, type))
    return (rtx)0;
  
  if (cum->reg_count >= MAX_ARGS_IN_REGISTERS)
    return (rtx)0;

  return gen_rtx_REG (mode, cum->reg_count);
}

/* Advance the argument to the next argument position.  */

void
rasc_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                           tree type, int named ATTRIBUTE_UNUSED)
{
  if (mode == VOIDmode || targetm.calls.must_pass_in_stack (mode, type))
    cum->reg_count = MAX_ARGS_IN_REGISTERS;
  else
    cum->reg_count += rasc_function_arg_size(mode,type);
}

/* Returns the number of bytes of argument registers required to hold *part*
   of a parameter of machine mode MODE and type TYPE (which may be NULL if
   the type is not known).  If the argument fits entirely in the argument
   registers, or entirely on the stack, then 0 is returned.  CUM is the
   number of argument registers already used by earlier parameters to
   the function.  */

static int
rasc_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                        tree type, bool named ATTRIBUTE_UNUSED)
{
  if (targetm.calls.must_pass_in_stack (mode, type))
    return 0;
      
  if (cum->reg_count >= MAX_ARGS_IN_REGISTERS)
    return 0;

  /* If the argument fits entirely in registers, return 0.  */
  if (cum->reg_count + rasc_function_arg_size(mode,type) <= MAX_ARGS_IN_REGISTERS)
    return 0;

  /* The argument overflows the number of available argument registers.
     Compute how many argument registers have not yet been assigned to
     hold an argument.  */
  /* Return partially in registers and partially on the stack.  */
  return (MAX_ARGS_IN_REGISTERS-cum->reg_count) * UNITS_PER_WORD;
}

void
override_options (void)
{
  init_machine_status = rasc_init_machine_status;

  if (flag_pic)
    {
      warning (0, "-f%s not supported: ignored",
               (flag_pic > 1) ? "PIC" : "pic");
      flag_pic = 0;
    }
}


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the '%' specification that was used to request
   printing of the operand.  If the specification was just '%DIGIT'
   then CODE is 0; if the specification was '%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array 'reg_names' whose type is
   'char *[]'.  'reg_names' is initialized from 'REGISTER_NAMES'.

   When the machine description has a specification '%PUNCT' (a '%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   'a', 'c', 'l', and 'n' are reserved.

   The RASC specific codes are:

   'd'  CONST_INT, print as signed decimal
   'x'  CONST_INT, print as signed hexadecimal
   'H'  Most significant word of a double
   'L'  Least significant word of a double
   'n'  CONST_INT, print the negated value as signed decimal
   'r'  Interpret const_int as register number (push/pop)
   
*/

static void
printx (FILE *file, signed int val)
{
  /* Print a hexadecimal value in a nice way.  */
  if ((val > -0xa) && (val < 0xa))
    fprintf (file, "%d", val);
  else if (val < 0)
    fprintf (file, "-0x%x", -val);
  else
    fprintf (file, "0x%x", val);
}


void
print_operand (FILE *file, rtx x, int letter)
{
  if (!x)
    error ("PRINT_OPERAND null pointer");

  switch (letter)
    {
    case 'H' :
    case 'L' :
      if (GET_CODE (x) == REG)
        {
          /* L = least significant word, H = most significant word */
          if (letter == 'L')
            fputs (reg_names[REGNO (x)], file);
          else
            fputs (reg_names[REGNO (x)+1], file);
        }
      else if (GET_CODE (x) == CONST_INT
               || GET_CODE (x) == CONST_DOUBLE)
        {
          rtx first, second;
          split_double (x, &first, &second);
          fprintf (file, "0x%08lx",
                   (long)(letter == 'L' ? INTVAL (first) : INTVAL (second)));
        }
      else
        output_operand_lossage ("invalid operand to %%H/%%L code");
      return;

    case 'x':
      if (GET_CODE (x) == CONST_INT)
        printx (file, INTVAL (x));
      else
        output_operand_lossage ("invalid %%x value");
      return;

    case 'd':
      if (GET_CODE (x) == CONST_INT)
        fprintf (file, "%ld", INTVAL (x));
      else
        output_operand_lossage ("invalid %%d value");
      return;

    case 'n':
      if (GET_CODE (x) == CONST_INT)
        fprintf (file, "%ld", -INTVAL (x));
      else
        output_operand_lossage ("invalid %%n value");
      return;

    case 'N':
      if (GET_CODE (x) == CONST_INT)
        fprintf (file, "%ld", ~INTVAL (x));
      else
        output_operand_lossage ("invalid %%n value");
      return;

    case 'r':
      if (GET_CODE (x) == CONST_INT && INTVAL (x)<=15)
        fprintf (file, "%s", reg_names[INTVAL(x)]);
      else
        output_operand_lossage ("invalid %%r value");
      return;

    case 0 :
      /* Digit only, the normal case.  */
      break;

    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
    }

    if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
      fprintf (file, "%s", reg_names[xt_true_regnum (x)]);
    else if (GET_CODE (x) == MEM)
      output_address (XEXP (x, 0));
    else if (GET_CODE (x) == CONST_INT)
      fprintf (file, "%ld", INTVAL (x));
    else if ( GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
      {
        REAL_VALUE_TYPE d;
        long l;
        REAL_VALUE_FROM_CONST_DOUBLE (d, x);
        REAL_VALUE_TO_TARGET_SINGLE (d, l);
        fprintf (file, "0x%08lx", l);
      }
    else
      output_addr_const (file, x);
}


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.  */

void
print_operand_address (FILE *file, rtx addr)
{
  if (!addr)
    error ("PRINT_OPERAND_ADDRESS, null pointer");

  switch (GET_CODE (addr))
    {
    default:
      fatal_insn ("invalid address", addr);
      break;

    case REG:
      fprintf (file, "%s", reg_names [REGNO (addr)]);
      break;

    case PLUS:
      {
        rtx reg;
        rtx offset;
        rtx arg0 = XEXP (addr, 0);
        rtx arg1 = XEXP (addr, 1);

        if (GET_CODE (arg0) == REG)
          {
            reg = arg0;
            offset = arg1;
          }
        else if (GET_CODE (arg1) == REG)
          {
            reg = arg1;
            offset = arg0;
          }
        else
          fatal_insn ("no register in address", addr);

        if (CONSTANT_P (offset))
          {
            fprintf (file, "%s, #", reg_names [REGNO (reg)]);
            output_addr_const (file, offset);
          }
        else
          fatal_insn ("address offset not a constant", addr);
      }
      break;

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
      output_addr_const (file, addr);
      break;
    }
}


/* RASC stack frame:
         incoming arguments         (pushed by caller)
         varargs {r0-r4}            (pushed by callee in case of ...-function)
         save regs {r5-r14}         (register variables used in the function)
   fp -> local variables            
         dynamic local variables    (size not known at compile time)
   sp -> outgoing args              (size defined by called function with most
                                     arguments)  */
static void
rasc_get_stack_layout (struct rasc_stack_layout *layout)
{
  int frame_size;
  
  /* Initially this is the size of the local variables.  It will translated
     into an offset once we have determined the size of preceding data.  */
  frame_size = (get_frame_size() + 3) & ~3;

  /* Calculate first_reg to push, based on register usage.  */
  {
    int reg, first_reg = SP_REGNUM;
    int sc_reg = cfun->static_chain_decl ? STATIC_CHAIN_REGNUM : -1;

    /* Save all general registers used (but only if we are not allowed
       to clobber them).  The RASC push/pop instructions support a
       range rn-r14.  */
    for (reg = 0; reg < LR_REGNUM; reg++)
      if (regs_ever_live[reg] && ! call_used_regs[reg] && reg != sc_reg)
        {
            first_reg = reg;
            break;
        }

    /* If LR was used, then we should save it.  */
    if (regs_ever_live[LR_REGNUM] && first_reg > LR_REGNUM)
      first_reg = LR_REGNUM;

    /* If FP was used, then we should save it.  */
    if (frame_pointer_needed && first_reg > FP_REGNUM)
      first_reg = FP_REGNUM;

    layout->first_reg = first_reg;
  }

  /* Fill in the size of each field in the stack layout */
  layout->arg_size      = current_function_pretend_args_size;
  layout->reg_size      = UNITS_PER_WORD * (SP_REGNUM - layout->first_reg);
  layout->local_size    = frame_size;
  layout->outbound_size = current_function_outgoing_args_size;
}

int
rasc_initial_elimination_offset (int from, int to)
{
  int above_frame;
  int below_frame;
  struct rasc_stack_layout layout;
  rasc_get_stack_layout(&layout);
  
  /* fp to ap */
  above_frame = layout.local_size + layout.reg_size;
  /* sp to fp */
  below_frame = layout.outbound_size;
  
  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return above_frame;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return above_frame + below_frame;

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return below_frame;

  gcc_unreachable ();
}

// 
// push {r<x>-r14}
// sub  sp,local_size
// mov  fp,sp
// sub  sp,outbound_size
//
// push {r<x>-r14}
// sub  sp,local_size + outbound_size

static void
emit_subsp(int size)
{
  if (size)
    {
      rtx insn;
      insn = gen_subsi3(stack_pointer_rtx, stack_pointer_rtx, GEN_INT(size));
      insn = emit_insn(insn);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

static void
emit_savearg(int offset, int regno)
{
  rtx insn;
  rtx addr = plus_constant(stack_pointer_rtx, offset);
  addr = memory_address(SImode, addr);

  insn = gen_rtx_SET (VOIDmode,
                      gen_rtx_MEM (Pmode, addr),
                      gen_rtx_REG(SImode, regno));
  insn = emit_insn(insn);
  RTX_FRAME_RELATED_P (insn) = 1;
}

int
rasc_return_allowed (void)
{
  return cfun->machine->return_allowed;
}

void
rasc_expand_prologue (void)
{
  struct rasc_stack_layout layout;

  rasc_get_stack_layout(&layout);

  if (layout.arg_size)
    {
      int i;
      emit_subsp(layout.arg_size);
      /* If we have a parameter passed partially in regs and partially in
         memory, the registers will have been stored to memory already in
         function.c.  So we only need to do something here for varargs
         functions.  */
      if (cfun->machine->n_varargs > 0)
        for(i = 0; i < layout.arg_size; i += UNITS_PER_WORD)
            emit_savearg(i,
               MAX_ARGS_IN_REGISTERS - layout.arg_size / UNITS_PER_WORD
               + i / UNITS_PER_WORD);
    }

  if (layout.reg_size)
    {
        int i;
        rtx dwarf, tmp;
        int nof_reg = LR_REGNUM - layout.first_reg + 1;
        rtx insn = gen_push(GEN_INT(layout.first_reg));
        insn = emit_insn(insn);
        RTX_FRAME_RELATED_P (insn) = 1;

        dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (1 + nof_reg));

        tmp = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
                           plus_constant (stack_pointer_rtx, -layout.reg_size));
        RTX_FRAME_RELATED_P (tmp) = 1;
        XVECEXP (dwarf, 0, 0) = tmp;

        for (i = 0; i < nof_reg; i++) {
            tmp = gen_rtx_SET (VOIDmode,
                               gen_frame_mem (SImode,
                                              plus_constant (stack_pointer_rtx,
                                                             i * 4)),
                               gen_rtx_REG(SImode, layout.first_reg + i));
            RTX_FRAME_RELATED_P (tmp) = 1;
            XVECEXP (dwarf, 0, i + 1) = tmp;
        }

        REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
                                              dwarf, REG_NOTES (insn));

      /* lr_needed will be set when for example __builtin_return_addr is used
         in that case we have to reload lr since push clobbers lr
         TODO: Handle this better.  Technically lr is stored by the push so
         rasc_return_addr_rtx should be able to load from this location
         directly.  However the frame is not laid out when
         rasc_return_addr_rtx is invoked, so how can we generate the correct
         stack offset?  */
      if ( cfun->machine->lr_needed)
        {
          rtx addr = plus_constant(stack_pointer_rtx,
                                   layout.reg_size - UNITS_PER_WORD);
          addr = memory_address(SImode, addr);
          insn = gen_rtx_SET (VOIDmode,
                              gen_rtx_MEM(Pmode, addr),
                              gen_rtx_REG(SImode, LR_REGNUM));
          insn=emit_insn(insn);
          RTX_FRAME_RELATED_P (insn) = 1;
        }
    }

  if (frame_pointer_needed)
    {
      rtx insn;
      emit_subsp(layout.local_size);
      insn = gen_rtx_SET (VOIDmode,
                          gen_rtx_REG(SImode, FP_REGNUM),
                          stack_pointer_rtx);
      insn=emit_insn(insn);
      RTX_FRAME_RELATED_P (insn) = 1;
      emit_subsp(layout.outbound_size);
    }
  else
    emit_subsp(layout.local_size+layout.outbound_size);

  if (layout.reg_size && !layout.arg_size)
    cfun->machine->popr_regs = layout.first_reg;
  else
    cfun->machine->popr_regs = 15;
  cfun->machine->return_allowed =
    !layout.arg_size
    && !frame_pointer_needed
    && (layout.local_size + layout.outbound_size) == 0;
}
  
// 
// mov  sp,fp
// add  sp,local_size
// popr {r<x>-r14}
//
// add  sp,local_size + outbound_size
// popr {r<x>-r14}
//
// rts

static void
emit_addsp(int size)
{
  if (size)
    emit_insn(gen_addsi3(stack_pointer_rtx, stack_pointer_rtx, GEN_INT(size)));
}

void
rasc_expand_epilogue (bool is_sibcall_epilogue)
{
  struct rasc_stack_layout layout;

  rasc_get_stack_layout(&layout);

  if (frame_pointer_needed)
    {
      emit_move_insn(stack_pointer_rtx, gen_rtx_REG(SImode, FP_REGNUM));
      emit_addsp(layout.local_size);
    }
  else
    emit_addsp(layout.local_size+layout.outbound_size);

  if (layout.reg_size && layout.arg_size)
    {
      emit_insn(gen_pop(GEN_INT(layout.first_reg)));
      emit_addsp(layout.arg_size);
    }
  else if (layout.arg_size)
    emit_addsp(layout.arg_size);

  if (is_sibcall_epilogue)
    {
      if (cfun->machine->popr_regs != 15)
        emit_insn(gen_pop(GEN_INT(cfun->machine->popr_regs)));
    }
  else
    emit_jump_insn (gen_return_internal());
}

char *
rasc_emit_return_internal (void)
{
  static char pattern[100];
  if (cfun->machine->popr_regs == 15)
    sprintf(pattern, "rts");
  else
    sprintf(pattern, "popr\tr%d-lr", cfun->machine->popr_regs);
  return pattern;
}

static bool rasc_function_ok_for_sibcall (tree a ATTRIBUTE_UNUSED,
                                          tree b ATTRIBUTE_UNUSED)
{
  /* Do sibcall only to functions identified by a symbol.  We do not handle
     calls through function pointers as they need too much work (the function
     address must be placed in a register that do not conflict with the
     parameters to the function, and is not clobbered by the sibcall
     epilogue).  */
  return a != NULL;
}

/* Do any needed setup for a variadic function.  CUM has not been updated
   for the last named argument which has type TYPE and mode MODE.

   We generate the actual spill instructions during prologue generation.  */

static void
rasc_setup_incoming_varargs (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                             tree type, int * pretend_size,
                             int second_time ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS next_cum = *cum;
  
  /* Skip the current argument.  */
  rasc_function_arg_advance(&next_cum, mode, type, 1);

  if (next_cum.reg_count < MAX_ARGS_IN_REGISTERS)
    {
      int n = MAX_ARGS_IN_REGISTERS - next_cum.reg_count;
      *pretend_size = n * UNITS_PER_WORD;
      cfun->machine->n_varargs = n;
    }
}


/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */

rtx rasc_return_addr_rtx (int count, rtx frame ATTRIBUTE_UNUSED) {
  if (count != 0)
    return const0_rtx;
  // Tell the prologue handler that lr is needed. 
  cfun->machine->lr_needed = 1;
  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}


/* The literal pool stays with the function.  */

static section *
rasc_select_rtx_section (enum machine_mode mode ATTRIBUTE_UNUSED,
                           rtx x ATTRIBUTE_UNUSED,
                           unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  return function_section (current_function_decl);
}


/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
rasc_rtx_costs (rtx x ATTRIBUTE_UNUSED, int code, int outer_code, int *total)
{
  switch (code)
    {
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      *total = COSTS_N_INSNS (50);
      break;

    case NEG:
      if (outer_code == AND)
        *total = 0;
      break;

    case MULT:
      *total = COSTS_N_INSNS (16);
      break;

    case CONST:
      /* CONST is only used to encapsulate CONST_INT etc. */
      *total = 0;
      break;

    case CONST_INT:
        if (((0 <= INTVAL (x)) && (INTVAL (x) < 12)) ||
            ((-12 <= INTVAL (x)) && (INTVAL (x) < 0)))
            *total = 0;
        else
            *total = 4;
      return true;

    case LABEL_REF:
      *total = 4;
      return true;

    case SYMBOL_REF:
      *total = 4;
      return true;

    case MEM:
      *total = 4;
      break;

    case CONST_DOUBLE:
      *total = 8;
      return true;

    case IF_THEN_ELSE:
      *total = COSTS_N_INSNS (2);
      return true;

    case SET:
      *total = COSTS_N_INSNS (1);
      break;

    default:
      break;
    }

  return false;
}

/* We give the cost corresponding to how big the instruction will be. */
static int
rasc_address_cost (rtx xinsn)
{
  while (GET_CODE (xinsn) == SUBREG)
    xinsn = SUBREG_REG (xinsn);

  /* Base registers. */
  if (GET_CODE (xinsn) == REG)
    return 0;

  /* "Register + offset" addressing. */
  if (GET_CODE (xinsn) == PLUS)
    {
      rtx xplus0 = XEXP (xinsn, 0);
      rtx xplus1 = XEXP (xinsn, 1);
      enum rtx_code code0;
      enum rtx_code code1;

      while (GET_CODE (xplus0) == SUBREG)
        xplus0 = SUBREG_REG (xplus0);
      code0 = GET_CODE (xplus0);

      while (GET_CODE (xplus1) == SUBREG)
        xplus1 = SUBREG_REG (xplus1);
      code1 = GET_CODE (xplus1);

      /* Swap operands if necessary so the register is first. */
      if (code0 != REG && code1 == REG)
        {
          xplus0 = XEXP (xinsn, 1);
          xplus1 = XEXP (xinsn, 0);
          code0 = GET_CODE (xplus0);
          code1 = GET_CODE (xplus1);
        }

      if (code1 == CONST_INT)
        {
            /* Let all constant be cheap -- otherwise gcc adds more
             * expensive add/sub to get it in range.  This might need
             * to be re-evaluated when the global cost model has been
             * better tuned.  */
            return 0;
        }

      /* Give label/symbol references slightly higher cost than what is likely
       * in reality, so that we prefer other alternatives if possible.  */
      return 4;
    }

  gcc_unreachable ();
}


/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
rasc_return_in_memory (tree type, tree fntype ATTRIBUTE_UNUSED)
{
  return ((unsigned HOST_WIDE_INT) int_size_in_bytes (type)
          > 4 * UNITS_PER_WORD);
}

/* Implement TARGET_SHIFT_TRUNCATION_MASK.  SImode shifts use normal
  RASC insns and therefore guarantee that the shift count is modulo 64.
  NOTE: The intent of adding this was to get optimizations similar to
  those achieved using SHIFT_COUNT_TRUNCATED, but with a different modulo.
  Currently I haven't been able to confirm any optimization resulting from this. */
static unsigned HOST_WIDE_INT
rasc_shift_truncation_mask (enum machine_mode mode)
{
  return mode == SImode ? 63 : 0;
}

static unsigned int
rasc_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  /* SECTION_OVERRIDE is used to prevent the error ("%J%D causes a section
     type conflict", decl, decl);  This error is issued e.g. when a function
     and a global variable are put in the same section.  However, in systems
     with a shared direct mapped I/D cache you might want to do this to gain
     cache efficiency.  */
  if (strncmp(name, ".pool", 5) == 0)
    flags |= SECTION_OVERRIDE;

  if (strncmp (name, ".rascstack", 7) == 0)
    {
      if (decl && TREE_CODE (decl) == VAR_DECL
          && DECL_INITIAL (decl) == NULL_TREE)
        flags |= SECTION_BSS;  /* @nobits */
      else
        warning (0, "only uninitialized variables can be placed in the "
                 ".rascstack section");
    }

  return flags;
}

/* Try to expand a block move operation to an inlined memcpy.  This function
   does only expand when it can do a reasonable efficient implementation
   with regard to both size and speed; it falls back to calling memcpy()
   for the other cases.  The GCC middle-end does the memory-to-memory
   expansion governed by MOVE_RATIO before it tries this function, i.e.
   the block to move is relatively big.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

int
rasc_expand_block_move (rtx *operands)
{
  rtx src_addr, dst_addr, tmp_hi, tmp_qi;
  HOST_WIDE_INT bytes, align;

  /* If this is not a fixed size move, just call memcpy.  */
  if (!optimize || (GET_CODE (operands[2]) != CONST_INT))
    return 0;

  bytes = INTVAL (operands[2]);
  align = INTVAL (operands[3]);

  /* Anything to move?  */
  if (bytes <= 0)
    return 0;

  /* We do only deal with word-aligned data as other and unknown alignment
     expand the code too much if we generate efficient code.  */
  if (align < 4)
      return 0;

  /* Copy the addresses to a new register.  This is needed as we will
     modify the value.  */
  dst_addr = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (VOIDmode, dst_addr,
                          force_reg (Pmode, XEXP (operands[0], 0))));
  src_addr = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (VOIDmode, src_addr,
                          force_reg (Pmode, XEXP (operands[1], 0))));

  /* Handle the part that can be copied one word at a time.  */
  if (bytes >= 4)
    {
        rtx iter, label, cond, if_then_else;

      iter = gen_reg_rtx (SImode);
      emit_insn (gen_rtx_SET (VOIDmode, iter, GEN_INT (bytes / 4)));

      label = gen_label_rtx ();
      emit_label (label);

      emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (SImode, dst_addr),
                              gen_rtx_MEM (SImode, src_addr)));
      emit_insn(gen_addsi3(src_addr, src_addr, GEN_INT (4)));
      emit_insn(gen_addsi3(dst_addr, dst_addr, GEN_INT (4)));

      emit_insn(gen_subsi3(iter, iter, GEN_INT (1)));
      cond = gen_rtx_fmt_ee (NE, VOIDmode, iter, GEN_INT (0));
      if_then_else = gen_rtx_IF_THEN_ELSE (VOIDmode, cond,
                                           gen_rtx_LABEL_REF (VOIDmode, label),
                                           pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, if_then_else));
    }

  /* Handle the remaining non-word sized part of the data.  We load the
     values before storing them in order to prevent pipe stalls, as GCC
     does not seem to dare move the loads over the stores.  */
  tmp_hi = tmp_qi = NULL;
  if (bytes & 2)
    {
      tmp_hi = gen_reg_rtx (HImode);
      emit_insn (gen_rtx_SET (VOIDmode, tmp_hi,
                              gen_rtx_MEM (HImode, src_addr)));
      emit_insn (gen_addsi3(src_addr, src_addr, GEN_INT (2)));
    }
  if (bytes & 1)
    {
        tmp_qi = gen_reg_rtx (QImode);
        emit_insn (gen_rtx_SET (VOIDmode, tmp_qi,
                                gen_rtx_MEM (QImode, src_addr)));
    }
  if (tmp_hi != NULL)
    {
      emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (HImode, dst_addr),
                              tmp_hi));
      emit_insn(gen_addsi3(dst_addr, dst_addr, GEN_INT (2)));
    }
  if (tmp_qi != NULL)
    emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (QImode, dst_addr), tmp_qi));

  return 1;
}

int rasc_expand_block_set (rtx *operands)
{
    rtx dst_addr, val;
  HOST_WIDE_INT bytes, align, value;

  /* If this is not a fixed size/value set, just call memset.  */
  if (!optimize ||
      (GET_CODE (operands[1]) != CONST_INT) ||
      (GET_CODE (operands[2]) != CONST_INT))
    return 0;

  bytes = INTVAL (operands[1]);
  value = INTVAL (operands[2]);
  align = INTVAL (operands[3]);

  /* Anything to set?  */
  if (bytes <= 0)
    return 0;

  /* We do only deal with word-aligned data as other and unknown alignment
     expand the code too much if we generate efficient code.  */
  if (align < 4)
      return 0;

  /* Duplicate the byte over a full word.  */
  value = value & 0xff;
  value = (value << 8) | value;
  value = (value << 16) | value;

  /* Copy the addresses to a new register.  This is needed as we will
     modify the value.  */
  dst_addr = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (VOIDmode, dst_addr,
                          force_reg (Pmode, XEXP (operands[0], 0))));

  val = gen_reg_rtx (SImode);
  emit_insn (gen_rtx_SET (VOIDmode, val, GEN_INT (value)));

  /* Handle the part that can be copied one word at a time.  */
  if (bytes >= 4)
    {
      rtx iter, label, cond, if_then_else;

      iter = gen_reg_rtx (SImode);
      emit_insn (gen_rtx_SET (VOIDmode, iter, GEN_INT (bytes / 4)));

      label = gen_label_rtx ();
      emit_label (label);

      emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (SImode, dst_addr), val));
      emit_insn(gen_addsi3(dst_addr, dst_addr, GEN_INT (4)));

      emit_insn(gen_subsi3(iter, iter, GEN_INT (1)));
      cond = gen_rtx_fmt_ee (NE, VOIDmode, iter, GEN_INT (0));
      if_then_else = gen_rtx_IF_THEN_ELSE (VOIDmode, cond,
                                           gen_rtx_LABEL_REF (VOIDmode, label),
                                           pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, if_then_else));
    }

  /* Handle the remaining non-word sized part of the data.  */
  if (bytes & 2)
    {
      emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (HImode, dst_addr),
                              gen_lowpart (HImode, val)));
      emit_insn(gen_addsi3(dst_addr, dst_addr, GEN_INT (2)));
    }
  if (bytes & 1)
    emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (QImode, dst_addr),
                            gen_lowpart (QImode, val)));

  return 1;
}

#include "gt-rasc.h"
