/* Definitions of target machine for GNU compiler,
   for RASC Processor. */
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

/* Standard GCC variables that we reference.  */
extern int current_function_calls_alloca;
extern int target_flags;
extern int optimize;

/* External variables defined in rasc.c.  */

#define OVERRIDE_OPTIONS override_options ()

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()                                       \
  do {                                                                  \
    builtin_assert ("cpu=rasc");                                        \
    builtin_assert ("machine=rasc");                                    \
    builtin_define ("__rasc__");                                        \
    builtin_define ("__RASC__");                                        \
  } while (0)

#define CPP_SPEC " %(subtarget_cpp_spec) "

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

#define EXTRA_SPECS                                                     \
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest.  */
#define WORDS_BIG_ENDIAN 0

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4
#define MIN_UNITS_PER_WORD 4

/* Width of a floating point register.  */
#define UNITS_PER_FPREG 4

/* Size in bits of various types on the target machine.  */
#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 8

/* Alignment of field after 'int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 32

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Promote integer modes smaller than a word to SImode.  Set UNSIGNEDP
   for QImode and HImode, since we cannot do sign extension in the load
   instruction.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)                             \
  do {                                                                  \
    if (GET_MODE_CLASS (MODE) == MODE_INT                               \
        && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)                       \
      {                                                                 \
        if ((MODE) == QImode || (MODE) == HImode)                       \
          (UNSIGNEDP) = 1;                                              \
        (MODE) = SImode;                                                \
      }                                                                 \
  } while (0)

/* Imitate the way many other C compilers handle alignment of
   bitfields and the structures that contain them.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Align constructors to at least a word boundary.
   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that 'strcpy' calls that copy
   constants can be done inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)                                  \
  ((TREE_CODE (EXP) == CONSTRUCTOR)                                     \
   && (ALIGN) < BITS_PER_WORD                                           \
        ? BITS_PER_WORD                                                 \
        : (ALIGN))

/* Align arrays, unions and records to at least a word boundary.
   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that 'strcpy' calls
   that copy constants to character arrays can be done inline.  */
#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)                                     \
  ((((ALIGN) < BITS_PER_WORD)                                           \
    && (TREE_CODE (TYPE) == ARRAY_TYPE                                  \
        || TREE_CODE (TYPE) == UNION_TYPE                               \
        || TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* rasc loads are zero-extended by default.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Standard register usage.  */

/* Number of actual hardware registers + fake register.  */
#define FIRST_PSEUDO_REGISTER 17

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS                                                 \
{                                                                       \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,                       \
  1                                                                     \
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS                                             \
{                                                                       \
  1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,                       \
  1                                                                     \
}

/* Allocate r0 through r4 in reverse order since r4 is least likely
   to contain a function parameter; in addition results are returned
   in r0.  */
#define REG_ALLOC_ORDER                                                 \
{                                                                       \
   4,  3,  2,  1,  0, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5, 15,       \
  16                                                                    \
}

/* The LR (r14) is marked as "not available across function call" which
   the register renamer interprets as it can safely be renamed to at any
   time.  But in reality, it cannot be used unless it has been saved by the
   prologue.  */
#define HARD_REGNO_RENAME_OK(SRC, DST) \
  (GP_REG_P(DST) && (((DST) != LR_REGNUM) || regs_ever_live[LR_REGNUM]))

/* Internal macros to classify a register number.  */

/* 16 general registers (+ fake_argp register not included in the range). */
#define GP_REG_FIRST       0
#define GP_REG_LAST       15
#define GP_FAKE_REG_ARGP  16
#define GP_REG_NUM        (GP_REG_LAST - GP_REG_FIRST + 1)

#define GP_REG_P(REGNO) (((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM) \
                         || REGNO == GP_FAKE_REG_ARGP)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)                                   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   Avoid r4 for DImode, as DImode need two adjacent registers, and spilling
   over to r5 forces the whole range r5-r14 to be pushed on the stack.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)                                 \
  (GP_REG_P(REGNO)                                                      \
   && ((MODE) != V8HImode)                                              \
   && !((MODE) == DImode && (REGNO) == (GP_REG_FIRST + 4)))

/* A C expression that is nonzero if a value of mode MODE1 is accessible
   in mode MODE2 without copying.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) != V8HImode && (MODE2) != V8HImode)

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (GP_REG_FIRST + 15)

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame. */
#define FRAME_POINTER_REGNUM (GP_REG_FIRST + 13)

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */
#define FRAME_POINTER_REQUIRED  0

/* Base register for access to arguments of the function.  This is later
   eliminated to the frame pointer or stack pointer.  */
#define ARG_POINTER_REGNUM (GP_FAKE_REG_ARGP)

/* Define the classes of registers for register constraints in the
   machine description.  */
enum reg_class
{
  NO_REGS,                      /* no registers in set */
  SP_REGS,                      /* sp register */
  HR_REGS,                      /* integer registers except lr and sp */
  GR_REGS,                      /* integer registers except sp */
  AR_REGS,                      /* all integer registers */
  ALL_REGS,                     /* all registers */
  LIM_REG_CLASSES               /* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS AR_REGS

/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */
#define REG_CLASS_NAMES                                                 \
{                                                                       \
  "NO_REGS",                                                            \
  "SP_REGS",                                                            \
  "HR_REGS",                                                            \
  "GR_REGS",                                                            \
  "AR_REGS",                                                            \
  "ALL_REGS"                                                            \
}

/* Contents of the register classes.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if 'MASK & (1 << R)' is 1.  */
#define REG_CLASS_CONTENTS                                             \
{                                                                      \
  { 0x00000000}, /* no registers */                                    \
  { 0x00008000}, /* sp registers */                                    \
  { 0x00003fff}, /* r0-r13 */                                          \
  { 0x00007fff}, /* general-purpose registers */                       \
  { 0x0001ffff}, /* integer registers */                               \
  { 0x0001ffff}  /* all registers */                                   \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
extern const enum reg_class rasc_regno_to_class[FIRST_PSEUDO_REGISTER];

#define REGNO_REG_CLASS(REGNO) rasc_regno_to_class[ (REGNO) ]

/* Use the rasc AR register file for base registers.
   No index registers.  */
#define BASE_REG_CLASS AR_REGS
#define INDEX_REG_CLASS NO_REGS

#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  (((CLASS) == SP_REGS || (CLASS) == AR_REGS) ? GR_REGS : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_UNITS(mode, size)                                         \
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

#define CLASS_MAX_NREGS(CLASS, MODE)                                    \
  (CLASS_UNITS (MODE, UNITS_PER_WORD))

/* It is as good or better to call a constant function address than to
   call an address kept in a register (since the register version is
   slower due to an extra load to set up the register and usually need
   one extra register being saved on the stack.  And this extra work often
   makes the resulting code bigger too).  */
#define NO_FUNCTION_CSE 1


/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.  */
#define STARTING_FRAME_OFFSET 0

/* The ARG_POINTER and FRAME_POINTER are not real rasc registers, so
   they are eliminated to either the stack pointer or frame pointer.  */
#define ELIMINABLE_REGS                                                 \
{{ ARG_POINTER_REGNUM,          STACK_POINTER_REGNUM},  \
 { ARG_POINTER_REGNUM,          FRAME_POINTER_REGNUM},  \
 { FRAME_POINTER_REGNUM,        STACK_POINTER_REGNUM}}

#define CAN_ELIMINATE(FROM, TO) 1

/* Specify the initial difference between the specified pair of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = rasc_initial_elimination_offset(FROM, TO)

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   'current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue
   should increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  If 'ARGS_GROW_DOWNWARD', this is the offset to the
   location above the first argument's address.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Align stack frames on 32 bits for rasc.  */
#define STACK_BOUNDARY 32

/* Functions do not pop arguments off the stack.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0

#define MAX_ARGS_IN_REGISTERS 5

/* Symbolic macros for the registers used to return integer, floating
   point, and values of coprocessor and user-defined modes.  */
#define GP_RETURN (GP_REG_FIRST + 0)
#define GP_OUTGOING_RETURN (GP_REG_FIRST + 0)

/* Symbolic macros for the first/last argument registers.  */
#define GP_ARG_FIRST (GP_REG_FIRST + 0)
#define GP_ARG_LAST  (GP_REG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
#define GP_OUTGOING_ARG_FIRST (GP_REG_FIRST + 0)
#define GP_OUTGOING_ARG_LAST  (GP_REG_FIRST + MAX_ARGS_IN_REGISTERS - 1)

/* Register in which the static-chain is passed to a function. 
   We use the first register after the argument registers, hence a
   low as possible register. This register will not be pushed in
   the function prologue if the function is a nested function */
#define STATIC_CHAIN_REGNUM (GP_REG_FIRST + MAX_ARGS_IN_REGISTERS)


/* Don't worry about compatibility with PCC.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  Because we have defined
   TARGET_PROMOTE_FUNCTION_RETURN that returns true, we have to
   perform the same promotions as PROMOTE_MODE.  */
#define RASC_LIBCALL_VALUE(MODE, OUTGOINGP)                             \
  gen_rtx_REG ((GET_MODE_CLASS (MODE) == MODE_INT                       \
                && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)               \
               ? SImode : (MODE),                                       \
               OUTGOINGP ? GP_OUTGOING_RETURN : GP_RETURN)

#define LIBCALL_VALUE(MODE)                                             \
  RASC_LIBCALL_VALUE ((MODE), 0)

#define LIBCALL_OUTGOING_VALUE(MODE)                                    \
  RASC_LIBCALL_VALUE ((MODE), 1)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define RASC_FUNCTION_VALUE(VALTYPE, FUNC, OUTGOINGP)                   \
  gen_rtx_REG ((INTEGRAL_TYPE_P (VALTYPE)                               \
                && TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)            \
               ? SImode: TYPE_MODE (VALTYPE),                           \
               OUTGOINGP ? GP_OUTGOING_RETURN : GP_RETURN)

#define FUNCTION_VALUE(VALTYPE, FUNC)                                   \
  RASC_FUNCTION_VALUE (VALTYPE, FUNC, 0)

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)                          \
  RASC_FUNCTION_VALUE (VALTYPE, FUNC, 1)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.  A
   register whose use for returning values is limited to serving as
   the second of a pair (for a value of type 'double', say) need not
   be recognized by this macro.  If the machine has register windows,
   so that the caller and the called function use different registers
   for the return value, this macro should recognize only the caller's
   register numbers.  */
#define FUNCTION_VALUE_REGNO_P(N)                                       \
  ((N) == GP_RETURN)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  This
   does *not* include implicit arguments such as the static chain and
   the structure-value address.  On many machines, no registers can be
   used for this purpose since all function arguments are pushed on
   the stack.  */
#define FUNCTION_ARG_REGNO_P(N)                                         \
  (/* (N) >= GP_OUTGOING_ARG_FIRST && */ (N) <= GP_OUTGOING_ARG_LAST)

typedef struct rasc_args
{
  int reg_count;   /* Number of argument words so far */
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  rasc_init_cumulative_args (&CUM)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)                    \
  rasc_function_arg_advance (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  rasc_function_arg (&CUM, MODE, TYPE, NAMED)

/* Profiling RASC code is typically done with the built-in profiling
   feature of the RASC emulator.  Profiling code on a real (i.e.,
   non-simulated) RASC processor is currently not supported.  */

#define NO_PROFILE_COUNTERS     1
#define FUNCTION_PROFILER(FILE, LABELNO)                                \
  do {                                                                  \
  } while (0)

/* Stack pointer value doesn't matter at exit.  */
#define EXIT_IGNORE_STACK 1

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */
/* NOTE: lpc r14, +8.  Coded as bytes to prevent relaxation. */
#define TRAMPOLINE_TEMPLATE(STREAM) \
  do {                                                                  \
    fprintf(STREAM, "    sub     sp,8\n");                              \
    fprintf(STREAM, "    st      r%d,[sp]\n", STATIC_CHAIN_REGNUM);     \
    fprintf(STREAM, "    st      r14,[sp,4]\n");                        \
    fprintf(STREAM, "    .byte   0x53, 0xfe, 8, 0\n");                  \
    fprintf(STREAM, "    ld      r%d,[r14,8]\n", STATIC_CHAIN_REGNUM);  \
    fprintf(STREAM, "    ld      r14,[r14,12]\n");                      \
    fprintf(STREAM, "    jsr     r14\n");                               \
    fprintf(STREAM, "    ld      r%d,[sp]\n", STATIC_CHAIN_REGNUM);     \
    fprintf(STREAM, "    ld      r14,[sp,4]\n");                        \
    fprintf(STREAM, "    add     sp,8\n");                              \
    fprintf(STREAM, "    rts\n");                                       \
    fprintf(STREAM, "    .long   0 // chain\n");                        \
    fprintf(STREAM, "    .long   0 // func\n");                         \
  } while (0)

/* Size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE 32

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 32

/* A C statement to initialize the variable parts of a trampoline.  */
/* NOTE: No I/D-cache sync. Will fail in a system with separate I and D cache */
#define INITIALIZE_TRAMPOLINE(ADDR,FUNC,CHAIN) \
  do {                                                                      \
    emit_move_insn (gen_rtx_MEM (SImode, plus_constant (ADDR, 24)), CHAIN); \
    emit_move_insn (gen_rtx_MEM (SImode, plus_constant (ADDR, 28)), FUNC);  \
  } while (0)


/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current
   frame, after the prologue.  */
#define RETURN_ADDR_RTX(count,frame) rasc_return_addr_rtx(count,frame)

/* Pick up the return address upon entry to a procedure.  Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */
#define INCOMING_RETURN_ADDR_RTX    gen_rtx_REG (Pmode, LR_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN   DWARF_FRAME_REGNUM (LR_REGNUM)

/* Addressing modes, and classification of registers for them.  */

/* C expressions which are nonzero if register number NUM is suitable
   for use as a base or index register in operand addresses.  It may
   be either a suitable hard register or a pseudo register that has
   been allocated such a hard register. The difference between an
   index register and a base register is that the index register may
   be scaled.  */

#define REGNO_OK_FOR_BASE_P(NUM) \
  (GP_REG_P (NUM) || GP_REG_P ((unsigned) reg_renumber[NUM]))

#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* C expressions that are nonzero if X (assumed to be a `reg' RTX) is
   valid for use as a base or index register.  For hard registers, it
   should always accept those which the hardware permits and reject
   the others.  Whether the macro accepts or rejects pseudo registers
   must be controlled by `REG_OK_STRICT'.  This usually requires two
   variant definitions, of which `REG_OK_STRICT' controls the one
   actually used. The difference between an index register and a base
   register is that the index register may be scaled.  */

#ifdef REG_OK_STRICT

#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X) \
  REGNO_OK_FOR_BASE_P (REGNO (X))

#else /* !REG_OK_STRICT */

#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X) \
  ((REGNO (X) >= FIRST_PSEUDO_REGISTER) || (GP_REG_P (REGNO (X))))

#endif /* !REG_OK_STRICT */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Identify valid rasc addresses.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, ADDR, LABEL)                     \
  do {                                                                  \
    rtx xinsn = (ADDR);                                                 \
                                                                        \
    while (GET_CODE (xinsn) == SUBREG)                                  \
      xinsn = SUBREG_REG (xinsn);                                       \
                                                                        \
    /* Allow base registers.  */                                        \
    if (GET_CODE (xinsn) == REG && REG_OK_FOR_BASE_P (xinsn))           \
      goto LABEL;                                                       \
                                                                        \
    /* Check for "register + offset" addressing.  */                    \
    if (GET_CODE (xinsn) == PLUS)                                       \
      {                                                                 \
        rtx xplus0 = XEXP (xinsn, 0);                                   \
        rtx xplus1 = XEXP (xinsn, 1);                                   \
        enum rtx_code code0;                                            \
        enum rtx_code code1;                                            \
                                                                        \
        while (GET_CODE (xplus0) == SUBREG)                             \
          xplus0 = SUBREG_REG (xplus0);                                 \
        code0 = GET_CODE (xplus0);                                      \
                                                                        \
        while (GET_CODE (xplus1) == SUBREG)                             \
          xplus1 = SUBREG_REG (xplus1);                                 \
        code1 = GET_CODE (xplus1);                                      \
                                                                        \
        /* Swap operands if necessary so the register is first.  */     \
        if (code0 != REG && code1 == REG)                               \
          {                                                             \
            xplus0 = XEXP (xinsn, 1);                                   \
            xplus1 = XEXP (xinsn, 0);                                   \
            code0 = GET_CODE (xplus0);                                  \
            code1 = GET_CODE (xplus1);                                  \
          }                                                             \
                                                                        \
        if (code0 == REG && REG_OK_FOR_BASE_P (xplus0)                  \
            && CONSTANT_P (xplus1))                                     \
          {                                                             \
              /* Permit non-negative integer constants (negative   */   \
              /* constants bloats the binary too much, and the     */   \
              /* loop optimizers creates such if permitted).       */   \
              if (code1 == CONST_INT && INTVAL (xplus1) >= 0)           \
                goto LABEL;                                             \
                                                                        \
              /* Permit negative integer constants if it seems     */   \
              /* to be a HW address (their absolute values are     */   \
              /* big enough that the loop optimizers will not      */   \
              /* try to create them even if they are permitted).   */   \
              if (code1 == CONST_INT &&                                 \
                  (INTVAL (xplus1) & 0xff800000) != 0xff800000)         \
                goto LABEL;                                             \
                                                                        \
              /* Permit non-integer constant index (i.e. labels),  */   \
              /* as this generates better code when indexing       */   \
              /* in constant arrays.                               */   \
              if (code1 != CONST_INT)                                   \
                goto LABEL;                                             \
          }                                                             \
      }                                                                 \
  } while (0)

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  */
#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
#define LEGITIMATE_CONSTANT_P(X) 1

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)                       \
  do {                                                                  \
  } while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (SImode)

/* Define this as 1 if 'char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4
#define MAX_MOVE_MAX 4

/* Prefer word-sized loads.  */
#define SLOW_BYTE_ACCESS 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction is a word address (for
   indexing purposes) so give the MEM rtx a words's mode.  */
#define FUNCTION_MODE SImode

/* Inline memory-to-memory move and clear memory operations as long as the
   resulting code is smaller than a call to memcpy/memset or inlining of an
   opencoded version (note that we usually save some instruction by not
   calling memcpy, i.e. since memcpy will clobber r0-r4).  */
#define MOVE_RATIO  5
#define CLEAR_RATIO 5


/* Control the assembler format that we output.  */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES                                                  \
{                                                                       \
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7",         \
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "sp",         \
  "fake_argp"                                                           \
}

/* If defined, a C initializer for an array of structures containing a
   name and a register number.  This macro defines additional names
   for hard registers, thus allowing the 'asm' option in declarations
   to refer to registers using alternate names.  */
#define ADDITIONAL_REGISTER_NAMES                                       \
{                                                                       \
  { "lr",         14 + GP_REG_FIRST },                                  \
  { "r15",        15 + GP_REG_FIRST }                                   \
}

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* Declare an uninitialized external linkage data object.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)           \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)                          \
  fprintf (STREAM, "%s%sL%u\n", integer_asm_op (4, TRUE),               \
           LOCAL_LABEL_PREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.
   This is used for pc-relative code.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)              \
  do {                                                                  \
    fprintf (STREAM, "%s%sL%u-%sL%u\n", integer_asm_op (4, TRUE),       \
             LOCAL_LABEL_PREFIX, (VALUE),                               \
             LOCAL_LABEL_PREFIX, (REL));                                \
  } while (0)

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(STREAM, LOG)                                   \
  do {                                                                  \
    if ((LOG) != 0)                                                     \
      fprintf (STREAM, "\t.align\t%d\n", (LOG));                        \
  } while (0)

/* Having jump tables in the text section fails linker relaxation
   (as it cannot guarantee that the table stays word-aligned when it
   shrinks instructions).  */
#define JUMP_TABLES_IN_TEXT_SECTION 0


/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP     "\t.text"
#define DATA_SECTION_ASM_OP     "\t.data"
#define BSS_SECTION_ASM_OP      "\t.section\t.bss"


/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME) \
   fprintf (FILE, "%s", NAME)

/* How to start an assembler comment.  */
#define ASM_COMMENT_START "//"

/* Generate DWARF2 debugging information and make it the default */
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO 1

/* No specific purpose other than warningless compatibility.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP 1

/* Permit builtin transformations of C99 functions.  */
#undef TARGET_C99_FUNCTIONS
#define TARGET_C99_FUNCTIONS  1
