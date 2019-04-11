;; GCC machine description RASC.
;;  Copyright (C) 2007, 2008 ARM
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see http://www.gnu.org/licenses/.


;; Constants

(define_constants [
  (FP_REGNUM            13)
  (LR_REGNUM            14)
  (SP_REGNUM            15)

  (UNSPEC_PLT           1)
  (UNSPECV_PUSH         2)
  (UNSPECV_POP          3)
])


;; Attributes.

(define_attr "type"
  "unknown,jump,call,load,store,move,arith,multi,nop"
  (const_string "unknown"))

(define_attr "mode"
  "unknown,none,QI,HI,SI"
  (const_string "unknown"))

;; Let length denote the number of instructions (as this is the relevant
;; measure when calculating the cost of branches).
(define_attr "length" "" (const_int 1))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])


;; Pipeline model.

(define_insn_reservation "rasc_any_insn" 1
                         (eq_attr "type" "!load")
                         "nothing")

(define_insn_reservation "rasc_memory" 2
                         (eq_attr "type" "load")
                         "nothing")


;; Include predicates and constraints definitions.

(include "predicates.md")
(include "constraints.md")


;; Addition.

(define_insn "addsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,r,r")
        (plus:SI (match_operand:SI 1 "ar_register_operand" "%0,0,0")
                 (match_operand:SI 2 "nonmemory_operand" "a,I,i")))]
  ""
  "@
   add\t%0, %2
   sub\t%0, #%n2
   add\t%0, #%2"
  [(set_attr "type"     "arith,arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "adddi3"
  [(set (match_operand:DI 0 "ar_register_operand" "=a")
        (plus:DI (match_operand:DI 1 "ar_register_operand" "%0")
                 (match_operand:DI 2 "ar_register_operand" "a")))]
  ""
  "add\t%L0,%L2\;adc\t%H0,%H2"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")
   (set_attr "length"   "2")])


;; Subtraction.

(define_insn "subsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,r,a,a")
        (minus:SI (match_operand:SI 1 "nonmemory_operand" "0,0,a,i")
                 (match_operand:SI 2 "nonmemory_operand" "a,i,0,0")))]
  ""
  "@
   sub\t%0, %2
   sub\t%0, #%2
   rsb\t%0, %1
   rsb\t%0, #%1"
  [(set_attr "type"     "arith,arith,arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "ar_register_operand" "=a")
        (minus:DI (match_operand:DI 1 "ar_register_operand" "0")
                 (match_operand:DI 2 "ar_register_operand" "a")))]
  ""
  "sub\t%L0,%L2\;sbc\t%H0,%H2"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")
   (set_attr "length"   "2")])


;; Multiplication.
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (mult:SI (match_operand:SI 1 "ar_register_operand" "%0")
                 (match_operand:SI 2 "nonmemory_operand" "a")))]
  ""
  "@
   mul\t%0, %2"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])


;; Division.


;; Remainders.


;; Square roots.


;; Absolute value.


;; Min and max.


;; Find first bit.


;; Negation and one's complement.

(define_insn "negsi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (neg:SI (match_operand:SI 1 "ar_register_operand" "0")))]
  ""
  "rsb\t%0, 0"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (not:SI (match_operand:SI 1 "ar_register_operand" "0")))]
  ""
  "not\t%0"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])


;; Logical instructions.

(define_insn "andsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a,a")
        (and:SI (match_operand:SI 1 "ar_register_operand" "%0,0,0")
                (match_operand:SI 2 "nonmemory_operand" "a,I,i")))]
  ""
  {
    static const char * const asms[] =
    {
      "and\t%0, %2",
      "andn\t%0, #%N2",
      "and\t%0, #%2"
    };
    /* "zext" is smaller than "and". */
    if (which_alternative == 2 && INTVAL (operands[2]) == 0xff)
      return "zextb\t%0";
    if (which_alternative == 2 && INTVAL (operands[2]) == 0xffff)
      return "zexth\t%0";
    return asms[which_alternative];
  }
  [(set_attr "type"     "arith,arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "*andn"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (and:SI (not:SI (match_operand:SI 1 "nonmemory_operand" "a"))
                (match_operand:SI 2 "ar_register_operand" "0")))]

  ""
  "andn\t%0, %1"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (ior:SI (match_operand:SI 1 "ar_register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "a,i")))]
  ""
  "@
   or\t%0, %2
   or\t%0, #%2"
  [(set_attr "type"     "arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (xor:SI (match_operand:SI 1 "ar_register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "a,i")))]
  ""
  "@
   xor\t%0, %2
   xor\t%0, #%2"
  [(set_attr "type"     "arith,arith")
   (set_attr "mode"     "SI")])


;; Zero-extend instructions.

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0,m")))]
  ""
  "@
   zexth\t%0
   ldh\t%0, [%1]"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "SI")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,m")))]
  ""
  "@
   zextb\t%0
   ldb\t%0, [%1]"
  [(set_attr "type"     "arith,load")
   (set_attr "mode"     "SI")])


;; Sign-extend instructions.

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0")))]
  ""
  "sexth\t%0"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "ar_register_operand" "=a")
        (sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0")))]
  ""
  "sextb\t%0"
  [(set_attr "type"     "arith")
   (set_attr "mode"     "SI")])


;; Field extract instructions.


;; Conversions.


;; Integer data movement instructions.

;; 8-bit
(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand")
        (match_operand:QI 1 "general_operand"))]
  ""
  {
    if (!no_new_pseudos)
      {
        // Eliminate zext for volatile load.
        if (GET_CODE (operands[1]) == MEM && optimize > 0)
	  {
	    rtx reg = gen_reg_rtx (SImode);

	    emit_insn (gen_zero_extendqisi2 (reg, operands[1]));
	    operands[1] = gen_lowpart (QImode, reg);
	  }

        // Eliminate zext for volatile store.
	if (GET_CODE (operands[0]) == MEM && optimize > 0)
          {
	    emit_insn (gen_storeqi_single_op (operands[0],
                                              force_reg (QImode, operands[1])));
            DONE;
          }

        if (GET_CODE (operands[0]) != REG)
	  operands[1] = force_reg (QImode, operands[1]);
      }
  })

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,a,m")
        (match_operand:QI 1 "nonimmediate_operand" "r ,m,a"))]
  ""
  "@
   mov\t%0, %1
   ldb\t%0, [%1]
   stb\t%1, [%0]"
  [(set_attr "type"     "move,load,store")
   (set_attr "mode"     "QI")])

(define_insn "*movhi_internal_imm"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (match_operand:QI 1 "immediate_operand" "i"))]
  ""
  "mov\t%0, #%1"
  [(set_attr "type"     "move")
   (set_attr "mode"     "SI")])

(define_insn "storeqi_single_op"
  [(set (match_operand:QI 0 "memory_operand" "=m")
        (subreg:QI (match_operand:SI 1 "register_operand" "a") 0))]
  ""
  "stb\t%1, [%0]"
  [(set_attr "type"     "store")
   (set_attr "mode"     "QI")])

;; 16-bit
(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand")
        (match_operand:HI 1 "general_operand"))]
  ""
  {
    if (!no_new_pseudos)
      {
        // Eliminate zext for volatile load.
	if (GET_CODE (operands[1]) == MEM && optimize > 0)
	  {
	    rtx reg = gen_reg_rtx (SImode);

	    emit_insn (gen_zero_extendhisi2 (reg, operands[1]));
	    operands[1] = gen_lowpart (HImode, reg);
	  }

        // Eliminate zext for volatile store.
	if (GET_CODE (operands[0]) == MEM && optimize > 0)
          {
	    emit_insn (gen_storehi_single_op (operands[0],
                                              force_reg (HImode, operands[1])));
            DONE;
          }

        if (GET_CODE (operands[0]) != REG)
	  operands[1] = force_reg (HImode, operands[1]);
      }
  })

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,a,m")
        (match_operand:HI 1 "nonimmediate_operand" "r ,m,a"))]
  ""
  "@
   mov\t%0, %1
   ldh\t%0, [%1]
   sth\t%1, [%0]"
  [(set_attr "type"     "move,load,store")
   (set_attr "mode"     "HI")])

(define_insn "*movhi_internal_imm"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (match_operand:HI 1 "immediate_operand" "i"))]
  ""
  "mov\t%0, #%1"
  [(set_attr "type"     "move")
   (set_attr "mode"     "SI")])

(define_insn "storehi_single_op"
  [(set (match_operand:HI 0 "memory_operand" "=m")
        (subreg:HI (match_operand:SI 1 "register_operand" "a") 0))]
  ""
  "sth\t%1, [%0]"
  [(set_attr "type"     "store")
   (set_attr "mode"     "HI")])

;; 32-bit
(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand")
        (match_operand:SI 1 "general_operand"))]
  ""
  {
    if (!no_new_pseudos)
      {
        if (GET_CODE (operands[0]) != REG)
	  operands[1] = force_reg (SImode, operands[1]);
      }
  })

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,a,m")
        (match_operand:SI 1 "nonimmediate_operand" "r,m,a"))]
  ""
  "@
   mov\t%0, %1
   ld\t%0, [%1]
   st\t%1, [%0]"
  [(set_attr "type"     "move,load,store")
   (set_attr "mode"     "SI")])

(define_insn "*movsi_internal_imm"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "immediate_operand" "i"))]
  ""
  "mov\t%0, #%1"
  [(set_attr "type"     "move")
   (set_attr "mode"     "SI")])

;; 64-bit
(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand")
        (match_operand:DI 1 "general_operand"))]
  ""
  {
    if (!no_new_pseudos)
      {
        if (GET_CODE (operands[0]) != REG)
	  operands[1] = force_reg (DImode, operands[1]);
      }
  })

(define_insn_and_split "*movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,a,m")
        (match_operand:DI 1 "general_operand"       "r,i,m,a"))]
  "ar_register_operand (operands[0], DImode) || ar_register_operand (operands[1], DImode)"
  "#"
  "reload_completed"
  [(set (match_dup 0) (match_dup 2))
   (set (match_dup 1) (match_dup 3))]
  {
    rasc_split_operand (operands[1], &operands[2], SImode);
    rasc_split_operand (operands[0], &operands[0], SImode);
    if (reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp;
      tmp = operands[0], operands[0] = operands[1], operands[1] = tmp;
      tmp = operands[2], operands[2] = operands[3], operands[3] = tmp;
    }
  })

;; Block moves

(define_expand "movmemsi"
  [(use (match_operand:BLK 0 "memory_operand" ""))
   (use (match_operand:BLK 1 "memory_operand" ""))
   (use (match_operand:SI 2 "nonmemory_operand" ""))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  if (!rasc_expand_block_move (operands))
    FAIL;
  DONE;
})

(define_expand "setmemsi"
  [(use (match_operand:BLK 0 "memory_operand" ""))
   (use (match_operand:SI 1 "nonmemory_operand" ""))
   (use (match_operand:SI 2 "const_int_operand" ""))
   (use (match_operand:SI 3 "const_int_operand" ""))]
  ""
{
  if (!rasc_expand_block_set (operands))
    FAIL;
  DONE;
})


;; Floating point data movement instructions.


;; Block moves


;; Shift instructions.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a,a,a")
        (ashift:SI (match_operand:SI 1 "ar_register_operand" "a,a,0,0")
                   (match_operand:SI 2 "nonmemory_operand" "J,K,a,i")))]
  ""
  "@
   movh\t%0, %1
   movw\t%0, %1
   lsl\t%0, %2
   lsl\t%0, #%2"
  [(set_attr "type"     "arith,arith,arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (ashiftrt:SI (match_operand:SI 1 "ar_register_operand" "0,0")
                   (match_operand:SI 2 "nonmemory_operand" "a,i")))]
  ""
  "@
   asr\t%0, %2
   asr\t%0, #%2"
  [(set_attr "type"     "arith,arith")
   (set_attr "mode"     "SI")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "ar_register_operand" "=a,a")
        (lshiftrt:SI (match_operand:SI 1 "ar_register_operand" "0,0")
                   (match_operand:SI 2 "nonmemory_operand" "a,i")))]
  ""
  "@
   lsr\t%0, %2
   lsr\t%0, #%2"
  [(set_attr "type"     "arith,arith")
   (set_attr "mode"     "SI")])


(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))	; loop pseudo
   (use (match_operand 1 "" ""))	; iterations; zero if unknown
   (use (match_operand 2 "" ""))	; max iterations
   (use (match_operand 3 "" ""))	; loop level
   (use (match_operand 4 "" ""))]       ; label
  ""
  {
    if (GET_MODE (operands[0]) != SImode)
      FAIL;

    emit_jump_insn (gen_doloop_end_si (operands[0], operands[4]));
    DONE;
  }
)

(define_insn "doloop_end_si"
  [(set (pc)
        (if_then_else
         (ne (match_operand:SI 0 "ar_register_operand" "+a,m")
             (const_int 1))
         (label_ref (match_operand 1 "" ""))
         (pc)))
   (set (match_dup 0)
        (plus:SI (match_dup 0)
                 (const_int -1)))
   (clobber (match_scratch:SI 2 "=a,a"))]
  ""
  {
    if (which_alternative == 0)
      return "dbne\t%0, %1";
    else
      return "#";
  }
  [(set_attr "type" "jump")])

(define_split
  [(set (pc)
        (if_then_else
         (ne (match_operand:SI 0 "memory_operand" "")
             (const_int 1))
         (label_ref (match_operand 1 "" ""))
         (pc)))
   (set (match_dup 0)
        (plus:SI (match_dup 0)
                 (const_int -1)))
   (clobber (match_scratch:SI 2 "=a"))]
  "reload_completed"
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (plus:SI (match_dup 2) (const_int -1)))
   (set (match_dup 0) (match_dup 2))
   (set (pc)
        (if_then_else
         (ne (match_dup 2) (const_int 0))
         (label_ref (match_dup 1)) (pc)))]
   "")

;; Conditional branches.

(define_insn "*cbranchsi4_and"
  [(set (pc)
        (if_then_else (match_operator 0 "equality_operator"
                         [(and:SI
                                (match_operand 1 "ar_register_operand" "a")
                                (match_operand 2 "immediate_operand" "i")
                           )
                          (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  {
    switch (GET_CODE (operands[0]))
    {
      case EQ:
        return "bteq\t%1, #%2, %3";
      case NE:
        return "btne\t%1, #%2, %3";
      default:
        gcc_unreachable ();
    }
  }
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])

(define_insn "*cbranchsi4_zero_extract"
  [(set (pc)
        (if_then_else (match_operator 0 "equality_operator"
                         [(zero_extract:SI
                                (match_operand 1 "ar_register_operand" "a")
                                (match_operand 2 "const_int_operand" "")
                                (match_operand 3 "const_int_operand" "")
                           )
                          (const_int 0)])
                      (label_ref (match_operand 4 "" ""))
                      (pc)))]
  ""
  {
    static char result[64];
    unsigned int val = (1ull << INTVAL(operands[2])) - 1;
    int shift = INTVAL(operands[3]);
    unsigned int mask = val << shift;
    switch (GET_CODE (operands[0]))
    {
      case EQ:
        sprintf (result, "bteq\t%%1, #%d, %%4", mask);
        return result;
      case NE:
        sprintf (result, "btne\t%%1, #%d, %%4", mask);
        return result;
      default:
        gcc_unreachable ();
    }
  }
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])

(define_insn "cbranchsi4"
  [(set (pc)
	(if_then_else
	 (match_operator 0 "comparison_operator"
			 [(match_operand:SI 1 "ar_register_operand" "a,a")
			  (match_operand:SI 2 "nonmemory_operand" "a,i")])
	 (label_ref (match_operand 3 "" ""))
	 (pc)))]
  ""
  {
    int is_imm = which_alternative == 1;
    switch (GET_CODE (operands[0]))
    {
      case EQ:
        return is_imm ? "beq\t%1, #%2, %3" : "beq\t%1, %2, %3";
      case NE:
        return is_imm ? "bne\t%1, #%2, %3" : "bne\t%1, %2, %3";
      case LT:
        return is_imm ? "blt\t%1, #%2, %3" : "blt\t%1, %2, %3";
      case LE:
        return is_imm ? "ble\t%1, #%2, %3" : "ble\t%1, %2, %3";
      case GT:
        return is_imm ? "bgt\t%1, #%2, %3" : "bgt\t%1, %2, %3";
      case GE:
        return is_imm ? "bge\t%1, #%2, %3" : "bge\t%1, %2, %3";
      case LTU:
        return is_imm ? "blo\t%1, #%2, %3" : "blo\t%1, %2, %3";
      case LEU:
      	return is_imm ? "bls\t%1, #%2, %3" : "bls\t%1, %2, %3";
      case GTU:
        return is_imm ? "bhi\t%1, #%2, %3" : "bhi\t%1, %2, %3";
      case GEU:
        return is_imm ? "bhs\t%1, #%2, %3" : "bhs\t%1, %2, %3";
      default:
        gcc_unreachable ();
    }
  }
  [(set_attr "type" "jump")])

;; These transforms code of the form a = !(b < c) to let gcc understand
;; what happens.  Otherwise it outputs code like
;;   mov     r0, #1
;;   xor     r0, #1
;;   and     r0, #1
;; for code like:
;;   bool foo1(unsigned int a, unsigned int b, unsigned int c) {
;;       return a < b &&  b < c;
;;   }
;;
;; Both SI and QI versions are needed; early pass applies the QI version,
;; and it is upgraded to SI in combine when it combines it with the xor.
(define_insn_and_split "*set_cmp_si"
  [(set (match_operand:SI 0 "ar_register_operand" "=&a,&a")
        (match_operator:SI 3 "comparison_operator"
			 [(match_operand:SI 1 "ar_register_operand" "a,a")
			  (match_operand:SI 2 "nonmemory_operand" "a,i")]))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (const_int 1))
   (set (pc)
        (if_then_else
         (match_op_dup 3 [(match_dup 1) (match_dup 2)])
         (label_ref (match_dup 4))
         (pc)))
   (set (match_dup 0) (const_int 0))
   (set (pc) (label_ref (match_dup 4)))
   (match_dup 4)]
  {
    operands[4] = gen_label_rtx ();
  }
)

(define_insn_and_split "*set_cmp_qi"
  [(set (match_operand:QI 0 "ar_register_operand" "=&a,&a")
        (match_operator:QI 3 "comparison_operator"
			 [(match_operand:SI 1 "ar_register_operand" "a,a")
			  (match_operand:SI 2 "nonmemory_operand" "a,i")]))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 0) (const_int 1))
   (set (pc)
        (if_then_else
         (match_op_dup 3 [(match_dup 1) (match_dup 2)])
         (label_ref (match_dup 4))
         (pc)))
   (set (match_dup 0) (const_int 0))
   (set (pc) (label_ref (match_dup 4)))
   (match_dup 4)]
  {
    operands[4] = gen_label_rtx ();
  }
)


;; Setting a register from a comparison.


;; Conditional moves.


;; Floating-point comparisons.


;; Unconditional branches.

(define_insn "jump"
  [(set (pc)
        (label_ref (match_operand 0 "" "")))]
  ""
  "br\t%l0"
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])

(define_insn "indirect_jump"
  [(set (pc)
        (match_operand 0 "ar_register_operand" "b"))]
  ""
  "jmp\t%0"
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])


;; Function calls.

(define_expand "sym_PLT"
  [(const (unspec [(match_operand:SI 0 "" "")] UNSPEC_PLT))]
  ""
  "")

(define_expand "call"
  [(parallel[(call (match_operand 0 "memory_operand" "")
                   (match_operand 1 "" ""))
             (clobber (reg:SI LR_REGNUM))])]
  ""
{
  rtx addr = XEXP (operands[0], 0);
  if (flag_pic && GET_CODE (addr) == SYMBOL_REF
      && (!SYMBOL_REF_LOCAL_P (addr) || SYMBOL_REF_EXTERNAL_P (addr)))
    addr = gen_sym_PLT (addr);
  if (!call_insn_operand (addr, VOIDmode))
    XEXP (operands[0], 0) = copy_to_mode_reg (Pmode, addr);
})

(define_insn "call_internal"
  [(call (mem (match_operand:SI 0 "call_insn_operand" "nir"))
         (match_operand 1 "" "i"))
   (clobber (reg:SI LR_REGNUM))]
  ""
{
  return rasc_emit_call (0, operands,
                         find_reg_note (insn, REG_NORETURN, NULL_RTX) != 0);
}
  [(set_attr "type"     "call")
   (set_attr "mode"     "none")])

(define_expand "call_value"
  [(parallel[(set (match_operand 0 "ar_register_operand" "")
                (call (match_operand 1 "memory_operand" "")
                      (match_operand 2 "" "")))
             (clobber (reg:SI LR_REGNUM))])]
  ""
{
  rtx addr = XEXP (operands[1], 0);
  if (flag_pic && GET_CODE (addr) == SYMBOL_REF
      && (!SYMBOL_REF_LOCAL_P (addr) || SYMBOL_REF_EXTERNAL_P (addr)))
    addr = gen_sym_PLT (addr);
  if (!call_insn_operand (addr, VOIDmode))
    XEXP (operands[1], 0) = copy_to_mode_reg (Pmode, addr);
})

(define_insn "call_value_internal"
   [(set (match_operand 0 "ar_register_operand" "=a")
         (call (mem (match_operand:SI 1 "call_insn_operand" "nir"))
               (match_operand 2 "" "i")))
    (clobber (reg:SI LR_REGNUM))]
  ""
{
  return rasc_emit_call (1, operands,
                         find_reg_note (insn, REG_NORETURN, NULL_RTX) != 0);
}
  [(set_attr "type"     "call")
   (set_attr "mode"     "none")])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
	      (return)])]
  ""
  ""
)

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "" "")
                   (call (match_operand 1 "" "")
                         (match_operand 2 "" "")))
              (return)])]
  ""
  ""
)

(define_insn "*sibcall_insn"
 [(call (mem:SI (match_operand:SI 0 "" ""))
	(match_operand 1 "" ""))
  (return)]
  "GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  return \"br\t%0\";
  "
  [(set_attr "type" "call")]
)

(define_insn "*sibcall_value_insn"
 [(set (match_operand 0 "" "")
       (call (mem:SI (match_operand:SI 1 "" ""))
	     (match_operand 2 "" "")))
  (return)]
  "GET_CODE (operands[1]) == SYMBOL_REF"
  "*
  return \"br\t%1\";
  "
  [(set_attr "type" "call")]
)

(define_expand "return"
  [(return)]
  "rasc_return_allowed()"
{
})

(define_insn "return_internal"
  [(return)]
  ""
{
  return rasc_emit_return_internal ();
}
  [(set_attr "type"     "jump")
   (set_attr "mode"     "none")])

(define_insn "push"
  [(set (reg:SI SP_REGNUM)
        (unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "n")]
                            UNSPECV_PUSH))]
  ""
  "push\t%r0-lr"
  [(set_attr "type"     "store")
   (set_attr "mode"     "SI")])

(define_insn "pop"
  [(set (reg:SI SP_REGNUM)
        (unspec_volatile:SI [(match_operand:SI 0 "const_int_operand" "n")]
                            UNSPECV_POP))]
  ""
  "pop\t%r0-lr"
  [(set_attr "type"     "load")
   (set_attr "mode"     "SI")])


;; Miscellaneous instructions.

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  rasc_expand_prologue();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  rasc_expand_epilogue(false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
{
  rasc_expand_epilogue(true);
  DONE;
})

(define_insn "nop"
  [(const_int 0)]
  ""
{
  return "nop";
}
  [(set_attr "type"     "nop")
   (set_attr "mode"     "none")])
