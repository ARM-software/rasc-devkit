;; Predicate definitions for RASC.
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

;; True for EQ & NE
(define_special_predicate "equality_operator"
  (match_code "eq,ne"))

(define_predicate "ar_register_operand"
  (match_code "reg,subreg")
{
  if (! register_operand(op,mode))
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (REGNO (op) >= FIRST_PSEUDO_REGISTER
          || REGNO_REG_CLASS (REGNO (op)) == GR_REGS
          || REGNO_REG_CLASS (REGNO (op)) == SP_REGS
          || REGNO_REG_CLASS (REGNO (op)) == AR_REGS);
})

(define_predicate "call_insn_operand"
  (match_code "const_int,const,symbol_ref,reg")
{
  if ((GET_CODE (op) == REG)
      && (op != arg_pointer_rtx)
      && ((REGNO (op) < FRAME_POINTER_REGNUM)
          || (REGNO (op) > LAST_VIRTUAL_REGISTER)))
    return TRUE;

  if (CONSTANT_ADDRESS_P (op))
    {
      /* Direct calls only allowed to static functions with PIC.  */
      if (flag_pic)
        {
          tree callee, callee_sec, caller_sec;

          if (GET_CODE (op) != SYMBOL_REF
              || !SYMBOL_REF_LOCAL_P (op) || SYMBOL_REF_EXTERNAL_P (op))
            return FALSE;

          /* Don't attempt a direct call if the callee is known to be in
             a different section, since there's a good chance it will be
             out of range.  */

          if (flag_function_sections
              || DECL_ONE_ONLY (current_function_decl))
            return FALSE;
          caller_sec = DECL_SECTION_NAME (current_function_decl);
          callee = SYMBOL_REF_DECL (op);
          if (callee)
            {
              if (DECL_ONE_ONLY (callee))
                return FALSE;
              callee_sec = DECL_SECTION_NAME (callee);
              if (((caller_sec == NULL_TREE) ^ (callee_sec == NULL_TREE))
                  || (caller_sec != NULL_TREE
                      && strcmp (TREE_STRING_POINTER (caller_sec),
                                 TREE_STRING_POINTER (callee_sec)) != 0))
                return FALSE;
            }
          else if (caller_sec != NULL_TREE)
            return FALSE;
        }
      return TRUE;
    }

  return FALSE;
})
