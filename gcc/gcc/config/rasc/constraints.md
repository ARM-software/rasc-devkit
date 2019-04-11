;; Constraint definitions for RASC.
;;  Copyright (C) 2007 ARM
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

;; Register constraints.

(define_register_constraint "a" "GR_REGS"
 "Registers @code{r0}-@code{r14}.")

(define_register_constraint "b" "HR_REGS"
 "Registers @code{r0}-@code{r13}.")

;; Integer constant constraints.

(define_constraint "I"
 "Integer operand that need fewer number of bytes when represented in
  negated form."
 (and (match_code "const_int")
      (match_test "(((ival >> 24) & 0xff) == 0xff)")))

(define_constraint "J"
  "Integer constant 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "K"
  "Integer constant 2."
  (and (match_code "const_int")
       (match_test "ival == 2")))
