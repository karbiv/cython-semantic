;;; cython-wy.el --- Generated parser support file

;; Copyright (C) 2016 alex

;; Author: alex <alex@server.alex.local>
;; Created: 2016-10-07 05:13:49+0300
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file cython.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;
(require 'wisent-cython)

;;; Declarations
;;
(defconst cython-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("and" . AND)
	 ("api" . API)
	 ("as" . AS)
	 ("assert" . ASSERT)
	 ("async" . ASYNC)
	 ("await" . AWAIT)
	 ("break" . BREAK)
	 ("cdef" . CDEF)
	 ("cimport" . CIMPORT)
	 ("class" . CLASS)
	 ("continue" . CONTINUE)
	 ("cpdef" . CPDEF)
	 ("ctypedef" . CTYPEDEF)
	 ("struct" . STRUCT)
	 ("def" . DEF)
	 ("del" . DEL)
	 ("elif" . ELIF)
	 ("else" . ELSE)
	 ("enum" . ENUM)
	 ("except" . EXCEPT)
	 ("except?" . EXCEPTQ)
	 ("exec" . EXEC)
	 ("extern" . EXTERN)
	 ("False" . FALSE)
	 ("finally" . FINALLY)
	 ("for" . FOR)
	 ("from" . FROM)
	 ("gil" . GIL)
	 ("global" . GLOBAL)
	 ("if" . IF)
	 ("import" . IMPORT)
	 ("in" . IN)
	 ("inline" . INLINE)
	 ("is" . IS)
	 ("lambda" . LAMBDA)
	 ("nogil" . NOGIL)
	 ("not" . NOT)
	 ("None" . NONE)
	 ("nonlocal" . NONLOCAL)
	 ("or" . OR)
	 ("pass" . PASS)
	 ("print" . PRINT)
	 ("property" . PROPERTY)
	 ("public" . PUBLIC)
	 ("raise" . RAISE)
	 ("return" . RETURN)
	 ("struct" . STRUCT)
	 ("True" . TRUE)
	 ("try" . TRY)
	 ("while" . WHILE)
	 ("with" . WITH)
	 ("yield" . YIELD)
	 ("auto" . AUTO)
	 ("_Atomic" . ATOMIC)
	 ("const" . CONST)
	 ("restrict" . RESTRICT)
	 ("volatile" . VOLATILE))
   'nil)
  "Table of language keywords.")

(defconst cython-wy--token-table
  (semantic-lex-make-type-table
   '(("symbol"
	  (NAME))
	 ("number"
	  (NUMBER_LITERAL))
	 ("string"
	  (STRING_LITERAL))
	 ("punctuation"
	  (QUESTION . "?")
	  (RETURNTYPE . "->")
	  (ATEQ . "@=")
	  (AT . "@")
	  (BACKQUOTE . "`")
	  (ELLIPSIS . "...")
	  (ASSIGN . "=")
	  (COMMA . ",")
	  (SEMICOLON . ";")
	  (COLON . ":")
	  (BAR . "|")
	  (TILDE . "~")
	  (PERIOD . ".")
	  (MINUS . "-")
	  (PLUS . "+")
	  (MOD . "%")
	  (DIV . "/")
	  (MULT . "*")
	  (AMP . "&")
	  (GT . ">")
	  (LT . "<")
	  (HAT . "^")
	  (NE . "!=")
	  (LTGT . "<>")
	  (HATEQ . "^=")
	  (OREQ . "|=")
	  (AMPEQ . "&=")
	  (MODEQ . "%=")
	  (DIVEQ . "/=")
	  (MULTEQ . "*=")
	  (MINUSEQ . "-=")
	  (PLUSEQ . "+=")
	  (LE . "<=")
	  (GE . ">=")
	  (EQ . "==")
	  (EXPONENT . "**")
	  (GTGT . ">>")
	  (LTLT . "<<")
	  (DIVDIV . "//")
	  (DIVDIVEQ . "//=")
	  (EXPEQ . "**=")
	  (GTGTEQ . ">>=")
	  (LTLTEQ . "<<="))
	 ("close-paren"
	  (RBRACK . "]")
	  (RBRACE . "}")
	  (RPAREN . ")")
	  (BACKQUOTE . "`"))
	 ("open-paren"
	  (LBRACK . "[")
	  (LBRACE . "{")
	  (LPAREN . "(")
	  (BACKQUOTE . "`"))
	 ("block"
	  (BACKQUOTE_BLOCK . "(BACKQUOTE BACKQUOTE)")
	  (BRACK_BLOCK . "(LBRACK RBRACK)")
	  (BRACE_BLOCK . "(LBRACE RBRACE)")
	  (PAREN_BLOCK . "(LPAREN RPAREN)"))
	 ("indentation"
	  (INDENT_BLOCK . "(INDENT DEDENT)")
	  (DEDENT . "[:INDENT:]")
	  (INDENT . "^\\s-+"))
	 ("newline"
	  (NEWLINE . "\n"))
	 ("charquote"
	  (BACKSLASH . "\\")))
   '(("keyword" :declared t)
	 ("symbol" :declared t)
	 ("number" :declared t)
	 ("punctuation" :declared t)
	 ("block" :declared t)))
  "Table of lexical tokens.")

(defconst cython-wy--parse-table
  (progn
	(eval-when-compile
	  (require 'semantic/wisent/comp))
	(wisent-compile-grammar
	 '((BACKSLASH NEWLINE INDENT DEDENT INDENT_BLOCK PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK BACKQUOTE_BLOCK BACKQUOTE LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LTLTEQ GTGTEQ EXPEQ DIVDIVEQ DIVDIV LTLT GTGT EXPONENT EQ GE LE PLUSEQ MINUSEQ MULTEQ DIVEQ MODEQ AMPEQ OREQ HATEQ LTGT NE HAT LT GT AMP MULT DIV MOD PLUS MINUS PERIOD TILDE BAR COLON SEMICOLON COMMA ASSIGN ELLIPSIS AT ATEQ RETURNTYPE QUESTION STRING_LITERAL NUMBER_LITERAL NAME AND API AS ASSERT ASYNC AWAIT BREAK CDEF CIMPORT CLASS CONTINUE CPDEF CTYPEDEF STRUCT DEF DEL ELIF ELSE ENUM EXCEPT EXCEPTQ EXEC EXTERN FALSE FINALLY FOR FROM GIL GLOBAL IF IMPORT IN INLINE IS LAMBDA NOGIL NOT NONE NONLOCAL OR PASS PRINT PROPERTY PUBLIC RAISE RETURN TRUE TRY WHILE WITH YIELD AUTO ATOMIC CONST RESTRICT VOLATILE)
	   ((nonassoc LOW)
		(nonassoc COMMA SEMICOLON ASSIGN MULT)
		(nonassoc PAREN_BLOCK NAME)
		(nonassoc HIGH))
	   (goal
		((NEWLINE))
		((stmt)))
	   (stmt
		((simple_stmt))
		((compound_stmt)))
	   (simple_stmt
		((small_stmt-list semicolon-opt NEWLINE)))
	   (small_stmt-list
		((small_stmt))
		((small_stmt-list SEMICOLON small_stmt)))
	   (semicolon-opt
		(nil
		 [LOW])
		((SEMICOLON)))
	   (small_stmt
		((expr_stmt))
		((del_stmt))
		((PASS))
		((flow_stmt))
		((import_stmt))
		((global_stmt))
		((nonlocal_stmt))
		((assert_stmt))
		((cdef))
		((cimport_stmt)))
	   (compound_stmt
		((if_stmt))
		((while_stmt))
		((for_stmt))
		((try_stmt))
		((with_stmt))
		((funcdef))
		((classdef))
		((decorated))
		((async_stmt))
		((with_nogil))
		((with_gil))
		((cdef_class))
		((cdef_func))
		((cpdef))
		((cdef_extern))
		((c_struct))
		((cython_property))
		((cdef_enum))
		((cdef_block)))
	   (decorator
		((AT dotted_name arglist NEWLINE)
		 (progn $2))
		((AT dotted_name NEWLINE)
		 (progn $2)))
	   (decorators
		((decorator)
		 (list $1))
		((decorators decorator)
		 (cons $2 $1)))
	   (decorated
		((decorators decorated-item)
		 (cython-decorated $1 $2)))
	   (decorated-item
		((classdef))
		((funcdef))
		((cdef_func))
		((cpdef))
		((ASYNC funcdef)
		 (progn $2)))
	   (funcdef
		((DEF NAME parameters RETURNTYPE test COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $2 $5 $3 :def "def" :hint t))
		  $7))
		((DEF NAME parameters COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $2 nil $3 :def "def"))
		  $5)))
	   (parameters
		((PAREN_BLOCK)
		 (let
			 ((wisent-python-EXPANDING-block t))
		   (semantic-parse-region
			(car $region1)
			(cdr $region1)
			'typed_function_parameters 1))))
	   (typed_function_parameters
		((LPAREN RPAREN)
		 nil)
		((LPAREN)
		 nil)
		((typedargslist-opt))
		((RPAREN)
		 nil))
	   (typedargslist-opt
		(nil)
		((typedargslist)))
	   (typedargslist
		((tfpdef comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil nil)))
		((tfpdef ASSIGN test comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil $3)))
		((EXPONENT tfpdef comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable
		   (concat $1 $2)
		   nil nil)))
		((MULT comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil nil)))
		((MULT tfpdef comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable
		   (concat $1 $2)
		   nil nil))))
	   (comma_rparen
		((COMMA))
		((RPAREN)))
	   (tfpdef
		((NAME NAME)
		 (concat $1 " " $2))
		((NAME COLON test)
		 (concat $1 $2 $3))
		((NAME))
		((cdef_memoryview)))
	   (assign_test-opt
		(nil)
		((ASSIGN test)))
	   (varargslist
		((vfpdef assign_test-opt comma_vfpdef-eq_test-list-opt comma_mult_vfpdef-or-exp_vfpdef))
		((vfpdef assign_test-opt comma_vfpdef-eq_test-list-opt))
		((MULT vfpdef-opt comma_vfpdef-eq_test-list-opt comma_exp_vfpdef-opt))
		((EXPONENT vfpdef)))
	   (comma_mult_vfpdef-or-exp_vfpdef
		((COMMA mult_vfpdef-or-exp_vfpdef))
		((COMMA)))
	   (mult_vfpdef-or-exp_vfpdef
		((MULT vfpdef-opt comma_vfpdef-eq_test-list-opt comma_exp_vfpdef-opt))
		((EXPONENT vfpdef)))
	   (comma_vfpdef-eq_test-list-opt
		(nil
		 [LOW])
		((comma_vfpdef-eq_test-list)
		 [LOW]))
	   (comma_vfpdef-eq_test-list
		((COMMA vfpdef assign_test-opt))
		((comma_vfpdef-eq_test-list COMMA vfpdef assign_test-opt)))
	   (comma_exp_vfpdef-opt
		(nil)
		((COMMA EXPONENT vfpdef)))
	   (vfpdef
		((NAME)))
	   (vfpdef-opt
		(nil)
		((NAME)))
	   (expr_stmt
		((testlist_star_expr augassign yield_expr_or_testlist)
		 (if
			 (and $2
				  (stringp $1)
				  (string-match "^\\(\\sw\\|\\s_\\|\\.\\)+$" $1))
			 (wisent-raw-tag
			  (semantic-tag-new-variable $1 nil nil))
		   (wisent-raw-tag
			(semantic-tag-new-code $1 nil))))
		((testlist_star_expr expr_stmt_rhs_assign-list)
		 (if
			 (and $2
				  (stringp $1)
				  (string-match "^\\(\\sw\\|\\s_\\|\\.\\)+$" $1))
			 (wisent-raw-tag
			  (semantic-tag-new-variable $1 nil nil :assign t))
		   (wisent-raw-tag
			(semantic-tag-new-code $1 nil))))
		((testlist_star_expr)))
	   (expr_stmt_rhs_assign-list
		((expr_stmt_rhs_assign))
		((expr_stmt_rhs_assign-list expr_stmt_rhs_assign)))
	   (expr_stmt_rhs_assign
		((ASSIGN yield_expr_or_testlist_star_expr)))
	   (yield_expr_or_testlist
		((yield_expr))
		((testlist)))
	   (yield_expr_or_testlist_star_expr
		((yield_expr))
		((testlist_star_expr)))
	   (comma-opt
		(nil)
		((COMMA)))
	   (augassign
		((PLUSEQ))
		((MINUSEQ))
		((MULTEQ))
		((ATEQ))
		((DIVEQ))
		((MODEQ))
		((AMPEQ))
		((OREQ))
		((HATEQ))
		((LTLTEQ))
		((GTGTEQ))
		((EXPEQ))
		((DIVDIVEQ)))
	   (del_stmt
		((DEL exprlist)))
	   (flow_stmt
		((BREAK))
		((CONTINUE))
		((return_stmt))
		((raise_stmt))
		((yield_expr)))
	   (return_stmt
		((RETURN testlist))
		((RETURN)))
	   (raise_stmt
		((RAISE test_from_test-opt)))
	   (test_from_test-opt
		(nil)
		((test from_test-opt)))
	   (from_test-opt
		(nil)
		((FROM test)))
	   (import_stmt
		((import_name))
		((import_from)))
	   (import_name
		((IMPORT dotted_as_names)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil))))
	   (import_as_name
		((NAME))
		((NAME AS NAME)))
	   (import_as_name-list
		((import_as_name))
		((import_as_name-list COMMA import_as_name)))
	   (import_as_names
		((import_as_name-list comma-opt)))
	   (import_from
		((FROM from-what IMPORT MULT)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil)))
		((FROM from-what IMPORT PAREN_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil)))
		((FROM from-what IMPORT import_as_names)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil))))
	   (from-what
		((dots dotted_name))
		((dotted_name))
		((dots)))
	   (dots
		((PERIOD))
		((dots PERIOD)))
	   (dotted_as_name
		((dotted_name))
		((dotted_name AS NAME)))
	   (dotted_as_names
		((dotted_as_name)
		 (list $1))
		((dotted_as_names COMMA dotted_as_name)
		 (cons $3 $1)))
	   (global_stmt
		((GLOBAL names)))
	   (names
		((NAME))
		((names COMMA NAME)))
	   (nonlocal_stmt
		((NONLOCAL names)))
	   (assert_stmt
		((ASSERT tests)))
	   (tests
		((test))
		((tests COMMA test)))
	   (async_stmt
		((ASYNC async_stmt_variants)))
	   (async_stmt_variants
		((funcdef))
		((with_stmt))
		((for_stmt)))
	   (if_stmt
		((IF test COLON suite else_colon_suite))
		((IF test COLON suite elif_test_colon_suite else_colon_suite)))
	   (elif_test_colon_suite
		((ELIF test COLON suite))
		((elif_test_colon_suite ELIF test COLON suite)))
	   (else_colon_suite
		(nil)
		((ELSE COLON suite)))
	   (while_stmt
		((WHILE test COLON suite else_colon_suite)))
	   (for_stmt
		((FOR exprlist IN testlist COLON suite else_colon_suite)))
	   (expr-or-star
		((expr))
		((star_expr)))
	   (expr-or-star-list
		((expr-or-star))
		((expr-or-star-list COMMA expr-or-star)))
	   (exprlist
		((expr-or-star-list comma-opt)))
	   (testlist
		((testlist-comma comma-opt)))
	   (testlist-comma
		((test))
		((testlist-comma COMMA test)))
	   (test-or-star_expr
		((test))
		((star_expr)))
	   (testlist_star_expr-list
		((test-or-star_expr))
		((testlist_star_expr-list COMMA test-or-star_expr)))
	   (testlist_star_expr
		((testlist_star_expr-list comma-opt)))
	   (try_stmt
		((TRY COLON suite try_stmt_block)))
	   (try_stmt_block
		((except_clause_colon_suite_list else_colon_suite))
		((except_clause_colon_suite_list else_colon_suite FINALLY COLON suite))
		((FINALLY COLON suite)))
	   (except_clause_colon_suite_list
		((EXCEPT test_as_name COLON suite))
		((except_clause_colon_suite_list EXCEPT test_as_name COLON suite)))
	   (test_as_name
		(nil)
		((test))
		((test AS NAME)))
	   (with_stmt
		((WITH with_item_list COLON suite)))
	   (with_item_list
		((with_item))
		((with_item_list COMMA with_item)))
	   (with_item
		((test as_expr)))
	   (as_expr
		(nil)
		((AS expr)))
	   (suite
		((simple_stmt)
		 (list $1))
		((NEWLINE indented_block)
		 (progn $2)))
	   (indented_block
		((INDENT_BLOCK)
		 (semantic-parse-region
		  (car $region1)
		  (cdr $region1)
		  'indented_block_body 1)))
	   (indented_block_body
		((INDENT)
		 nil)
		((DEDENT)
		 nil)
		((simple_stmt))
		((compound_stmt)))
	   (test
		((or_test IF or_test ELSE test))
		((or_test))
		((lambdef)))
	   (test_nocond
		((or_test))
		((lambdef_nocond)))
	   (lambdef
		((LAMBDA varargslist COLON test))
		((LAMBDA COLON test)))
	   (lambdef_nocond
		((LAMBDA varargslist COLON test_nocond))
		((LAMBDA COLON test_nocond)))
	   (or_test
		((and_test or_and_test_list))
		((and_test)))
	   (or_and_test_list
		((OR and_test))
		((or_and_test_list OR and_test)))
	   (and_test
		((not_test and_not_test_list))
		((not_test)))
	   (and_not_test_list
		((AND not_test))
		((and_not_test_list AND not_test)))
	   (not_test
		((NOT not_test))
		((comparison)))
	   (comparison
		((expr comp_op_expr-list))
		((expr)))
	   (comp_op_expr-list
		((comp_op expr))
		((comp_op_expr-list comp_op expr)))
	   (comp_op
		((LT))
		((GT))
		((EQ))
		((GE))
		((LE))
		((LTGT))
		((NE))
		((IN))
		((NOT IN))
		((IS))
		((IS NOT)))
	   (star_expr
		((MULT expr)))
	   (expr
		((xor bar_xor-list))
		((xor)))
	   (bar_xor-list
		((BAR xor))
		((bar_xor-list BAR xor)))
	   (xor
		((and hat_and-list))
		((and)))
	   (hat_and-list
		((HAT and))
		((hat_and-list HAT and)))
	   (and
		((shift amp_shift-list))
		((shift)))
	   (amp_shift-list
		((AMP shift))
		((amp_shift-list AMP shift)))
	   (shift
		((arith ltlt_or_gtgt_arith-list))
		((arith)))
	   (ltlt_or_gtgt
		((LTLT))
		((GTGT)))
	   (ltlt_or_gtgt_arith-list
		((ltlt_or_gtgt arith))
		((ltlt_or_gtgt_arith-list ltlt_or_gtgt arith)))
	   (arith
		((term))
		((term plus_or_minus_term-list)))
	   (plus_or_minus
		((PLUS))
		((MINUS)))
	   (plus_or_minus_term-list
		((plus_or_minus term))
		((plus_or_minus_term-list plus_or_minus term)))
	   (term
		((factor mult_at_div_mod_divdiv_factor-list))
		((factor)))
	   (mult_at_div_mod_divdiv
		((MULT))
		((AT))
		((DIV))
		((MOD))
		((DIVDIV)))
	   (mult_at_div_mod_divdiv_factor-list
		((mult_at_div_mod_divdiv factor))
		((mult_at_div_mod_divdiv_factor-list mult_at_div_mod_divdiv factor)))
	   (factor
		((plus_minus_tilde factor))
		((power)))
	   (plus_minus_tilde
		((PLUS))
		((MINUS))
		((TILDE)))
	   (power
		((atom_expr EXPONENT factor))
		((atom_expr)))
	   (atom_expr
		((AWAIT atom)
		 (concat $1 $2))
		((AWAIT atom trailer-list)
		 (concat $1 $2 $3))
		((atom))
		((atom trailer-list)
		 (concat $1 $2)))
	   (trailer-list
		((trailer))
		((trailer-list trailer)
		 (concat $1 $2)))
	   (trailer
		((PAREN_BLOCK)
		 nil)
		((BRACK_BLOCK)
		 nil)
		((PERIOD NAME)
		 (concat $1 $2)))
	   (dotted_name
		((NAME))
		((dotted_name PERIOD NAME)
		 (format "%s.%s" $1 $3)))
	   (atom
		((PAREN_BLOCK)
		 nil)
		((BRACK_BLOCK)
		 nil)
		((BRACE_BLOCK))
		((BACKQUOTE_BLOCK))
		((NAME))
		((NUMBER_LITERAL))
		((strings))
		((ELLIPSIS))
		((NONE))
		((TRUE))
		((FALSE)))
	   (strings
		((STRING_LITERAL))
		((strings STRING_LITERAL)
		 (concat $1 $2)))
	   (arguments
		((LPAREN)
		 nil)
		((RPAREN)
		 nil)
		((NAME comp_for-opt comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil nil)))
		((NAME ASSIGN test comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil nil)))
		((EXPONENT NAME comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $2 nil nil)))
		((MULT NAME comma_rparen)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $2 nil nil))))
	   (classdef
		((CLASS NAME arglist-opt COLON suite)
		 (wisent-raw-tag
		  (semantic-tag-new-type $2 $1 $5
								 (cons $3 nil)
								 :def "class"))))
	   (arglist
		((PAREN_BLOCK)
		 (let
			 ((wisent-python-EXPANDING-block t))
		   (semantic-parse-region
			(car $region1)
			(cdr $region1)
			'arguments 1))))
	   (arglist-opt
		(nil)
		((arglist)))
	   (comp_iter
		((comp_for))
		((comp_if)))
	   (comp_iter-opt
		(nil)
		((comp_iter)))
	   (comp_if
		((IF test_nocond comp_iter-opt)))
	   (comp_for-opt
		(nil)
		((comp_for)))
	   (comp_for
		((FOR exprlist IN or_test comp_iter-opt)))
	   (yield_expr
		((YIELD yield_arg))
		((YIELD)))
	   (yield_arg
		((FROM test))
		((testlist)))
	   (cdef
		((CDEF cdef_vars comma-opt)
		 (wisent-raw-tag
		  (semantic-tag "cdef_vars" 'temp :contents
						(nreverse $2)))))
	   (cdef_var
		((NAME NAME)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $2 $1 nil)))
		((NAME NAME ASSIGN test)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $2 $1 $4)))
		((NAME ASSIGN test)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil $3)))
		((NAME)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $1 nil nil)))
		((cdef_memoryview)))
	   (cdef_memoryview
		((NAME BRACK_BLOCK NAME)
		 [LOW]
		 (wisent-raw-tag
		  (semantic-tag-new-variable $3
									 (concat $1 $2)
									 nil)))
		((NAME BRACK_BLOCK NAME ASSIGN test)
		 [LOW]
		 (wisent-raw-tag
		  (semantic-tag-new-variable $3
									 (concat $1 $2)
									 $5))))
	   (cdef_vars
		((cdef_var)
		 (list $1))
		((cdef_vars COMMA cdef_var)
		 (cons $3 $1)))
	   (cdef_block
		((CDEF COLON NEWLINE INDENT_BLOCK)
		 nil))
	   (cdef_func
		((CDEF NAME NAME parameters cdef_except_opt COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $3 $2 $4 :def "cdef"))
		  $7))
		((CDEF NAME parameters cdef_except_opt COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $2 nil $3 :def "cdef"))
		  $6))
		((CDEF INLINE NAME NAME parameters cdef_except_opt COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $4 $3 $5 :def "cdef" :inline t))
		  $8)))
	   (cdef_except_opt
		(nil)
		((EXCEPT MULT))
		((EXCEPT NUMBER_LITERAL))
		((EXCEPTQ NUMBER_LITERAL))
		((EXCEPT PLUS)))
	   (cdef_class
		((CDEF CLASS NAME arglist-opt COLON suite)
		 (cython-reconstitute-class-tag
		  (wisent-raw-tag
		   (semantic-tag-new-type $3 $2 $6
								  (cons $4 nil)
								  :def "cdef class")))))
	   (cython_property
		((PROPERTY NAME COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $2 nil nil))
		  $4)))
	   (cpdef
		((CPDEF NAME parameters COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $2 nil $3 :def "cpdef"))
		  $5))
		((CPDEF NAME NAME parameters COLON suite)
		 (cython-reconstitute-function-tag
		  (wisent-raw-tag
		   (semantic-tag-new-function $3 $2 $4 :def "cpdef"))
		  $6)))
	   (cimport_stmt
		((cimport_name))
		((cimport_from)))
	   (cimport_name
		((CIMPORT dotted_as_names)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil :def "cimport"))))
	   (cimport_from
		((FROM from-what CIMPORT MULT)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil :def "cimport")))
		((FROM from-what CIMPORT PAREN_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil :def "cimport")))
		((FROM from-what CIMPORT import_as_names)
		 (wisent-raw-tag
		  (semantic-tag-new-include $2 nil :def "cimport"))))
	   (cdef_enum
		((CDEF ENUM NAME COLON NEWLINE INDENT_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $3 $2 nil))))
	   (with_nogil
		((WITH NOGIL COLON NEWLINE INDENT_BLOCK)
		 nil))
	   (with_gil
		((WITH GIL COLON NEWLINE INDENT_BLOCK)
		 nil))
	   (c_struct
		((STRUCT NAME COLON NEWLINE INDENT_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag-new-variable $2
									 (concat $1 " " $2)
									 nil))))
	   (cdef_extern
		((CDEF EXTERN FROM STRING_LITERAL COLON NEWLINE INDENT_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag
		   (substring $4 1 -1)
		   $nterm)))
		((CDEF EXTERN FROM MULT COLON NEWLINE INDENT_BLOCK)
		 (wisent-raw-tag
		  (semantic-tag $4 $nterm)))))
	 '(goal indented_block typed_function_parameters arguments indented_block_body)))
  "Parser table.")

(defun cython-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
		semantic--parse-table cython-wy--parse-table
		semantic-debug-parser-source "cython.wy"
		semantic-flex-keywords-obarray cython-wy--keyword-table
		semantic-lex-types-obarray cython-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
			'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-block-type-analyzer cython-wy--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
	 ("{" LBRACE BRACE_BLOCK)
	 ("[" LBRACK BRACK_BLOCK)
	 ("`" BACKQUOTE BACKQUOTE_BLOCK))
	(")" RPAREN)
	("}" RBRACE)
	("]" RBRACK)
	("`" BACKQUOTE))
  )

(define-lex-string-type-analyzer cython-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((QUESTION . "?")
	(RETURNTYPE . "->")
	(ATEQ . "@=")
	(AT . "@")
	(BACKQUOTE . "`")
	(ELLIPSIS . "...")
	(ASSIGN . "=")
	(COMMA . ",")
	(SEMICOLON . ";")
	(COLON . ":")
	(BAR . "|")
	(TILDE . "~")
	(PERIOD . ".")
	(MINUS . "-")
	(PLUS . "+")
	(MOD . "%")
	(DIV . "/")
	(MULT . "*")
	(AMP . "&")
	(GT . ">")
	(LT . "<")
	(HAT . "^")
	(NE . "!=")
	(LTGT . "<>")
	(HATEQ . "^=")
	(OREQ . "|=")
	(AMPEQ . "&=")
	(MODEQ . "%=")
	(DIVEQ . "/=")
	(MULTEQ . "*=")
	(MINUSEQ . "-=")
	(PLUSEQ . "+=")
	(LE . "<=")
	(GE . ">=")
	(EQ . "==")
	(EXPONENT . "**")
	(GTGT . ">>")
	(LTLT . "<<")
	(DIVDIV . "//")
	(DIVDIVEQ . "//=")
	(EXPEQ . "**=")
	(GTGTEQ . ">>=")
	(LTLTEQ . "<<="))
  'punctuation)

(define-lex-regex-type-analyzer cython-wy--<symbol>-regexp-analyzer
  "regexp analyzer for <symbol> tokens."
  "\\(\\sw\\|\\s_\\)+"
  nil
  'NAME)

(define-lex-regex-type-analyzer cython-wy--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'NUMBER_LITERAL)

(define-lex-keyword-type-analyzer cython-wy--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;

(provide 'cython-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; cython-wy.el ends here
