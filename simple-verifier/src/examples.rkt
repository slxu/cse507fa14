#lang racket

(require "bv.rkt" "verifier.rkt" "solver.rkt")

; This is how we construct BV fragments:
(define-fragment (P1 x1 y1 x2 y2)
  (return (bvmul (bvadd x1 y1) (bvadd x2 y2))))

(define-fragment (P2  x1 y1 x2 y2)
  (define u1 (bvadd x1 y1))
  (define u2 (bvadd x2 y2))
  (return (bvmul u1 u2)))

; P1 is now bound to an object that contains the 
; AST of the above fragment, as well as procedure 
; to which the fragment evaluates.  
(printf "This is the AST of the P1 fragment:\n ~a\n" (fragment-ast P1))
(printf "The result of applying the P1 fragment to 3, 4, 5, 6: ~a\n" (P1 3 4 5 6))

; You can use Racket's pattern matching to deconstruct ASTs:
(define (fragment-info P)
  (match (fragment-ast P)
    [(list 'define-fragment (list id arg ...) stmt ... ret)
     (printf "Fragment info:\n")
     (printf "  Name: ~a\n" id)
     (printf "  Formal parameters: ~a\n" arg)
     (printf "  Statements: ~a\n" stmt)
     (printf "  Return: ~a\n" ret)]))

(fragment-info P1)
(fragment-info P2)

; Uncomment the following code to apply your verifier to P1 and P2:
; (verify P1 P2)

; If you didn't put a z3 binary in the ../bin directory, you can 
; set the path to it like this:
; (z3 "absolute path to z3 binary, e.g. /Users/user/solvers/z3")

; The solve procedure takes as input a list of s-expressions in SMT-LIB
; syntax.  The list should omit the (set-logic ...), (check-sat), and (get-model) commands. 
; These are added to the formula by solve procedure.  If the formula is 
; unsat, the output is #f.  Otherwise, the output is a hashmap from constant names 
; to values.
(solve
 '((declare-const a Bool)
   (declare-const b (_ BitVec 32))
   (declare-const c (_ BitVec 32))
   (assert (= (ite a b c) b))))

(solve
 '((declare-const a Bool)
   (assert (and a (not a)))))