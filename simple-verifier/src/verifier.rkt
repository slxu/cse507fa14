#lang racket

(require (only-in "bv.rkt" fragment-ast) "solver.rkt")

(provide verify)

; The verifier takes as input two BV fragments
; (see examples.rkt and bv.rkt) and produces one 
; of two outputs: (1) 'EQUIVALENT if the fragments are 
; semantically equivalent; or (2) an input, represented 
; as a list of values, on which the fragments produce 
; different outputs.  Note that the only datatypes supported 
; by BV are booleans and 32-bit integers.  As a result, 
; inputs to a fragment are values of one of these types.
;
; The verifier performs the equivalence check by 
; producing a QF_BV formula that is unsatisfiable iff 
; the fragments are equivalent.  See solver.rkt.
;
; (-> fragment? fragment? (or/c 'EQUIVALENT (listof (or/c integer? boolean?))))
(define (verify f1 f2)
  (error 'verify "not yet implemented!"))

