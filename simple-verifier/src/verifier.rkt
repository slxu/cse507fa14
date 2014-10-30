#lang racket

(require (only-in "bv.rkt" fragment-ast) "solver.rkt")

(provide verify)

; The verifier takes as input two BV fragments
; (see examples.rkt and bv.rkt) and produces one 
; of two outputs: (1) 'EQUIVALENT if the fragments are 
; semantically equivalent; or (2) an input, represented 
; as a list of values, on which the fragments produce 
; different outputs.  The inputs to a fragment are always 
; integer values.
;
; The verifier performs the equivalence check by 
; producing a QF_BV formula that is unsatisfiable iff 
; the fragments are equivalent.  See solver.rkt.
;
; (-> fragment? fragment? (or/c 'EQUIVALENT (listof integer?))))
(define (verify f1 f2)
  (define s (generate-z3-input f1 f2))
  (printf (~a s))
  (solve s))

(define (generate-z3-input f1 f2)
  (define f1parameters (extract-parameters f1))
  (define f2parameters (extract-parameters f2))
  (append 
     (declare-parameters f1parameters "f1")
     (declare-parameters f2parameters "f2")
     (declare-functions f1 f1parameters "f1")
     (declare-functions f2 f2parameters "f2")
     (list (list 'assert (list 'not (list '=> (equals-parameters f1parameters f2parameters) (equals-formulas f1parameters f2parameters))))))
  )

(define (equals-parameters f1parameters f2parameters)
  (cons 'and (map (lambda (a b) (list '= a b )) (prefix-parameters f1parameters "f1") (prefix-parameters f2parameters "f2"))))

(define (equals-formulas f1parameters f2parameters)
  (list '= (cons 'f1return (prefix-parameters f1parameters "f1")) (cons 'f2return (prefix-parameters f2parameters "f2"))))

(define (declare-functions frgmt parameters prefix)
  (define statements (extract-statements frgmt))
  (define parameters-hash (make-hash (map (lambda (b) (list b (string->symbol (string-append prefix (~a b) )))) parameters)))
  (map (lambda (b) 
         (let* [(terms (syntax-e b))
                (head (syntax-e (car terms)))
                (body (cdr terms))]
         (cond 
           [(eq? head 'define) (process-define-statement parameters prefix body parameters-hash)]
           [(eq? head 'return) (process-return-statement parameters prefix body parameters-hash)]
           ;[(eq? head 'set!) "find set!"]
           [else (error (string-append "unknown statement head:" (~a head)))]
           )))
       statements)
  )

(define (declare-expression parameters prefix stmt parameters-hash) 
  (begin
  ;(printf "current exp: ")
  ;(printf (~a stmt))
  (define stmt-content (syntax-e stmt))
  (cond  
    [(symbol? stmt-content) (cond [(eq? stmt-content 'true) 'true ]
                          [(eq? stmt-content 'false) 'false ]
                          [else (car (hash-ref parameters-hash stmt-content (list (cons (string->symbol (string-append prefix (~a stmt-content))) 
                                                                                                     (prefix-parameters parameters prefix))) ))])] ; id or true or false
    [(number? stmt-content) stmt-content] ; const
    [else (cons (syntax->datum (car stmt-content)) (map (lambda (b) (declare-expression parameters prefix b parameters-hash)) (cdr stmt-content)))]
  )))

;list of new symbols
(define (prefix-parameters parameters prefix)
  (map (lambda (b) (string->symbol (string-append prefix (~a b) )) ) parameters))

(define (process-return-statement parameters prefix body parameters-hash)
  (define fun-name (string->symbol (string-append prefix "return")))
  ;(printf "in process return \n")
  (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) '(_ BitVec 32)
    (declare-expression parameters prefix (car body) parameters-hash)))

(define (process-define-statement parameters prefix body parameters-hash)
  (define fun-name (string->symbol (string-append prefix (~a (syntax-e (car body))))))
  ;(printf "in process define: ")
  ;(printf (~a fun-name))
  ;(printf "\n")
  ;(printf (~a body))
  (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) '(_ BitVec 32)
    (declare-expression parameters prefix (car (cdr body)) parameters-hash) ))

(define (parameters-as-fun-parameters parameters prefix)
   (map (lambda (b) (list b '(_ BitVec 32))) (prefix-parameters parameters prefix))
  )

;list of datum
(define (declare-parameters parameters prefix)
  (map (lambda (b) (list 'declare-const b '(_ BitVec 32))) (prefix-parameters parameters prefix))
  )

;return list of symbols
(define (extract-parameters frgmt)
  (define parameters (cdr (syntax-e (car (cdr (syntax-e (datum->syntax #f (fragment-ast frgmt))))))))
  (map (lambda (b) (syntax-e b)) parameters)
  )

;return list of syntax objects
(define (extract-statements frgmt)
  (cdr (cdr (syntax-e (datum->syntax #f (fragment-ast frgmt))))))

