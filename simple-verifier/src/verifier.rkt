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
  (define f1prefix-parameters (prefix-parameters f1parameters "f1"))
  (define f2prefix-parameters (prefix-parameters f2parameters "f2"))
  (append 
     (declare-parameters f1parameters "f1")
     (declare-parameters f2parameters "f2")
     (declare-functions f1 f1parameters "f1")
     (declare-functions f2 f2parameters "f2")
     (list (list 'assert (list 'not (list '=> (equals-parameters f1prefix-parameters f2prefix-parameters) (equals-formulas f1prefix-parameters f2prefix-parameters))))))
  )

(define (equals-parameters f1prefix-parameters f2prefix-parameters)
  (cons 'and (map (lambda (a b) (list '= a b )) f1prefix-parameters f2prefix-parameters)))

(define (equals-formulas f1prefix-parameters f2prefix-parameters)
  (list '= (cons 'f1return f1prefix-parameters ) (cons 'f2return f2prefix-parameters)))

(define (declare-functions frgmt parameters prefix)
  (define statements (extract-statements frgmt))
  (define parameters-hash (make-hash (map (lambda (b) (list b (string->symbol (string-append prefix (~a b) )))) parameters)))
  (define variable-fun-name-hash (make-hash))
  (map (lambda (b) 
         (let* [(terms (syntax-e b))
                (head (syntax-e (car terms)))
                (body (cdr terms))]
         (cond 
           [(eq? head 'define) (process-define-statement parameters prefix body parameters-hash variable-fun-name-hash)]
           [(eq? head 'return) (process-return-statement parameters prefix body parameters-hash variable-fun-name-hash)]
           [(eq? head 'set!) (process-set-statement parameters prefix body parameters-hash variable-fun-name-hash)]
           [else (error (string-append "unknown statement head:" (~a head)))]
           )))
       statements)
  )

(define (declare-expression parameters prefix stmt parameters-hash variable-fun-name-hash) 
  (begin
  (define stmt-content (syntax-e stmt))
  (cond  
    [(symbol? stmt-content) (cond [(eq? stmt-content 'true) 'true ]
                          [(eq? stmt-content 'false) 'false ]
                          [else (car (hash-ref parameters-hash stmt-content (list (cons (hash-ref variable-fun-name-hash (~a stmt-content) "") 
                                                                                                     (prefix-parameters parameters prefix))) ))])] ; id or true or false
    [(number? stmt-content) (list '_ (string->symbol (string-append "bv" (~a stmt-content))) '32)] ; const
    [else (let [(head (syntax->datum (car stmt-content)))]
            (cons (if (eq? head 'if ) 'ite  head )  (map (lambda (b) (declare-expression parameters prefix b parameters-hash variable-fun-name-hash)) (cdr stmt-content))))]
  )))

;list of new symbols
(define (prefix-parameters parameters prefix)
  (map (lambda (b) (string->symbol (string-append prefix (~a b) )) ) parameters))

(define (process-return-statement parameters prefix body parameters-hash variable-fun-name-hash)
  (define fun-name (string->symbol (string-append prefix "return")))
  (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) '(_ BitVec 32)
    (declare-expression parameters prefix (car body) parameters-hash variable-fun-name-hash)))

(define (process-define-statement parameters prefix body parameters-hash variable-fun-name-hash)
  (define variable-name (~a (syntax-e (car body))))
  (define fun-name (string->symbol (string-append prefix variable-name)))
  (begin
    (hash-set! variable-fun-name-hash variable-name fun-name)
    (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) '(_ BitVec 32)
      (declare-expression parameters prefix (car (cdr body)) parameters-hash variable-fun-name-hash) )))

(define (process-set-statement parameters prefix body parameters-hash variable-fun-name-hash)
  (define variable-name (~a (syntax-e (car body))))
  (define variable-old-fun-name (hash-ref variable-fun-name-hash variable-name))
  (define variable-new-fun-name (string->symbol (string-append prefix (symbol->string variable-old-fun-name))))
  (begin
    (hash-set! variable-fun-name-hash variable-name variable-new-fun-name)
    (list 'define-fun variable-new-fun-name (parameters-as-fun-parameters parameters prefix) '(_ BitVec 32)
      (declare-expression parameters prefix (car (cdr body)) parameters-hash variable-fun-name-hash) )))

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

