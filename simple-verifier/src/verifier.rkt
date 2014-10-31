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
  (define f1parameters (extract-parameters f1))
  (define f2parameters (extract-parameters f2))
  (define f1prefix-parameters (prefix-parameters f1parameters "f1"))
  (define f2prefix-parameters (prefix-parameters f2parameters "f2"))
  (define s (generate-z3-input f1 f2 f1parameters f2parameters f1prefix-parameters f2prefix-parameters))
  ;(printf (~a s))
  (define sr (solve s))
  (if (eq? sr #f) 'EQUIVALENT (map (lambda (a) (hash-ref sr a)) f1prefix-parameters)))

(define expression-type-hash
  (make-hash '((= . Bool) (bvule . Bool) (bvult . Bool) (bvuge . Bool) (bvugt . Bool) 
               (bvsle . Bool) (bvslt . Bool) (bvsge . Bool) (bvsgt . Bool)
               (bvneg . (_ BitVec 32)) (bvadd . (_ BitVec 32)) (bvmul . (_ BitVec 32))
               (bvnot . (_ BitVec 32)) (bvdiv . (_ BitVec 32)) (bvsrem . (_ BitVec 32))
               (bvshl . (_ BitVec 32)) (bvlshr . (_ BitVec 32)) (bvashr . (_ BitVec 32))
               (bvsub . (_ BitVec 32)) (bvor . (_ BitVec 32)) (bvand . (_ BitVec 32)) (bvxor . (_ BitVec 32)))))

(define (generate-z3-input f1 f2 f1parameters f2parameters f1prefix-parameters f2prefix-parameters)
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
  (define parameters-hash (make-hash (map (lambda (b) (cons b (string->symbol (string-append prefix (~a b) )))) parameters)))
  (define variable-fun-name-hash (make-hash))
  (define fun-type-hash (make-hash))
  (map (lambda (b) 
         (let* [(terms (syntax-e b))
                (head (syntax-e (car terms)))
                (body (cdr terms))]
         (cond 
           [(eq? head 'define) (process-define-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)]
           [(eq? head 'return) (process-return-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)]
           [(eq? head 'set!) (process-set-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)]
           [else (error (string-append "unknown statement head:" (~a head)))]
           )))
       statements)
  )

;return: (type, list of symbols )
(define (declare-expression parameters prefix stmt parameters-hash variable-fun-name-hash fun-type-hash) 
  (begin
  (define stmt-content (syntax-e stmt))
  ;(printf (string-append "currently expression: " (~a stmt-content)))
  (cond  
    [(symbol? stmt-content) (cond [(eq? stmt-content 'true) (list 'Bool 'true) ]
                          [(eq? stmt-content 'false) (list 'Bool 'false) ]
                          [else (let* [(parameter-name (hash-ref parameters-hash stmt-content ""))
                                      (function-name (hash-ref variable-fun-name-hash (~a stmt-content) ""))
                                      (function-call-signature (cons function-name (prefix-parameters parameters prefix)))]
                                    (begin ;(printf (string-append "parameter-name: " (~a parameter-name)))
                                           ;(printf (string-append "function-name: " (~a function-name)))
                                           ;(printf (string-append "function-call-signature: " (~a function-call-signature)))
                                         (if (symbol? parameter-name)
                                    (list '(_ BitVec 32) parameter-name)
                                    (begin ;(printf (string-append "fun-name: " (~a function-name) " fun-type: " (~a (hash-ref fun-type-hash function-name "")) "\n"))
                                           ;(printf (string-append "fun-type-hash: " (~a fun-type-hash) "\n"))
                                           (list (hash-ref fun-type-hash function-name "") function-call-signature)))
                                         ))
                                ;(car (hash-ref parameters-hash stmt-content function-call))
                                ])] ; id or true or false
    [(number? stmt-content) (if (< stmt-content 0)
                              (list '(_ BitVec 32) (list 'bvneg (list '_ (string->symbol (string-append "bv" (~a (- stmt-content)))) '32))); <0
                              (list '(_ BitVec 32) (list '_ (string->symbol (string-append "bv" (~a stmt-content))) '32)); >=0
                             )] ; const
    [else (let* [(head (syntax->datum (car stmt-content)))
                 (sub-expressions (map (lambda (b) 
                                         (begin 
                                           ;(printf (string-append "exp: " (~a (syntax->datum b)) " we get: " (~a (declare-expression parameters prefix b parameters-hash variable-fun-name-hash fun-type-hash)) "\n"))
                                                (declare-expression parameters prefix b parameters-hash variable-fun-name-hash fun-type-hash))) (cdr stmt-content)))]
          (if (eq? head 'if )
            (begin ;(printf (string-append "if subexp: " (~a sub-expressions) "\n"))
                   (list (car (car (cdr sub-expressions))) (cons 'ite (sub-expressions-to-symbol-list sub-expressions)))) ; if expression
             (begin 
                ;(printf (string-append "sub-expressions: " (~a sub-expressions)))
 (list (hash-ref expression-type-hash head) (cons head (sub-expressions-to-symbol-list sub-expressions)))); others
            ))]
  )))

(define (sub-expressions-to-symbol-list sub-expressions)
  (map (lambda(a)(car (cdr a))) sub-expressions))

;list of new symbols
(define (prefix-parameters parameters prefix)
  (map (lambda (b) (string->symbol (string-append prefix (~a b) )) ) parameters))

(define (process-return-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)
  (define fun-name (string->symbol (string-append prefix "return")))
  (define expression-declared (declare-expression parameters prefix (car body) parameters-hash variable-fun-name-hash fun-type-hash))
  (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) (car expression-declared)
    (car (cdr expression-declared))))

(define (process-define-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)
  (define variable-name (~a (syntax-e (car body))))
  (define fun-name (string->symbol (string-append prefix variable-name)))
  (begin
    (hash-set! variable-fun-name-hash variable-name fun-name)
    (define expression-declared (declare-expression parameters prefix (car (cdr body)) parameters-hash variable-fun-name-hash fun-type-hash))
    (hash-set! fun-type-hash fun-name (car expression-declared))
    (list 'define-fun fun-name (parameters-as-fun-parameters parameters prefix) (car expression-declared)
      (car (cdr expression-declared)))))

(define (process-set-statement parameters prefix body parameters-hash variable-fun-name-hash fun-type-hash)
  (define variable-name (~a (syntax-e (car body))))
  (define variable-old-fun-name (hash-ref variable-fun-name-hash variable-name))
  (define variable-new-fun-name (string->symbol (string-append prefix (symbol->string variable-old-fun-name))))
  (begin
    (hash-set! variable-fun-name-hash variable-name variable-new-fun-name)
    (define expression-declared (declare-expression parameters prefix (car (cdr body)) parameters-hash variable-fun-name-hash fun-type-hash))
    (hash-set! fun-type-hash variable-new-fun-name (car expression-declared))
    (list 'define-fun variable-new-fun-name (parameters-as-fun-parameters parameters prefix) (car expression-declared)
      (car (cdr expression-declared)))))

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

