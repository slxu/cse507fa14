#lang racket

(require "solver.rkt" "graph.rkt")

(provide
  k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
  valid-coloring? ; (-> graph/c coloring/c boolean?)
  )

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (for*/and ([(e n) (in-indexed graph)] [child e])
            (not (= (color-ref coloring n) (color-ref coloring child)))))

; use vertex_index*k+1, ..., vertex_index*k+c .. vertex_index*k+k  to denote
; that vertex of vertex_index has color c
(define (vertex-color-to-variable v c k)
  (+ (+ (* v  k)  c ) 1)
  )

(define (variable-to-vertex var k)
  (quotient (- var  1)  k)
  )

(define (variable-to-color var k)
  (- (- var  1 )  (* k  ( variable-to-vertex var k)))
  )

(define  (sesc v w c k) (list (- (vertex-color-to-variable v c k)) (- (vertex-color-to-variable w c k))))

(define (single-edge-single-color-encoding v w c k)
  (if (= (+ c 1) k)
    (list (sesc v w c k))
    (cons (sesc v w c k) (single-edge-single-color-encoding v w (+ c  1) k))
    )
  )

(define (single-edge-encoding v w k)
  (single-edge-single-color-encoding v w 0 k)
  )

(define (adjacent-vertices-have-different-color-encoding graph k)
  (foldl (lambda (vertex-idx le) 
           ( append  le (foldl (lambda (child le) 
                                 (append le (single-edge-encoding vertex-idx child k))
                                 )
                               '()
                               (sequence-ref graph vertex-idx))
                     )
           )
         '() 
         (build-list (sequence-length graph) values)
         )
  )

(define (vertex-has-one-color-encoding graph k)
  (map 
    (lambda (vertex-idx)
      (foldl (lambda (c l)
               (append l (list ( vertex-color-to-variable vertex-idx c k)))
               )  '()  (build-list k values)
             )
      )
    (build-list (sequence-length graph) values)
    )
  )

(define (k-coloring-encoding graph k)
  (append (vertex-has-one-color-encoding graph k) 
          (adjacent-vertices-have-different-color-encoding graph k) )
  )


(define (get-colorings solve-result k)
  (define positive-ones (filter positive? solve-result))
  (list->vector (map (lambda (x) 
                       (variable-to-color x k ))
                     (sort (filter (lambda (e) (
                                                for/and ([ c (in-range (variable-to-color e k))]) (not (member (vertex-color-to-variable (variable-to-vertex e k) c k) positive-ones))
                                                ))
                                   positive-ones
                                   ) <)
                     ))
  )


; Returns a coloring/c if the given graph can 
; be colored with k colors.  Otherwise returns #f.
; The procedure accepts an optional solver% arugment.
; If not specified, the solver defaults to lingeling.
(define (k-coloring graph k [solver (lingeling)])
  ; encode each vertex must have a coloring
  (define do-solve (solve (k-coloring-encoding graph k)))
  (if do-solve
    (get-colorings do-solve k) ; k-colorable
    #f
    )
  )
