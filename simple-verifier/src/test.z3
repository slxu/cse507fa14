(set-logic QF_BV)

(declare-const p1x1 (_ BitVec 32))
(declare-const p1y1 (_ BitVec 32))
(declare-const p1x2 (_ BitVec 32))
(declare-const p1y2 (_ BitVec 32))

(declare-const p2x1 (_ BitVec 32))
(declare-const p2y1 (_ BitVec 32))
(declare-const p2x2 (_ BitVec 32))
(declare-const p2y2 (_ BitVec 32))


(define-fun p1return ((p1x1 (_ BitVec 32)) 
                      (p1y1 (_ BitVec 32))
                      (p1x2 (_ BitVec 32))
                      (p1y2 (_ BitVec 32))) (_ BitVec 32) 
  (bvmul (bvadd p1x1 p1y1) (bvadd p1x2 p1y2))
)

(define-fun p2u1 ((p2x1 (_ BitVec 32)) 
                      (p2y1 (_ BitVec 32))
                      (p2x2 (_ BitVec 32))
                      (p2y2 (_ BitVec 32))) (_ BitVec 32) 
  (bvadd p2x1 p2y1)
)

(define-fun p2u2 ((p2x1 (_ BitVec 32)) 
                      (p2y1 (_ BitVec 32))
                      (p2x2 (_ BitVec 32))
                      (p2y2 (_ BitVec 32))) (_ BitVec 32) 
  (bvadd p2x2 p2y2)
)

(define-fun p2return ((p2x1 (_ BitVec 32)) 
                      (p2y1 (_ BitVec 32))
                      (p2x2 (_ BitVec 32))
                      (p2y2 (_ BitVec 32))) (_ BitVec 32) 
  (bvmul (p2u1 p2x1 p2y1 p2x2 p2y2) (p2u2 p2x1 p2y1 p2x2 p2y2))
)

(assert 
  (not 
    (=> 
      (and 
        (= p1x1 p2x1)
        (= p1x2 p2x2)
        (= p1y1 p2y1)
        (= p1y2 p2y2))
      (= 
        (p1return p1x1 p1y1 p1x2 p1y2)
        (p2return p2x1 p2y1 p2x2 p2y2)))))
(check-sat)
