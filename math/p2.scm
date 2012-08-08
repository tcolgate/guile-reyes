
(define vec f64vector)
(define vec-ref f64vector-ref)

(define* (make-p2 #:optional (x 0.0)  (y x)) (vec x y))
(define* (make-p3 #:optional (x 0.0)  (y 0.0) (z 0.0)) (vec x y z))
(define* (make-p4 #:optional (x 0.0)  (y 0.0) (z 0.0) (w 0.0)) (vec x y z w))

(define-syntax op-2
  (syntax-rules ()
		((_ op)  (lambda (v1 v2) (vec (op (vec-ref v1 0)(vec-ref v2 0)) 
			                      (op (vec-ref v1 1)(vec-ref v2 1)))))))

(define v2-+  (op-2 +))
(define v2--  (op-2 -))
(define v2-*  (op-2 *))
(define v2-/  (op-2 /))

(define (v2-mag v1) (+ (* (vec-ref v1 0)(vec-ref v1 0)) 
		       (* (vec-ref v1 1)(vec-ref v1 1))))

(define (v2-sum v1) (+ (vec-ref v1 0)(vec-ref v1 1)))

(define (v2-length v1) (sqrt (v2-mag v1)))

(define (v2-dot v1 v2) (+ (* (vec-ref v1 0)(vec-ref v2 0)) 
		          (* (vec-ref v1 1)(vec-ref v2 1))))

(define (v2-cross v1 v2) (+ (* (vec-ref v1 0)(vec-ref v2 1)) 
		            (* (vec-ref v1 1)(vec-ref v2 0))))

