(define-module (maths vector)
	       #:export (
			 make-v2 v2-+ v2-- v2-* v2-/ v2-mag v2-sum
			 v2-length v2-norm v2-dot v2-cross v2-dist

			 make-v3 v3-+ v3-- v3-* v3-/ v3-mag v3-sum
			 v3-length v3-norm v3-dot v3-cross v3-dist
			 v3-faceforward

			 make-v4 v4-+ v4-- v4-* v4-/ v4-mag v4-sum
			 v4-length v4-norm v4-dist
			 ))

(define vec f64vector)
(define vec-ref f64vector-ref)

(define* (make-v2 #:optional (x 0.0)  (y x)) 
  (vec x y))

(define-syntax op-2
  (syntax-rules ()
		((_ op) (lambda (v1 v2)
			   (vec (op (vec-ref v1 0)(vec-ref v2 0)) 
				(op (vec-ref v1 1)(vec-ref v2 1)))))))

(define v2-+  (op-2 +))
(define v2--  (op-2 -))
(define v2-*  (op-2 *))
(define v2-/  (op-2 /))

(define (v2-mag v1) 
  (+ (* (vec-ref v1 0)(vec-ref v1 0)) 
     (* (vec-ref v1 1)(vec-ref v1 1))))

(define (v2-sum v1) 
  (+ (vec-ref v1 0)(vec-ref v1 1)))

(define (v2-length v1)
  (sqrt (v2-mag v1)))

(define (v2-norm v1) 
  (let ((len (v2-length v1)))
    (if (> len 0)
      (vec (/ (vec-ref v1 0) len) (/ (vec-ref v1 1) len))
      v1)))

(define (v2-dot v1 v2) 
  (+ (* (vec-ref v1 0)(vec-ref v2 0)) 
     (* (vec-ref v1 1)(vec-ref v2 1))))

(define (v2-cross v1 v2)
  (+ (* (vec-ref v1 0)(vec-ref v2 1)) 
     (* (vec-ref v1 1)(vec-ref v2 0))))

(define (v2-dist v1 v2) 
  (v2-length (v2-- v1 v2)))

(define* (make-v3 #:optional (x 0.0)  (y 0.0) (z 0.0)) 
  (vec x y z))

(define-syntax op-3
  (syntax-rules ()
		((_ op) (lambda (v1 v2) 
			   (vec (op (vec-ref v1 0)(vec-ref v2 0)) 
				(op (vec-ref v1 1)(vec-ref v2 1)) 
				(op (vec-ref v1 2)(vec-ref v2 2)))))))

(define v3-+  (op-3 +))
(define v3--  (op-3 -))
(define v3-*  (op-3 *))
(define v3-/  (op-3 /))

(define (v3-mag v1) 
  (+ (* (vec-ref v1 0)(vec-ref v1 0)) 
     (* (vec-ref v1 1)(vec-ref v1 1)) 
     (* (vec-ref v1 2)(vec-ref v1 2))))

(define (v3-sum v1) 
  (+ (vec-ref v1 0)(vec-ref v1 1)(vec-ref v1 2)))

(define (v3-length v1) 
  (sqrt (v3-mag v1)))

(define (v3-norm v1) 
  (let ((len (v3-length v1)))
    (if (> len 0)
      (vec 
	(/ (vec-ref v1 0) len) 
	(/ (vec-ref v1 1) len)
	(/ (vec-ref v1 2) len))
      v1)))

(define (v3-dot v1 v2) 
  (+ (* (vec-ref v1 0)(vec-ref v2 0)) 
     (* (vec-ref v1 1)(vec-ref v2 1))
     (* (vec-ref v1 2)(vec-ref v2 2))))

(define (v3-cross v1 v2) 
  (vec 
    (- (* (vec-ref v1 1)(vec-ref v2 2)) 
       (* (vec-ref v1 2)(vec-ref v2 1))) 
    (- (* (vec-ref v1 2)(vec-ref v2 0)) 
       (* (vec-ref v1 0)(vec-ref v2 2)))
    (- (* (vec-ref v1 0)(vec-ref v2 1)) 
       (* (vec-ref v1 1)(vec-ref v2 0)))))

(define (v3-faceforward v1 v2)
  (if (> (v3-dot v1 v2))
    (vec (v3-* v1 (vec -1.0 -1.0 -1.0)))
    v1))

(define (v3-dist v1 v2) 
  (v3-length (v2-- v1 v2)))

(define* (make-v4 #:optional (x 0.0)  (y 0.0) (z 0.0) (w 0.0)) 
  (vec x y z w))

(define-syntax op-4
  (syntax-rules ()
		((_ op) (lambda (v1 v2)
			   (vec (op (vec-ref v1 0)(vec-ref v2 0)) 
				(op (vec-ref v1 1)(vec-ref v2 1)) 
				(op (vec-ref v1 2)(vec-ref v2 2)) 
				(op (vec-ref v1 3)(vec-ref v2 3)))))))

(define v4-+  (op-4 +))
(define v4--  (op-4 -))
(define v4-*  (op-4 *))
(define v4-/  (op-4 /))

(define (v4-mag v1) 
  (+ (* (vec-ref v1 0)(vec-ref v1 0)) 
     (* (vec-ref v1 1)(vec-ref v1 1))
     (* (vec-ref v1 2)(vec-ref v1 2))
     (* (vec-ref v1 3)(vec-ref v1 3))))

(define (v4-sum v1) 
  (+ (vec-ref v1 0)(vec-ref v1 1)(vec-ref v1 2)(vec-ref v1 3)))

(define (v4-length v1) 
  (sqrt (v4-mag v1)))

(define (v4-norm v1) 
  (let ((len (v4-length v1)))
    (if (> len 0)
      (vec (/ (vec-ref v1 0) len) 
	   (/ (vec-ref v1 1) len)
	   (/ (vec-ref v1 2) len)
	   (/ (vec-ref v1 3) len))
      v1)))

(define (v4-dist v1 v2) 
  (v4-length (v4-- v1 v2)))
