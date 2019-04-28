(define (cube x) (* x x x ))

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-intergers (+ 1 a) b))))


(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



;;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))


;;1.31
(define (product term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (* res (term a)))))
  (iter a 1))


;;1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (combiner res (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;;1.33
(define (filtered-accumulate cimbiner null-value term a next b condition?)
  (define (iter a res)
    (cond ((> a b) res)
	  ((condition? a) (iter (next a) (cimbiner res (term a))))
	  (else (iter (next a) res))))
  (iter a null-value))


(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pot-point)
      midpoint
      (let ((test-value (f midpoint)))
	(cond ((positive? test-value)
	       (search f neg-point midpoint))
	      ((negative? test-value)
	       (search f midpoint pos-point))
	       (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))


;;1.37
(define (cont-frac n d k)
  (define (cf i)
    (if (= k i)
      (/ (n k) (d k))
      (/ (n i)
	 (+ (d i) (cf (+ i 1))))))
  (cf 1))

;;1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c
       )))

;;1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;;1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;;1.43
(define (repeated f n)
  (if (= n 1) f
    (repeated (lambda (x) (f (f x))) (- n 1))))
(define (square x)
  (* x x))

;;1.46
(define (iterative-improve close-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
	(if (close-enough? guess next) next
	  (try next))))
    (try first-guess)))






























