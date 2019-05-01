(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? xy)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



;;2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment p)
  (car p))
(define (end-segment p)
  (cdr p))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment line)
  (cons (/ (+ (x-point (start-segment line))
	      (x-point (end-segment line)))
	   2)
	(/ (+ (y-point (start-segment line))
	      (y-point (end-segment line)))
	   2)))

;;2.4
;;(define cdr z)
;; (z (lambda (p q) q)))


;;2.5
(define (ex2.5 a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car2.5 z)
  (if (= 0 (remainder z 2))
    (+ 1 (car (/ z 2)))
    0))

(define (cdr2.5 z)
  (if (= 0 (remainder z 3))
    (+ 1 (cdr (/ z 3)))
    0))

;;2.6 p-62
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x )))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define tow
  (lambda (x)
    (f (f x))))

(define plus2.6
  (lambda (m)
    (lambda (n)
      (lambda (f)
	(lambda(x)
	  (m f (n f x)))))))



























