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
;;2.17
(define (last-pair x)
  (cond ((null? x) '())
	((null? (cdr x)) x)
	(else (last-pair (cdr x)))))

;;2.18
(define (ex2.18 x)
  (iter2.18 x '()))

(define (iter2.18 remain res)
  (if (null? remain) res
    (iter2.18 (cdr remain) (cons (car remain) res))))


;;2.20


;;2.21
(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
	  (square-list (cdr items)))))

(define (square-list items)
  (map square items))



;;2.22
(define (ex2.22 items)
      (define (iter things answer)
	        (if (null? things)
		              answer
			                  (iter (cdr things)
						                  (cons answer
									                        (square (car things))))))
          (iter items '()))


(define (square x)
  (* x x))

;;2.27
(define (deep-reverse x)
  (cond ((null? x) '())
	((not (pair? x)) x)
	(else (reverse (list (deep-reverse (car x))
			     (deep-reverse (cdr x)))))))

;;2.28
(define (fringe x)
  (cond ((null? x) '())
	((not (pair? x)) x)
	(else (append (fringe (car x)
			      (cadr x))))))

;;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch stru)
  (car stru))

(define (right-branch stru)
  (cadr stru))

(define (branch-length bran)
  (car bran))

(define (branch-structure bran)
  (cadr bran))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (mobile-balance? mobile)
  (and (same-torque? (left-branch mobile) (right-branch mobile))
       (branch-balance? (left-branch mobile))
       (branch-balance? (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (same-torque? left right)
  (= (branch-torque left)
     (branch-torque right)))

(define (branch-balance? branch)
  (if (pair? (branch-structure branch))
    (mobile-balance? (branch-structure branch))
    #t))

;;2.30
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree) factor)
		    (square-tree (cdr tree) factor)))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree sub-tree)
	   (square sub-tree)))
       tree))


;;2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (tree-map func sub-tree)
	   (func sub-tree)))
       tree))


;;2.32
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map lambda (x)
			(cons (car s) x))
	      rest))))

;;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* higher-terms x)))
	      0
	      coefficient-sequence))


;;2.35
(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (sub-tree)
		     (if (pair? sub-tree)
		       (count-leaves sub-tree)
		       1))
		   t)))

;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (car seqs))
	  (accumulate-n op init (cdr seqs)))))

;;2.37
(define (matrix-*-vector m v)
  (map (lambda (x)
	 (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
	   (matrix-*-vector  cols x))
	 m)))

;;----------------------------
(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq)
	(accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (permutations s)
  (if (null? s) '()
    (flatmap (lambda (x)
	       (map (lambda (p) (cons x p))
		    (permutations (remove x s))))
	     s)))

;;2.40







































































