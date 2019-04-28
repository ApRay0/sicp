(define (factorial n)
  (fact-iter 1 1 n))


(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* product counter)
	       (+ counter 1)
	       max-count)))

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

;; (A 0 n) = 2 * n
;; (A 1 n) = 2 ^ n
;; (A 2 n) = 2 ^ (2 ^ n)

(define (fib n)
  (fib-iter 1 0 n))


(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ b a) a (- count 1))))


(define (count-change amount)
  (cc amount 5))


(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	 ((or (< amount 0) (= kinds-of-coins 0)) 0)
	 (else (+ (cc amount (- kinds-of-coins 1))
		  (cc (- amount (first-denomination kinds-of-coins))
		      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))


;;1.11

(define (ex1.11 n)
  (cond ((< n 3) n)
	(else (+ (ex1.11 (- n 1))
		 (* 2 (ex1.11 (- n 2)))
		 (* 3 (ex1.11 (- n 3)))
		 ))))

(define (ex1.11.2 n)
  (f-iter 2 1 0 0 n))

(define (ex1.11.2-iter a b c count n)
  (if (= count n)
    c
    (f-iter (+ a (* 2 b) (3 * c))
	    a
	    b
	    (+ i 1)
	    n)))




(define (ex1.12 x y)
  ( cond ((or (= y 0) (= x y)) 1)
	 (else (+ (ex1.12 (- x 1) (- y 1))
		  (ex1.12 (- x 1) y)))))

(define (square x)
  (* x x))
;;1.16

(define (ex1.16 b n)
  (define (f-iter res b n)
    (cond ((= n 0) res)
          ((even? n) (f-iter res (square b) (/ n 2)))
	  (else (f-iter (* b res) b (- n 1)))))
  (f-iter 1 b n))


;;1.19
;;

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q n)
  (cond ((= n 0)
	 b)
	(( even? n)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 p q) (square q))
		   (/ n 2)))
	(else
	  (fib-iter (+ (* b q) (* a q) (* a p))
		    (+ (* b p) (* a q))
		    p
		    q
		    (- n 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp1 m)
  (cond ((= exp1 0) 1)
	((even? exp1)
	 (remainder (square (expmod base (/ exp1 2) m))
		    m))
	(else 
	  (remainder (* base (expmod base (- exp1 1) m))
		     m))))


