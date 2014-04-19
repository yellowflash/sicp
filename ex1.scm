(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-large-squares a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
	((and (< b a) (< b c)) (sum-of-squares a c))
	(else (sum-of-squares a b))))


; Ben Bitdiddle test

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))


; Newton's square root method

(define (abs x)
  ((if (< x 0) - +) x))

(define (good-enough? guess n)
  (< (abs (- (* guess guess) n)) 0.001))

(define (average x y)
  (/ (+ x y) 2))
     

(define (square-root-iter guess n)
  (if (good-enough? guess n)
      guess
      (square-root-iter (average guess (/ n guess)) n)))

(define (square-root n)
  (square-root-iter 1.0 n))

; Fixing the good-enough through previous guess

(define (square-root-iter-better prev guess n)
  (if (< (abs (- prev guess)) 0.001)
      guess
      (square-root-iter-better guess (average guess (/ n guess)) n)))

(define (square-root-better n)
  (square-root-iter-better 0 1.0 n))


; Newton's method to find cube root

(define (cube-root-iter prev guess n)
  (if (< (abs (- prev guess))  0.001)
      guess
      (cube-root-iter guess (/ (+ (/ n (* guess guess)) (* 2 guess)) 3) n)))

(define (cube-root n)
  (cube-root-iter 0 1.0 n))


; Ackermann function

(define (ackermann x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (ackermann (- x 1)
			 (ackermann x (- y 1))))))


(define (fibo-like-recur n)
  (cond ((< n 3) n)
	(else (+ (fibo-like-recur (- n 1)) (* 2 (fibo-like-recur (- n 2))) (* 3 (fibo-like-recur (- n 3)))))))

(define (fibo-like-iter n)
  (define (fibo-like-iter-till sofar-k sofar-k-1 sofar-k-2 k)
    (cond ((> k n) sofar-k)
	  ((= k 1) (fibo-like-iter-till 1 0 0 (+ k 1)))
	  ((= k 2) (fibo-like-iter-till 2 1 0 (+ k 1)))
	  (else (fibo-like-iter-till (+ sofar-k (* 2 sofar-k-1) (* 3 sofar-k-2)) sofar-k sofar-k-1 (+ k 1)))))
  (fibo-like-iter-till 0 0 0 0))


; Pascal's triangle

(define (pascal row col)  
  (cond ((= col 1)  1)
	((= row col) 1)
	(else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

; Iterative fast exp

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-exp n x)
  (define (fast-exp-iter r k s)
    (cond  ((= k 0) r)
	   ((even? k) (fast-exp-iter r (/ k 2) (square s)))
	   (else (fast-exp-iter (* r s) (- k 1) s))))
  (fast-exp-iter 1 x n))

(define (fast-mult a b)
  (define (fast-mult-iter r k s)
    (cond ((= k 0) r)
	  ((even? k) (fast-mult-iter r (/ k 2) (+ s s)))
	  (else (fast-mult-iter (+ r s) (- k 1) s))))
  (fast-mult-iter 0 b a))


; Smallest divisor
; Can increment 2 but lets not bother

(define (smallest-divisor n)
  (define (smallest-divisor-iter check)
    (if (> (* check check) n) 
	n
	(if (= (remainder n check) 0)
	    check
	    (smallest-divisor-iter (+ check 2)))))
  (if (even? n) 2
      (smallest-divisor-iter 3)))

;Timed Prime

(define (prime? n)
  (= (smallest-divisor n) n))

(define (report-prime n time)
  (display n)
  (display "****")
  (display time)
  (newline))

(define (iter-and-report n start-time x)
  (if (prime? n)
      (begin
       (report-prime n (- (runtime) start-time))
       (iter-and-report (+ n 1) (runtime) (+ x 1)))
      (if (< x 4) (iter-and-report (+ n 1) (runtime) x))))

;Carmichael Numbers

(define (exp-mod x n m)
  (cond ((= n 1) x)
	((even? n) (remainder (square (exp-mod x (/ n 2) m)) m))
	(else (remainder (* x (exp-mod x (- n 1) m)) m))))

(define (carmichael? n)
  (define (carmichael-iter i)
    (cond ((= i n)  true)
	  ((= i (exp-mod i n n)) (carmichael-iter (+ i 1)))
	  (else false)))
  (and (not (prime? n)) (carmichael-iter 2)))
	

;Sum of a series
(define (inc a) (+ a 1))

(define (sum-series a term b next)
  (if (> a b) 0
      (+ (term a) (sum-series (next a) term b next))))

(define (sum-series-iterative a term b next)
  (define (sum-series-iter s c)
    (if (> c b) s
	(sum-series-iter (+ s (term c)) (next c))))
  (sum-series-iter 0 a))

(define (sum-squares start end)
  (sum-series start square end inc))

;Product of a series

(define (identity a) a)

(define (product-series a term b next)
  (define (product-series-iter p c)
    (if (> c b) p
	(product-series-iter (* p (term c)) (next c))))
  (product-series-iter 1 a))


(define (product-series-recur a term b next)
  (if (> a b) 1
      (* (term a) (product-series-recur (next a) term b next))))

(define (factorial n)
  (product-series 1 identity n inc))

; John Wallis Sequence
(define (pi-series)
  (define (term a)
    (/ (- a (remainder a 2.0)) (- a (abs (- (remainder a 2.0) 1.0)))))
  (* (product-series 3.0 term 10000.0 inc) 4))


; Accumulate

(define (accumulate combiner null-val a term b next)
  (define (accumulate-iter curr sofar)
    (if (> curr b) sofar
	(accumulate-iter (next curr) (combiner (term curr) sofar))))
  (accumulate-iter a null-val))

(define (accumulate-recur combiner null-val a term b next)
  (if (> a b) null-val
      (combiner (accumulate-recur combiner null-val (next a) term b next) (term a))))

(define (sum-series-with-accumulate a term b next)
  (accumulate + 0 a term b next))

(define (product-series-with-accumulate a term b next)
  (accumulate * 1 a term b next))


; Filtered Accumulate

(define (filtered-accumulate filter combiner null-val a term b next)
  (define (filtered-accumulate-iter curr sofar)
    (if  (> curr b) sofar
	 (filtered-accumulate-iter (next curr) (if (filter curr) (combiner curr sofar) sofar))))
  (filtered-accumulate-iter a null-val))

(define (sum-of-primes start end)
  (filtered-accumulate prime? + 0 start identity end inc))


(define (product-of-rel-primes n)
  (define (relative-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate relative-prime? * 1 start identity end inc))

; Fixed Points

(define (close-enough? a b) (< (abs (- a b)) 0.000001))

(define (fixed-point fun guess)
  (let ((val (fun guess)))
     (if (close-enough? val guess)
	 val
	 (fixed-point fun val))))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1))

; Continued Fraction
(define (K x) (lambda (y) x))

(define (cont-fraction n d k)
  (define (cont-fraction-recur i)
    (if (> i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (cont-fraction-recur (+ i 1))))))
  (cont-fraction-recur 1))

(define (cont-fraction-iterative n d k)
  (define (cont-fraction-iter sofar i)
    (if (< i 1)
	sofar
	(cont-fraction-iter (/ (n i) (+ ( d i) sofar)) (- i 1))))
  (cont-fraction-iter 0 k))

(define inverse-golden-ratio (cont-fraction-iterative (K 1.0) (K 1.0) 100))


; Euler's continued-fraction for e

(define e-minus-2-series (cont-fraction (K 1.0) 
				(lambda (x) 
				  (cond ((= x 1) 1)
					((= x 2) 2)
					((= (remainder (+ x 1) 3) 0) (* (/ (+ x 1) 3) 2))
					(else 1))) 100))

; Lambert's tangent function

(define (tan-lambert d)
  (cont-fraction (lambda (x) (if (= x 1) d (- (* d d)))) (lambda (x) (- (* x 2) 1)) 100))
					
