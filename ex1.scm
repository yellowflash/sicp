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
  (sum-series-iterative start square end inc))
