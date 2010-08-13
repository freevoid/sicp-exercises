(module sicp-chapter1 scheme
  
(require "sicp-common.scm")
(provide repeated)

; This module contains solved excersices and differect code chunks from SICP
; book (second edition) chapter 1

; ex. 1.2
(define ex1_2 (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

; ex. 1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (ex1_3 x y z)
  (if (> x y)
      (if (> y z)
          (sum-of-squares x y)
          (sum-of-squares x z))
      (if (> x z)
          (sum-of-squares x y)
          (sum-of-squares y z))))

; Section 1.1.7
; Finding square roots with Newton's method
(define (improve-sqrt guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt1 x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve-sqrt guess x)
                   x)))
  (sqrt-iter 1.0 x))

; ex. 1.7 advanced good-enough? version
(define (good-enough? oldguess guess)
  (< (/ (abs (- guess oldguess)) guess) 0.001))
(define (ex1_7 x)
  (define (sqrt-iter guess x)
    (let ((newguess (improve-sqrt guess x)))
      (if (good-enough? guess newguess)
          newguess
          (sqrt-iter newguess
                     x))))
  (sqrt-iter 1.0 x))

; ex. 1.8 cube roots
(define (ex1_8 x)
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess x)
    (let ((newguess (improve guess x)))
      (if (good-enough? guess newguess)
          newguess
          (cbrt-iter newguess
                     x))))
  (cbrt-iter 1.0 x))

; ex. 1.10 Akkermann function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Section 1.2.2
; Tree recursion, Fibonacci numbers and change counting

(define (fib-naive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-naive (- n 1))
                 (fib-naive (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 2)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 10)
        ((= kinds-of-coins 5) 20)
        ((= kinds-of-coins 6) 50)
        ((= kinds-of-coins 7) 100)
        ((= kinds-of-coins 8) 200)))

; ex. 1.11
(define (ex1_11_rec n)
  (if (< n 3)
      n
      (+ (ex1_11_rec (- n 1)) (ex1_11_rec (- n 2)) (ex1_11_rec (- n 3)))))

(define (ex1_11_iter n)
  (define (inner-iter a b c count)
    (if (= 0 count)
        a
        (inner-iter (+ a b c) a b (- count 1))))
  (if (< n 3)
      n
      (inner-iter 2 1 0 (- n 2))))

; ex. 1.12 Pascal triangle (Binominal coef.)
(define (pascal n k)
  (let ((ndec (- n 1)))
    (cond ((or (< n 1) (= k 0) (= k n)) 1)
          (else (+ (pascal ndec k) (pascal ndec (- k 1))))
          )))

(define (comb n k)
  (define (comb-iter n k acc)
    (cond ((or (= k 0) (= n 0)) acc)
          (else (comb-iter (- n 1) (- k 1) (* (/ n k) acc)))))
  (comb-iter n k 1))

(define (comb-rep n k)
  (comb (+ n k (- 1)) k))

; Section 1.2.4 Exponent

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))
  (expt-iter b n 1))

;(define (even? x)
;  (= (remainder x 2) 0))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; ex. 1.16 iterative fast-expt
(define (ex1_16 b n)
  (define (inner-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (inner-iter (square b) (/ n 2) a))
          (else (inner-iter b (- n 1) (* a b)))))
  (inner-iter b n 1))

; ex. 1.19 smart fibonacci algorithm
(define (fib-smart n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (let ((sqq (square q)))
             (fib-iter a
                       b
                       (+ (square p) sqq)
                       (+ (* 2 p q) sqq)
                       (/ count 2))))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

; 1.2.6 Prime test
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime-naive n)
  (= (smallest-divisor n) n))

; Ex. 1.30
(define (ex1_30 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Ex. 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc a) (+ a 1))
(define (echo a) a)

(define (factorial n)
  (product echo 1 inc n))

(define (calc-pi n)
  (define (next x)
    (let ((a (car x))
          (b (car (cdr x))))
      (if (< a b)
          (list (+ b 1) b)
          (list a (+ a 1)))))
  (define (term x)
    (let ((a (car x))
          (b (car (cdr x))))
      (/ a b)))
  (product term '(2 3) next (list n (+ n 1))))

; ex. 1.41
(define (double fun)
  (lambda (x) (fun (fun x))))

; ex. 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; ex. 1.43
(define (repeated f count)
  (define (iter comp c)
    (if (= c 1) comp
        (iter (compose f comp) (- c 1))))
  (if (< count 1) (error "Bad count in call to ``repeated''")
      (iter f count)))

; ex. 1.44
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (nsmooth f dx n)
  ((repeated (lambda (fun) (smooth fun dx)) n) f))

)
