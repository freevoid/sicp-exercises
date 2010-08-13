#lang scheme
(require "sicp-common.scm")
(require "sicp-chapter1.scm") ; for repeated

; Section 2.1.1 rational numbers

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

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (quotient n g) (quotient d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

; 2.1.3

; ex 2.6
(define zero (lambda (f) (lambda (x) x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (make-church n)
  (if (= n 0)
      (lambda (f) (lambda (x) x))
      (lambda (f) (repeated f n))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (sum list)
  (define (iter cur acc)
    (if (null? cur) acc
        (iter (cdr cur) (+ acc (car cur)))))
  (iter list 0))


; Exercise 2.17.  Define a procedure last-pair that returns the list that
; contains only the last element of a given (nonempty) list

; > (last-pair (list 23 72 149 34))
; (34)

(define (last-pair sequence)
  (if (null? sequence)
      (error "Expects non-empty sequence")
      (let [(tail (cdr sequence))] 
        (if (null? tail)
            sequence
            (last-pair tail))
      )))


; Exercise 2.18.  Define a procedure reverse that takes a list as argument and
; returns a list of the same elements in reverse order:

; > (reverse (list 1 4 9 16 25))
; (25 16 9 4 1)

(define (reverse sequence)
  (define (reverse-iter acc seq)
    (if (null? seq)
        acc
        (reverse-iter (cons (car seq) acc) (cdr seq)))
  )
  (reverse-iter nil sequence))

; TODO: 2.19

; Exercise 2.20.
;Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd ;parity as the first argument. For example,

; > (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
; > (same-parity 2 3 4 5 6 7)
; (2 4 6)

(define (same-parity first . rest)
  (if (even? first)
      (cons first (filter even? rest))
      (cons first (filter odd? rest))))

; Exercise 2.32.

(define (seq from to)
  (define (iter count acc)
    (if (= count from)
        (cons from acc)
        (iter (- count 1) (cons count acc))))
  (iter to nil))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((head (car s))
            (rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons head x))
                          rest)))))

