#lang scheme
(require "sicp-common.scm")

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
