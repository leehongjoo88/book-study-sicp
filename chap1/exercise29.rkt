#lang racket

(require "sum.rkt")

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (coef k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (/ h 3) (coef k) (f (+ a (* k h)))))
  (define (next k)
    (+ k 1))
  (sum term 0 next n))

(define (simpsons-rule2 f a b n)
  (define (coef k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (term-template k h h-third)
    (* h-third (coef k) (f (+ a (* k h)))))
  (let* ([h (/ (- b a) n)]
         [h-third (/ h 3)]
         [term (lambda (x) (term-template x h h-third))]
         [next (lambda (x) (+ x 1))])
    (sum term 0 next n)))
