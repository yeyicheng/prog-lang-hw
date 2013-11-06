#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; problem 1
(define (sequence low high stride)
  (if (> low high)
      empty
      (cons low (sequence (+ low stride) high stride))))

; problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))
  
; problem 3
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))
  
; problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1)))