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
    (lambda () (f 1))))

; problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x 
                                (lambda () (f (if (string=? "dan.jpg" x) "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda(x) (cons 
                          (cons 0 (car (x)))
                          (lambda () (f (cdr (x))))))])
    (lambda ()(f s))))

; problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda(x) (cons
                          (cons (list-nth-mod xs x) (list-nth-mod ys x))
                          (lambda() (f (+ x 1)))))])
    (lambda() (f 0))))
                                
; problem 9
(define (vector-assoc v vec)
  (define (aux n) (if (>= n (vector-length vec))
                      #f
                      (let ([current (vector-ref vec n)])
                        (if (pair? current)
                            (if (equal? (car current) v) current (aux (+ n 1)))
                            (aux (+ n 1))))))
  (aux 0))

; problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin (vector-set! memo pos new-ans)
                                   (set! pos (remainder (add1 pos) n))
                                   new-ans)
                            #f)))))])
    f))

; problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x (eval e1)]
              [f (lambda (y) (if (< y x)
                                 (begin (f e2) #t)
                                 #f))])
       (f e2))]))