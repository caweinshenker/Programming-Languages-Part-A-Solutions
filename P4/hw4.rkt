
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


;; 1

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

;; 2

(define (string-append-map xs suffix)
  (map (lambda (x) 
         (string-append x suffix))
       xs))


;; 3

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; 4

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      ( let ([s-eval (s)])
         (cons (car s-eval) (stream-for-n-steps (cdr s-eval) (- n 1))))))
         

;; 5

(define funny-number-stream 
  (letrec ([f (lambda (x)
                 (if (equal? 0 (remainder x 5))
                     (cons (* x -1)  ( lambda() (f (+ x 1))))
                     (cons x (lambda () (f (+ x 1))))
                  ))])
  (lambda () (f 1))))

;; 6

(define dan-then-dog
  (letrec ([ f (lambda (x) 
                 (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

;; 7

(define (stream-add-zero s)
  (letrec ([ f (lambda (ss) 
                 (let ([s-eval (s)])
                   (cons (cons 0 (car s-eval)) (cdr s-eval))))])
    (lambda () (f s))))

;; 8

(define (cycle-lists xs ys)
  (letrec ([min-len (min (length xs) (length ys))]
            [f (lambda (xss yss n)
                 (cons (cons (list-nth-mod xss n) (list-nth-mod yss n)) (lambda () (f xss yss (modulo (+ n 1) (+ min-len 1))))))])
           (lambda () (f xs ys 0))))

;; 9

(define (vector-assoc v vec)
  (cond [(equal? (vector-length vec) 0) #f]
        [(not (pair? (vector-ref vec 0))) (vector-assoc v (vector-drop vec 1))]
        [(equal? v (car (vector-ref vec 0))) (vector-ref vec 0)]
        [#t (vector-assoc v (vector-drop vec 1))]
        ))

;; 10

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n )]
          [next 0 ])
  (lambda (x) (or (vector-assoc x cache)
                 (let ([list-hit (assoc x xs)])
                       (and list-hit
                           (begin (vector-set! cache next list-hit)
                                  (set! next (modulo (+ next 1) n)) 
                                  list-hit)))))))
                                               
           
  