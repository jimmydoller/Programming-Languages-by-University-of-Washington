
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda(i) (string-append i suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error 'list-nth-mod "negative number")]
        [(null? xs) (error 'list-nth-mod "empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda(stream tester lst)
             (if(= tester n)
                lst
                (cons (car (stream)) (f (cdr (stream)) (+ tester 1) lst))))])
    (f s 0 null)))


(define funny-number-stream
  (letrec ([f (lambda(x)
                (if(= (remainder x 5) 0)
                   (cons (* x -1) (lambda() (f (+ x 1))))
                   (cons x (lambda()  (f (+ x 1))))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda(x)
                (cond [(string=? x "dan.jpg") (cons x (lambda() (f "dog.jpg")))]
                      [#t (cons x (lambda() (f "dan.jpg")))]))])
    (lambda() (f "dan.jpg"))))


(define (stream-add-zero s)
  (letrec ([f (lambda(s)
                (cons (cons 0 (car (s))) (lambda() (f (cdr (s))))))])
    (lambda() (f s))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda(x1 y1)
                (cond [(and (null? x1) (null? y1)) (f xs ys)]
                      [(null? x1) (f xs y1)]
                      [(null? y1) (f x1 ys)]
                      [#t (cons (cons (car x1) (car y1)) (lambda() (f (cdr x1) (cdr y1))))]))])
    (lambda() (f xs ys))))

(define (vector-assoc v vec)
  (letrec ([prop_vec (vector-filter pair? vec)]
           [f (lambda(slot)
             (cond [(= slot (vector-length prop_vec)) #f]
                   [(equal? v (car (vector-ref prop_vec slot))) (vector-ref prop_vec slot)]
                   [#t (f (+ slot 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
           [k 0]
           [helper (lambda(v)
                     (let ([ans (vector-assoc v cache)])
                       (if ans
                           ans
                           (let ([new-ans (assoc v xs)])
                             (if new-ans
                                 (begin
                                   (cond [(= k n) (set! k 0)])
                                   (vector-set! cache k new-ans)
                                   (set! k (+ k 1))
                                   new-ans)
                                 new-ans)))))])
    helper))
