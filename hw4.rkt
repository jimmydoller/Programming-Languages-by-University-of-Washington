
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


(define (sequence low high stride)
  (if (> low high)
	  null
	  (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond ((< n 0) (error "list-nth-mod: negative number"))
		((null? xs) (error "list-nth-mod: empty list:"))
		(else (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (if (= n 0)
	  null
	  (let ([next (s)])
			(cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (* x -1) x)
								(lambda () (f (+ x 1)))))])
	(lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg")
													"dog.jpg"
													"dan.jpg")))))])
	(lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (let ([next (s)])
	(lambda () (cons (cons 0 (car next)) (stream-add-zero (cdr next))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (lambda () (cons n (f (+ n 1)))))]
		   [stream (lambda (x) (let ([iter (x)])
								 (cons (cons (list-nth-mod xs (car iter)) (list-nth-mod ys (car iter)))
									   (lambda () (stream (cdr iter))))))])
	(lambda () (stream (f 0)))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
		   [f (lambda (n) (if (= n l)
							  #f
							  (let ([next (vector-ref vec n)])
								(if (and (pair? next) (equal? v (car next)))
									next
									(f (+ n 1))))))])
	(f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
		   [next-slot 0]
		   [inc-slot (lambda () (if (< next-slot (- n 1))
									(set! next-slot (+ next-slot 1))
									(set! next-slot 0)))]
		   [check-cache (lambda (v count)
						  (if (= count n)
							  (begin (let
									   ([ans (assoc v xs)])
										(vector-set! cache next-slot ans)
										(inc-slot)
										ans))
							  (let ([next (vector-ref cache count)])
								(if (and (pair? next) (equal? v (car next)))
									next
									(check-cache v (+ count 1))))))])
	(lambda (v) (check-cache v 0))))