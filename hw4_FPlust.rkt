
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; p1
(define (sequence low high stride)
  (if (>= low high) 
      '()
      (build-list (+ (floor (/ (- high low) stride)) 1)
                  (lambda (x) (+ (* x stride) low)))))

;; p2
(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix)) xs))

;; p3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))  ;; list-ref ?

;; p4
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream ans i)
                (let ([pr (stream)])
                  (if (>= i n)
                      ans
                      (f (cdr pr) (append ans (list (car pr))) (+ i 1)))))])
    (f s '() 0)))

;; p5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 5) 0) (- x) x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


;; p6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons
                           (if x "dan.jpg" "dog.jpg")
                           (lambda () (f (not x)))))])
    (lambda () (f #t))))

;; p7
(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (let ([pr (x)])
                  (cons
                   (cons 0 (car pr))
                   (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

;; p8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons
                           (cons (list-nth-mod xs x) (list-nth-mod ys x))
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; p9
(define (vector-assoc v vec)
  (letrec ([f (lambda (x) (if (>= x (vector-length vec))
                              #f
                              (let [(vx (vector-ref vec x))]
                                (if (pair? vx)
                                    (if (equal? (car vx) v)
                                        vx
                                        (f (+ x 1)))
                                    (f (+ x 1))))))])
    (f 0)))

;; p10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
        [slot 0]
        [f (lambda (v)
             (let ([ans (vector-assoc v memo)])
               (if ans
                   (cdr ans)
                   (let ([new-ans (assoc v xs)])
                     (begin
                       (vector-set! memo (remainder slot n) new-ans)
                       (set! slot (+ slot 1))
                       new-ans)))))])
    f))