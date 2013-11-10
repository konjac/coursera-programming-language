#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  ( if (<= low high)
       (cons low (sequence (+ low stride) high stride))
       null))

;; Problem 2
(define (string-append-map xs suffix )
  (map (lambda(x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (let ([len (length xs)])
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [(= n 0) (car xs)]
          [(>= n len) (list-nth-mod xs (remainder n len))]
          [#t (list-nth-mod (cdr xs) (- n 1))])))

;; Problem 4
(define (stream-for-n-steps s n)
  (cond
    [(< 0 n) (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))                                              ]
    [#t null]))

;; Problem 5
(define funny-number-stream
  (letrec ([g (lambda (x) (if(zero? (remainder x 5)) (- 0 x) x))]
           [f (lambda (x) (cons (g x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 6
(define dan-then-dog
  (letrec ([g (lambda (x) (if(odd? x) "dan.jpg" "dog.jpg"))]
           [f (lambda (x) (cons (g x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car(s))) (lambda () (f (cdr(s))))))])
    (lambda () (f s))))

;; Problem 8
(define (cycle-lists xs ys)
  (letrec ([inc (lambda (x) (+ x 1))]
           [f (lambda (i j) (cons (cons (list-nth-mod xs i) (list-nth-mod ys j)) (lambda () (f (inc i) (inc j)))))])
    (lambda () (f 0 0))))

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda(len pos)
                (if (= len pos)
                    #f
                    (letrec ([cur (vector-ref vec pos)])
                        (if (pair? cur)
                            (cond [(equal? (car cur) v) cur]
                                  [#t (f len (+ pos 1))])
                            (f len (+ pos 1))))))])
    (f (vector-length vec) 0)))

;; Problem 10
(define (cached-assoc xs n)
  (letrec ([pos 0]
           [cache (make-vector n #f)])
    (lambda(v) (letrec ([cache-result (vector-assoc v cache)])
                 (cond [cache-result cache-result]
                       [#t (let ([result (assoc v xs)])
                             (begin
                               (vector-set! cache pos result)
                               (set! pos (remainder (+ pos 1) n))
                               result))])))))
;; Problem 11
(define-syntax while-less
   (syntax-rules (do) 
      [(while-less e1 do e2) 
       (letrec ([f (lambda(x) ( if (< e2 x) (f x) #t))]
                [v e1])
        (f v))]))
