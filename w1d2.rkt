#lang racket

(define duple (lambda (n x)
                (cond
                  [(= n 0) null]
                  [else (cons x (duple (- n 1) x))])))
;(duple 2 3)
;(duple 4 '(ha ha))
;(duple 0 '(blah))

(define swap (lambda (2lst)
               (list (car (cdr 2lst)) (car 2lst))))
(define invert (lambda (lst)
                 (cond
                   [(null? lst) null]
                   [else (cons (swap (car lst)) (invert (cdr lst)))])))
;(invert '((a 1) (a 2) (1 b) (2 b)))

(define down (lambda (lst)
               (cond
                 [(null? lst) null]
                 [else (cons (list (car lst)) (down (cdr lst)))])))
;(down '(1 2 3))
;(down '((a) (fine) (idea)))
;(down '(a (more (complicated)) object))
