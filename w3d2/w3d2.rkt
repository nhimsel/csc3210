#lang racket

(define ins (lambda (pred n loi)
              (cond
                [(null? loi) (list n)]
                [(pred n (car loi)) (cons n loi)]
                [else (cons (car loi) (ins pred n (cdr loi)))])))

(define sort (lambda (loi)
               (cond
                 [(null? loi) null]
                 [else (ins < (car loi) (sort (cdr loi)))])))
;(sort '(8 2 5 2 3))

(define sort/predicate (lambda (pred loi)
                         (cond
                           [(null? loi) null]
                           [else
                            (ins pred (car loi) (sort/predicate
                                                 pred (cdr loi)))])))
;(sort/predicate < '(8 2 5 2 3))
;(sort/predicate > '(8 2 5 2 3))
