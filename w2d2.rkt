#lang racket

(define traverse (lambda (sos1 sos2)
                   (cond
                     [(not (null? sos2)) (cons
                                          (list (car sos1) (car sos2))
                                          (traverse sos1 (cdr sos2)))]
                     [else null])))
(define product (lambda (sos1 sos2)
                  (cond
                    [(not (null? sos1)) (cons
                                         (traverse sos1 sos2)
                                         (product (cdr sos1) sos2))]
                    [else null])))
;(product '(a b c) '(x y))

(define filter-in (lambda (pred lst)
                    (cond
                      [(not (null? lst))
                       (if (pred (car lst))
                           (cons (car lst) (filter-in pred (cdr lst)))
                           (filter-in pred (cdr lst)))]
                      [else null])))
;(filter-in number? '(a 2 (1 3) b 7))
;(filter-in symbol? '(a (b c) 17 foo))

(define list-index (lambda (pred lst)
                     (cond
                       [(not (null? lst))
                        (if (pred (car lst))
                            0
                            (let ([n (list-index pred (cdr lst))])
                               (if (number? n)
                                   (+ 1 n)
                                   n)))]
                       [else #f])))
;(list-index number? '(a 2 (1 3) b 7))
;(list-index symbol? '(a (b c) 17 foo))
;(list-index symbol? '(1 2 (a b) 3))
