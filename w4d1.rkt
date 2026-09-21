#lang racket

;; initial code from w4d1 assignment post

(define scope (list '(a 1) '(b 2)))

;minor modifications to initial resolve function
;does not need to take in scope variable, uses local variable instead
;returns a more idiomatic #f instead of void
(define resolve (lambda (var)
                  (let search ((local scope))
                    (cond
                      [(null? local) false]
                      [(eq? (caar local) var) (cadar local)]
                      [else (search (cdr local))]))))
;(resolve 'a)
;(resolve 'b)
;(resolve 'c)

;define a function that can store new variable name and value
(define store (lambda (name val)
                (if (resolve name)
                    false
                    (set! scope (cons (list name val) scope)))))
;(store 'a 9)
;(store 'x 8)
;(resolve 'x)

;define a function that can override the value of an existed variable name
(define modify (lambda (name val)
                 (let ((new (let search ((local scope))
                   (cond
                     [(null? local) null]
                     [(eq? (caar local) name)
                      (cons (list name val) (cdr local))]
                     [else (cons (car local) (search (cdr local)))]))))
                   (if (equal? new scope)
                       false
                       (set! scope new)))))
;(modify 'x 9)
;(resolve 'x)
;(modify 'w 8)
;(resolve 'w)
