#lang racket

;; initial code from w4d2 assignment post

(define scope (list '(a 1) '(b 2)))

;var_env:= var_scop x (var_env)
(define environment (list scope))
;(define environment '(((a 1) (b 2)) ((c 3) (d 4))))

;define a function that can pass in a variable name, and resolve it
;(resolve #symbol) -> value or null
;; refactored to make easier to read
(define resolve (lambda (local var)
                  (cond
                    [(null? local) (void)]
                    [(eq? (caar local) var) (cadar local)]
                    [else (resolve (cdr local) var)])))
;; refactored to make easier to read
(define resolve_env (lambda (env var)
                      (cond
                        [(null? env) (void)]
                        [(void? (resolve (car env) var))
                         (resolve_env (cdr env) var)]
                        [else (resolve (car env) var)])))

;define a function that can store new variable name and value
;when variable name already in memory, then
;; refactored to make easier to read
;; renamed dog->store
(define store (lambda (local var val)
                (cond
                  [(null? local) (set! scope (cons (list var val) scope))]
                  [(void? (resolve local var))
                   (set! scope (cons (list var val) scope))]
                  [else (displayln "ERROR: Variable name has been used")])))
;; store a value in the current scope if it is not found in the environment
(define store_env (lambda (env var val)
                    (let ((new (let ((local (car env)))
                                 (cond
                                  [(null? local) (cons (list var val) local)]
                                  [(void? (resolve_env env var))
                                   (cons (list var val) local)]
                                  [else
                                   (displayln "ERROR: Variable already defined in environment")]))))
                      (if (or (equal? new env) (void? new))
                          (void)
                          (set! environment (list new))))))
;(store_env environment 'c 6)
;(resolve_env environment 'c)
;(store_env environment 'a 2)
;(resolve_env environment 'a)

;define a function that can override the value of an existed variable name
;; refactored to make easier to read
(define update_pair (lambda (left lst key val)
    (cond
      ((null? lst) left)
      ((eq? (car (car lst)) key) (append left (list (list key val)) (cdr lst)))
      (else (update_pair
             (append left (list (list key val))) (cdr lst) key val)))))

;insert and update
;; refactored to make easier to read
;; renamed wolf->modify
(define modify (lambda (local var val)
                 (cond
                   [(null? local) (store local var val)]
                   [(void? (resolve local var)) (store local var val)]
                   [else (set! scope (update_pair '() local var val))])))
;; update the val of var if defined in environment, else add to current scope
(define modify_env (lambda (env var val)
                     (let ((new (let search ((cur env))
                                  (cond
                                    [(null? cur) (begin (store_env env var val)
                                                        null)]
                                    [(void? (resolve (car cur) var))
                                     (cons (car cur) (search (cdr cur)))]
                                    [else
                                     (cons (map (lambda (pair)
                                                  (if (eq? (car pair) var)
                                                      (list var val)
                                                      pair))
                                                (car cur))
                                           (cdr cur))]))))
                       (if (equal? new env)
                           (void)
                           (set! environment new)))))
;(modify_env environment 'a 2)
;(resolve_env environment 'a)
;(modify_env environment 'd 7)
;(resolve_env environment 'd)
