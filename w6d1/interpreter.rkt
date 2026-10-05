#lang racket

; initial code from w6d1 assignment post
; now evaluates 'func-exp

(require "util.rkt")

(define
    process
    (lambda (parsed-exp) (
        ;var-exp symbol
        cond
            ((null? parsed-exp) (displayln "ERROR: EMPTY PROGRAM."))
            ;error handler from the parser
            ((void? parsed-exp) (void))
            ;if it is a variable, we will resoleve the value, and return the resolved value
            ;programmer should be responsible for what he wrote, rather than hand it to the error handler
            ;since the parser already validate the statement of the language, interpreter can omit this stage, but it is still risky if you do not use the parser before interpreter
            ((equal? 'var-exp (car parsed-exp)) (resolve_env environment (cadr parsed-exp)))
            ((equal? 'num-exp (car parsed-exp)) (car (cdr parsed-exp)))
            ((eq? 'math-exp (car parsed-exp))   ;math-exp + (math-exp + ...) (num-exp 10)
                (cond
                    ;((or (void? (process (caddr parsed-exp)) ) (process (cadddr parsed-exp))))
                    ((and (number? (process (caddr parsed-exp)) ) (number? (process (cadddr parsed-exp))))
                        (do_math (cadr parsed-exp) (process (caddr parsed-exp)) (process (cadddr parsed-exp))))
                    (else (displayln "INTERPRETOR ERROR: non-numeric cannot apply math."))
                )
            )
            ((eq? 'func-exp (car parsed-exp))
             ;; create temporary scope
             (let ((env (cons null environment)))
               (map (lambda (vars)
                      (set! env
                            (cons
                             (update_scope (car env)
                                           (car vars)
                                           (cadr vars))
                             (cdr env))))
                    (map (lambda (lst)
                           (list
                            (cadr (car lst))
                            (cadr (cadr lst))))
                         (cadr parsed-exp)))
               (update_environment env))
             ;; eval
             (let ((out (begin (process (caddr parsed-exp)))))
               (update_environment (cdr environment))
               out))
            ((eq? 'bulk-exp (car parsed-exp))
             ;; process all expressions in bulk-exp
             ;; return output of last expression
             (let eval ((exp (cdr parsed-exp)))
               (if (null? (cdr exp)) (process (car exp))
                   (begin (process (car exp)) (eval (cdr exp))))))
            (else (displayln "ERROR: expression has not been supported yet."))
    ))
)

(provide (all-defined-out))
