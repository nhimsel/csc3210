#lang racket

; initial code from w5d2 assignment post
; now supports 'func-exp

(require "util.rkt")

(define process
  (lambda (parsed-exp)
    (cond
      [(null? parsed-exp) (displayln "ERROR: EMPTY PROGRAM.")]
      [(void? parsed-exp) (void)]
      [(equal? 'var-exp (car parsed-exp))
       (resolve_env environment (cadr parsed-exp))]
      [(equal? 'num-exp (car parsed-exp))
       (car (cdr parsed-exp))]
      [(eq? 'math-exp (car parsed-exp))
       (cond
         [(and (number? (process (caddr parsed-exp)))
               (number? (process (cadddr parsed-exp))))
          (do_math (cadr parsed-exp)
                   (process (caddr parsed-exp))
                   (process (cadddr parsed-exp)))]
         [else (displayln "INTERPRETOR ERROR: non-numeric cannot apply math.")])]
      [(eq? 'func-exp (car parsed-exp))
       ;; create temporary local environment scope
       (let ((env (cons null environment)))
         (map (lambda (vars vals)
                (set! env
                      (cons
                        (update_scope (car env)
                                      (cadr vars)
                                      (cadr vals))
                        (cdr env))))
              (cadr parsed-exp) (cadddr parsed-exp))
         (update_environment env))
       ;; eval
       (let ((out (begin (process (caddr parsed-exp)))))
         (update_environment (cdr environment))
         out)]
      [else (displayln "ERROR: expression has not been supported yet.")])))

(provide (all-defined-out))
