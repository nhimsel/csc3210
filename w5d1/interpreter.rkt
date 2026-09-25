#lang racket
(require "util.rkt")

(define NULL (void))

(define test (lambda (actual expect)
               (if (equal? actual expect)
                   (begin (display "PASS: ")
                          (displayln actual))
                   (begin (display "FAIL: ")
                          (display actual)
                          (display " expected: ")
                          (displayln expect)))))

(define process (lambda (exp)
                  (cond
                    [(null? exp) NULL]
                    [(void? exp) NULL]
                    [(eq? (car exp) 'var-exp)
                     (let ((val (resolve_env environment (cadr exp))))
                       (if (void? val)
                           (displayln "INTERPRETER ERROR: variable not found")
                           val))]
                    [else
                     (displayln "INTERPRETER ERROR: value not supported yet")])))

(provide (all-defined-out))
