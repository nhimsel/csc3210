#lang racket

; initial code from w5d2 assignment post
; now supports 'function

(define parse
  (lambda (exp)
    (cond
      [(symbol? exp) (list 'var-exp exp)]
      [(number? exp) (list 'num-exp exp)]
      [(string? exp) (list 'string-exp exp)]
      [(null? exp) (displayln "ERROR: empty statement")]
      [(equal? 'math (car exp))
       (list
        'math-exp
        (caddr exp)
        (parse (cadr exp))
        (parse (cadddr exp)))]
      [(equal? 'function (car exp))
       (list
        'func-exp
        (list (parse (cadr (cadr exp)))
              (parse (caddr (cadr exp))))
        (parse (caddr exp))
        (list (parse (car (cadddr exp)))
              (parse (cadr (cadddr exp))))
        )]
      [else
       (displayln "PARSER ERROR: the statement has not been supported yet.")]
    )
  )
)

(provide (all-defined-out))
