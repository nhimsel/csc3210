#lang racket

; code unmodified from w5d1 assignment post

;parser will translate my programming language into an intemediate form
;it will be reletively easier to execute

(define parse
  (lambda
      (exp)
    (cond
      ((symbol? exp) (list 'var-exp exp))
       (else (displayln "ERROR"))
    )
  )
)

(provide (all-defined-out))
