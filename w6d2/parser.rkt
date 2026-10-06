#lang racket

; initial code from w6d2 assignment post
; implement 'ask, 'bool

(require "util.rkt")
;parser will translate my programming language into an intemediate form
;it will be reletively easier to execute

(define parse
  (lambda
      (exp)
    (cond
      ((symbol? exp)
       (cond
         [(eq? 'true exp) '(bool-exp t)]
         [(eq? 'false exp) '(bool-exp f)]
         [else (list 'var-exp exp)]))
      ((number? exp) (list 'num-exp exp))
      ((string? exp) (list 'string-exp exp))
      ((null? exp) (displayln "ERROR: empty statement"))
      [(bool_exp? exp)
       (cond
         [(equal? '(true) exp) '(bool-exp t)]
         [(equal? '(false) exp) '(bool-exp f)]
         [else (displayln "ERROR: not a boolean.")])]
      ((and (is_valid_math_op (cadr exp)) (equal? (length exp) 3))
        (list
          'math-exp
          (cadr exp)
          (parse (car exp))
          (parse (caddr exp))
        ))  ;math-exp has no error message here
      ;function expression
      ((equal? 'function (car exp))
        (if (and (equal? (length (cadr exp)) (length (cadddr exp))) (not (null? (caddr exp))))
          (list
            'func-exp
            (create_pair_list '() (map parse (cadr exp)) (map parse (cadddr exp)))
            (cons 'bulk-exp (map parse (caddr exp)))
          )
          (displayln "PARSER ERROR: this is not a valid anonymous function definition."))
      )
      [(equal? 'ask (car exp))
       (cond
         [(null? (cadr exp))
          (displayln "PARSE ERROR: null boolean expression.")]
         [(eq? 1 (length (cadr exp)))
          (list
           'ask-exp
           (parse (car (cadr exp)))
           (cons 'bulk-exp (map parse (caddr exp)))
           (cons 'bulk-exp (map parse (cadddr exp))))]
         [(not (or (and (bool_op? (cadr (cadr exp)))
                        (eq? 3 (length (cadr exp))))
                   (and (bool_op? (car (cadr exp)))
                        (eq? 2 (length (cadr exp))))
                   (or (list? (caddr exp)) (list? (cadddr exp)))))
          (displayln "PARSE ERROR: malformed boolean expression.")]
         ;; not must be handled separately
         [(eq? 2 (length (cadr exp)))
          (if (unary? (car (cadr exp)))
              (list 'ask-exp
                    (list (car (cadr exp)) (parse (cadr (cadr exp))))
                    (cons 'bulk-exp (map parse (caddr exp)))
                    (cons 'bulk-exp (map parse (cadddr exp))))
              (displayln "PARSER ERROR: Illegal unary operator."))]
         [else
          (if (not (unary? (cadr (cadr exp))))
              (list
               'ask-exp
               (list (cadr (cadr exp))
                     (parse (car (cadr exp)))
                     (parse (caddr (cadr exp))))
               (cons 'bulk-exp (map parse (caddr exp)))
               (cons 'bulk-exp (map parse (cadddr exp))))
             (displayln "PARSER ERROR: Illegal use of unary operator."))])]
      (else (displayln "PARSER ERROR: the statement has not been supported yet."))
    )
  )
)

(provide (all-defined-out))
