#lang racket

; initial code from w6d2 assignment post
; added support for 'ask-exp, 'bool-exp

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
            ((equal? 'bool-exp (car parsed-exp)) (cadr parsed-exp))
            ((eq? 'math-exp (car parsed-exp))   ;math-exp + (math-exp + ...) (num-exp 10)
                (cond
                    ;((or (void? (process (caddr parsed-exp)) ) (process (cadddr parsed-exp))))
                    ((and (number? (process (caddr parsed-exp)) ) (number? (process (cadddr parsed-exp))))
                        (do_math (cadr parsed-exp) (process (caddr parsed-exp)) (process (cadddr parsed-exp))))
                    (else (displayln "INTERPRETOR ERROR: non-numeric cannot apply math."))
                )
            )
            ((eq? 'func-exp (car parsed-exp))
                (let
                    (
                        ;cons the parameter list with the original environment
                        ;(((var-exp a) (num-exp 1)) ((var-exp b) (var-exp a)))
                        (my_env 
                            (cons (map (lambda (pair) (list (cadr (car pair)) (process (cadr pair)))) (cadr parsed-exp))
                        environment))
                        (expression_lst
                            (caddr parsed-exp))
                            ;(set! environment (cdr environment)))
                        (ret_val (void))
                    )
                    (begin
                        (update_base_environment my_env)
                        ;(print (cadr (cadr (process expression_lst))))
                        (set! ret_val (cadr (cadr (process expression_lst))))
                        (update_base_environment (cdr environment))
                        ret_val
                    )
                )
            )
            [(eq? 'ask-exp (car parsed-exp))
             (let ((bool (cond
                           [(eq? 2 (length (cadr parsed-exp)))
                            (if (eq? 'bool-exp (car (cadr parsed-exp)))
                                (if (eq? 't (cadr (cadr parsed-exp)))
                                    true
                                    false)
                            (do_bool (car (cadr parsed-exp))
                                           (process (cadr (cadr parsed-exp)))
                                           null))]
                           [(eq? 3 (length (cadr parsed-exp)))
                            (do_bool (car (cadr parsed-exp))
                                     (process (cadr (cadr parsed-exp)))
                                     (process (caddr (cadr parsed-exp))))]
                           [else (displayln "ERROR: Failed to interpret 'ask-exp.")])))
               (cond
                 [(void? bool) (displayln "Interpreter Error: failed to eval boolean expression.")]
                 [bool (cadr (cadr (process (caddr parsed-exp))))]
                 [else (cadr (cadr (process (cadddr parsed-exp))))]))]
            ;(bulk-exp
            ;  (math-exp + 1 2)
            ;  (var-exp a)
            ;)
            ;(bulk-exp)
            ;we will iterate every expressions in the list, and until it leaves no expression to execute
            ((eq? (car parsed-exp) 'bulk-exp)
                (
                    if (null? (cdr parsed-exp))
                        (list 'terminator-exp (list 'bulk-ret (void))) ;(bulk-exp)
                        (let
                            (
                                (ret_lst (map process (cdr parsed-exp)))
                            )
                            (list 'terminator-exp (list 'bulk-ret (list-ref ret_lst (- (length ret_lst) 1))))
                        )
                )
            )
            (else (displayln "ERROR: expression has not been supported yet."))
    )
    )
)

(provide (all-defined-out))
