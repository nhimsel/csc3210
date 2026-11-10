#lang racket

; initial code from w11d2 assignment post
; reserve 'True and 'False as boolean literals
; modify 'var-exp, 'math-exp, 'boolean-exp, 'fire-exp
;        'while-exp, 'do-while-exp, 'ask-exp
; to function with the type of variables stored in the environment

(require "util.rkt")

;a return happened when the nearest #return slot is no longer #none
;outside of any function there is no slot, resolve_env gives void, so it never stops
(define
    has_returned
    (lambda ()
        (let ((ret (resolve_env environment '|#return|)))
            (not (or (void? ret) (eq? ret '|#none|))))
    )
)

;homework week5 day 1, 9/22
;create a function, that "process" the parser's results
;if it is a var-exp, resolve the value if can find, otherwise output void error
;no other expression are supported, so all will prompt errors
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
            [(equal? 'var-exp (car parsed-exp))
             (cond
               [(eq? (cadr parsed-exp) 'True) (list 'bool #t)]
               [(eq? (cadr parsed-exp) 'False) (list 'bool #f)]
               [else (resolve_env environment (cadr parsed-exp))])]
            [(eq? (car parsed-exp) 'string-exp) (list 'str (cadr parsed-exp))]
            ;('num-exp ())
            ((equal? 'num-exp (car parsed-exp)) (car (cdr parsed-exp)))
            ;'(assign-exp ((var-exp a) (num-exp 2)))
            ((equal? 'assign-exp (car parsed-exp))
                (wolf_env environment (cadr (car (cadr parsed-exp))) (process (cadr (cadr parsed-exp)))))
            [(eq? 'math-exp (car parsed-exp))   ;math-exp + (math-exp + ...) (num-exp 10)
             (list 'num (do_math
                         (cadr parsed-exp)
                         (check_type 'num (process (caddr parsed-exp)))
                         (check_type 'num (process (cadddr parsed-exp)))))]
            [(eq? 'boolean-exp (car parsed-exp))
                (cond
                    ;boolean True void void
                  [(void? (caddr parsed-exp))
                   (if (eq? 'True (cadr parsed-exp))
                       (list 'bool #t)
                       (list 'bool #f))]
                  [(void? (cadddr parsed-exp))
                   (list 'bool (not (cadr (process (caddr parsed-exp)))))]
                  [else
                   (list 'bool (calculate_boolean (cadr parsed-exp)
                                      (process (caddr parsed-exp))
                                      (process (cadddr parsed-exp))))])]
            ((eq? 'func-exp (car parsed-exp))
                (let
                    (
                        ;cons the parameter list with the original environment
                        ;(((var-exp a) (num-exp 1)) ((var-exp b) (var-exp a)))
                        (new_scope
                            (map (lambda (pair) (list (cadr (car pair)) (process (cadr pair)))) (cadr parsed-exp))
                        )
                        (expression_lst
                            (caddr parsed-exp))
                    )
                    (begin
                        ;every function owns a #return slot, initialized as #none (void means "not exists" in resolve)
                        (push_scope_to_environment (cons (list '|#return| '|#none|) new_scope))
                        (process expression_lst)
                        ;read the slot before the scope is popped
                        (define ret (resolve_env environment '|#return|))
                        (pop_scope_from_environment)
                        ;no return happened, the default return value is void
                        (if (eq? ret '|#none|) (void) ret)
                    )
                )
            )
            (
                (eq? 'ask-exp (car parsed-exp))
                    (if (check_type 'bool (process (cadr parsed-exp)))
                        (process (caddr parsed-exp))
                        (process (cadddr parsed-exp))
                )
            )
            ;create a scope, and add all the parameters and its value into the scope
            ;append the scope on top of the environment
            ;wrap the begin with each statement of the bulk-exp
            ;remove the scope when the execution completes
            ;done
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
            ((eq? (car parsed-exp) 'list-exp)
                ;walk through each statement, and check if it is return
                ;if it is a return, update the value of the varaible scope of the "#return" to be the value
                ;and it also stop the iterations of the execution
                ;if it is not a return, we simply run this expression, and make the iteration continues
                ;a return may also happen inside a nested ask/while, so check the #return slot after each statement
                (cond
                    ((null? (cdr parsed-exp)) (void))
                    (else
                        (begin
                            (process (cadr parsed-exp))
                            (if (has_returned)
                                (void)
                                (process (cons 'list-exp (cddr parsed-exp)))
                            )
                        )
                    )
                )
                ;(map process (cdr parsed-exp))
            )
            ((eq? (car parsed-exp) 'while-exp)
             (if (check_type 'bool (process (cadr parsed-exp)))
                 (begin
                   (push_scope_to_environment '())
                   (process (caddr parsed-exp)) ;body
                   (pop_scope_from_environment)
                   ;stop the loop when the body returned
                   (if (has_returned)
                       (void)
                       (process parsed-exp)    ;next while-loop
                       )
                   )
                 (void)
                 )
             )
            ((eq? (car parsed-exp) 'do-while-exp)
                (begin
                    (push_scope_to_environment '())
                    (process (cadr parsed-exp))
                    (pop_scope_from_environment)
                    (if (and (not (has_returned))
                             (check_type 'bool (process (caddr parsed-exp))))
                        (process parsed-exp)
                        (void)
                    )
                )
            )
            ((eq? (car parsed-exp) 'for-exp)
                (begin
                    (push_scope_to_environment '())
                    (process (cadr parsed-exp))
                    ;let's reassemble it into the manner as a while-loop
                    ;caddr -: boolean
                    ;cadddr -: step
                    ;caddddr -: body
                    (let
                        (
                            (new_exp
                                (list
                                    'while-exp
                                    (caddr parsed-exp)
                                    (append
                                        (cadr (cdddr parsed-exp))
                                        (cdr (cadddr parsed-exp))
                                    )
                                )
                            )
                        )
                        (process new_exp)
                    )
                    (pop_scope_from_environment)
                )

            )
            ;store the value into the nearest #return slot, which belongs to the current function
            ((eq? (car parsed-exp) 'return-exp)
                (wolf_env environment '|#return| (process (cadr parsed-exp))))
            [(eq? (car parsed-exp) 'fire-exp)
             (let ((resolved (process (cadr parsed-exp))))
               (cond
                 [(void? resolved) (displayln 'void)]
                 [(simple-exp? resolved)
                  (if (eq? (car resolved) 'bool)
                      (if (cadr resolved)
                          (displayln 'True)
                          (displayln 'False))
                      (displayln (cadr resolved)))]
                 [else (display "INTERPRETER ERROR: ")
                       (display "CANNOT FIRE ")
                       (displayln resolved)]))]

            ;'new-func-exp
            ;(map parse (caddr exp))
            ;(list
             ; 'list-exp
              ;(map parse (cadddr exp))
            ;)
          ;)
          ((eq? (car parsed-exp) 'new-func-exp) parsed-exp)
          ((eq? (car parsed-exp) 'call-exp)
           (let*
               (
                (func (resolve_env environment (cadr (cadr parsed-exp))))
                (function (list 'func-exp
                                (create_pair_list '() (cadr func) (caddr parsed-exp))
                                (caddr func)
                                ))
                )
             (process function)
             )
           )
          (else (displayln "ERROR: expression has not been supported yet."))
    ))
)

(provide (all-defined-out))
