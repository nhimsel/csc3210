#lang racket

; initial code from w9d2 homework post
; support 'func-def-exp, 'call-exp, 'return-exp
; rewrite 'list-exp, 'while-exp, 'do-while-exp,
; 'for-exp, 'ask-exp to break on return
; rewrite 'math-exp to not process left and right twice

(require "util.rkt")

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
            ((equal? 'var-exp (car parsed-exp)) (resolve_env environment (cadr parsed-exp)))
            ((equal? 'num-exp (car parsed-exp)) (car (cdr parsed-exp)))
            ;'(assign-exp ((var-exp a) (num-exp 2)))
            ((equal? 'assign-exp (car parsed-exp))
                (wolf_env environment (cadr (car (cadr parsed-exp))) (process (cadr (cadr parsed-exp)))))
            [(eq? (car parsed-exp) 'math-exp)
             (let ((left (process (caddr parsed-exp)))
                   (right (process (cadddr parsed-exp))))
               (if (and (number? left) (number? right))
                   (do_math (cadr parsed-exp) left right)
                   (displayln
                    "INTERPRETER ERROR: non-numeric cannot apply math.")))]
            ((eq? 'boolean-exp (car parsed-exp))
                (cond
                    ;boolean True void void
                    ((void? (caddr parsed-exp)) (if (eq? 'True (cadr parsed-exp)) #t #f))
                    ;boolean ! (var-exp a) void
                    ((void? (cadddr parsed-exp)) (not (process (caddr parsed-exp))))
                    ;boolean > (num-exp 1) (var-exp a)
                    (else (calculate_boolean (cadr parsed-exp) (process (caddr parsed-exp)) (process (cadddr parsed-exp))))
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
                        (set! ret_val (cadr (cadr (process expression_lst))))
                        (update_base_environment (cdr environment))
                        ret_val
                    )
                )
            )
            (
                (eq? 'ask-exp (car parsed-exp))
                    (if (process (cadr parsed-exp))
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
            [(eq? (car parsed-exp) 'list-exp)
             (let ((last (void)))
               (let eval ((exp (cdr parsed-exp)))
                 (cond
                   [(null? exp) (void)]
                   [else (let ((tmp (process (car exp))))
                           (if (and (pair? tmp)
                                    (eq? (car tmp) 'return-val)) 
                               (set! last tmp)
                               (eval (cdr exp))))]))
               last)]
            [(eq? (car parsed-exp) 'while-exp)
                (if (process (cadr parsed-exp))
                    (let ((ret (begin
                                 (push_scope_to_environment)
                                 (let ((r (process (caddr parsed-exp))))
                                   (pop_scope_from_environment)
                                   r))))
                      (if (and (pair? ret) (eq? (car ret) 'return-val))
                          ret
                          (process parsed-exp)))
                    (void))]
            [(eq? (car parsed-exp) 'do-while-exp)
             (push_scope_to_environment)
             (let ((ret (process (cadr parsed-exp))))
               (pop_scope_from_environment)
               (cond
                 [(and (pair? ret) (eq? (car ret) 'return-val))
                  ret]
                 [(process (caddr parsed-exp))
                  (process parsed-exp)]
                 [else
                  (void)]))]
            ((eq? (car parsed-exp) 'for-exp)
                (begin
                    (push_scope_to_environment)
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
            ((eq? (car parsed-exp) 'fire-exp)
                (displayln (process (cadr parsed-exp))))
            [(eq? (car parsed-exp) 'main-exp)
             (push_scope_to_environment)
             (process (cadr parsed-exp))
             (pop_scope_from_environment)]
            [(eq? (car parsed-exp) 'func-def-exp)
             (wolf_env environment
                       (cadr (cadr parsed-exp))
                       (list 'func-val
                              (caddr parsed-exp)
                              (cadddr parsed-exp)))]
            [(eq? (car parsed-exp) 'call-exp)
             (let ((resolved (resolve_env environment (cadr (cadr parsed-exp)))))
               (if (and (list? resolved)
                        (eq? (car resolved) 'func-val))
                   (if (eq? (length (cadr resolved))
                            (length (caddr parsed-exp)))
                       (begin
                         (let pairify ((pairs null)
                                       (vars (cadr resolved))
                                       (vals (caddr parsed-exp)))
                           (if (null? vars)
                               (update_base_environment (cons pairs environment))
                               (pairify
                                (append pairs
                                        (list (list (cadar vars)
                                                    (process (car vals)))))
                                (cdr vars)
                                (cdr vals))))
                         (let ((ret (process (caddr resolved))))
                           (pop_scope_from_environment)
                           (if (and (pair? ret)
                                    (eq? (car ret) 'return-val))
                               (cadr ret)
                               ret)))
                       (displayln
                        "INTERPRETER ERROR: wrong number of arguments"))
                   (displayln "INTERPRETER ERROR: not a function")))]
            [(eq? (car parsed-exp) 'return-exp)
             (list 'return-val (process (cadr parsed-exp)))]
            (else (displayln "ERROR: expression has not been supported yet."))
    )
    )
)

(provide (all-defined-out))
