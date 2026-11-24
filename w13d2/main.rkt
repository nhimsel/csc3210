#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")
(require "heap.rkt")


;; (define fib
;;   '(main
;;     (cls Math ()
;;          ((func fib (n)
;;                 ((ask (n == 1) ((return 1))
;;                      ((ask (n == 0) ((return 0))
;;                           ((return ((call (this -> fib) ((n - 1)))
;;                                    + (call (this -> fib) ((n - 2)))))))))))))
;;     (f = (new Math ()))
;;     (fire (call (f -> fib) (10)))))

(define show_error
  (lambda (test)
    (with-handlers ((exn:fail? (lambda (e) (displayln (exn-message e))))) (test))))

(define fib
  '(main
    (cls Node (val prev next) ())

    (cls Math (head tail size work)
         ((func seed ()
                ((a = (new Node (0 NULL NULL)))
                 (b = (new Node (1 a NULL)))
                 ((a -> next) = b)
                 ((this -> head) = a)
                 ((this -> tail) = b)
                 ((this -> size) = 2)))
          (func extend (upto)
                ((while ((this -> size) < (upto + 1))
                   (((this -> work) = ((this -> work) + 1))
                    (a = (new Node ((((this -> tail) -> val)
                                     + (((this -> tail) -> prev) -> val))
                                    (this -> tail)
                                    NULL)))
                    (((this -> tail) -> next) = a)
                    ((this -> tail) = a)
                    ((this -> size) = ((this -> size) + 1))))))
          (func get (i)
                ((cur = (this -> head))
                 (j = 0)
                 (while (j < i)
                   ((cur = (cur -> next))
                    (j = (j + 1))))
                 (return (cur -> val))))
          (func ensure (upto)
                ((ask ((this -> size) == 0)
                      ((call (this -> seed) ())
                       (call (this -> extend) (upto)))
                      ((call (this -> extend) (upto))))))
          (func fib (n)
                (((this -> work) = 0)
                 (call (this -> ensure) (n))
                 (return (call (this -> get) (n)))))))

    (f = (new Math (NULL NULL 0 0)))
    (g = f)

    (fire "fib(10):")
    (fire (call (g -> fib) (10)))
    (fire "work:")
    (fire (f -> work))

    (fire "")

    (fire "fib(15):")
    (fire (call (g -> fib) (15)))
    (fire "work:")
    (fire (f -> work))

    (fire "")


    (fire "fib(20):")
    (fire (call (g -> fib) (20)))
    (fire "work:")
    (fire (f -> work))

    (fire "")


    (fire "fib(15):")
    (fire (call (g -> fib) (15)))
    (fire "work:")
    (fire (f -> work))

    (fire "")

    (fire "fib(10):")
    (fire (call (g -> fib) (10)))
    (fire "work:")
    (fire (f -> work))

    (fire "")

    (fire "fib(5):")
    (fire (call (g -> fib) (5)))
    (fire "work:")
    (fire (f -> work))

    (fire "")

    (fire "fib(30):")
    (fire (call (g -> fib) (30)))
    (fire "work:")
    (fire (f -> work))

    (fire "")

    (fire "fib(50):")
    (fire (call (g -> fib) (50)))
    (fire "work:")
    (fire (f -> work))))

(process (parse fib))

(displayln "")

(define err1 '(f -> works))
(show_error (lambda () (process (parse err1))))

(displayln "")

(define err2 '(call (f -> add) ()))
(show_error (lambda () (process (parse err2))))

(displayln "")

(define err3 '(x = (new Math (NULL NULL 0 0 0))))
(show_error (lambda () (process (parse err3))))
