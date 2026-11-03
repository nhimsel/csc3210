#lang racket

; implemented euclid's algorithm both via loop and via recursion

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(define gcd-loop
  '(main
    (func gcd (x y)
          ((while (y > 0)
             ((temp = y)
              (y = (x % y))
              (x = temp)))
           (return x)))
    (fire (call gcd (48 18))) ; 6
    (fire (call gcd (17 5))) ; 1
    (fire (call gcd (100 75))))) ; 25

(define gcd-recur
  '(main
    (func gcd (x y)
          ((ask (y > 0)
                ((return (call gcd (y (x % y)))))
                ((return x)))))
    (fire (call gcd (48 18))) ; 6
    (fire (call gcd (17 5))) ; 1
    (fire (call gcd (100 75))))) ; 25

(process (parse gcd-loop))
(displayln "")
(process (parse gcd-recur))
