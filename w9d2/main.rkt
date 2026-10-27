#lang racket

; test cases from w9d2 assignment post

(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((a = 5) (b = (a * 2)) (fire b))))) ; -> 10

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main (a = 1)))) ; -> parser error

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func sum_seq (a b s) ((c = (((a + b) * (((b - a) / s) + 1)) / 2)) (return c))) (x = (call sum_seq (1 99 2))) (fire x) (fire a) (fire c))))) ; -> 2500, 1, #<void>

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func hello (n) ((fire n))) (y = (call hello (7))) (fire y))))) ; -> 7, #<void>

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func first (n) ((return n) (fire 999))) (fire (call first (3))))))) ; -> 3

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func sum_seq (a b s) ((return a))) (call sum_seq (1 2)))))) ; -> interpreter error

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((q = 5) (call q (1)))))) ; -> interpreter error

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((k = 10) (func addone (k) ((return (k + 1)))) (fire (call addone ((k * 2)))) (fire k))))) ; -> 21, 10

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func fact (n) ((ask (n < 2) ((return 1)) ((return (n * (call fact ((n - 1))))))))) (fire (call fact (5))))))) ; -> 120

(display "\n")

(update_base_environment '(((a 1) (b 2))))
(process (parse '(main ((func find (lim) ((i = 0) (while (i < 100) ((i = (i + 1)) (ask (i == lim) ((return (i * 10))) ((i = i))))) (return -1))) (fire (call find (4))) (fire (call find (500))))))) ; -> 40, -1
