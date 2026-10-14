#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(process (parse 'a))
(process (parse 'b))
(parse '(for (((a = 7)) (a < 10) ((a = (a + 1)))) ((b = (b + 1)))))
(process (parse '(for (((a = 0)) (a < 10) ((a = (a + 1)))) ((b = (b + 1))))))
(process (parse 'a))
(process (parse 'b))
