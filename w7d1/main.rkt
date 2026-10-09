#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(parse '(ask ((a + 2) > 7) ((a / 7)) ((function (a b) (a / b) (0 1)))))
(process (parse '(ask ((a + 2) < 7) ((a / 7)) ((function (a b) (a / b) (0 1))))))

(process (parse 'a))

(parse '(while (a < 10) ((a = (a + 1)))))
(process (parse '(while (a < 10) ((a = (a + 1))))))
(process (parse 'a))

(parse '(do ((a = (a + 1))) while (a < 1)))
(process (parse '(do ((a = (a + 1))) while (a < 1))))
(process (parse 'a))
