#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(parse '(ask (1 > 2) (true) (false)))
(parse '(ask (a > b) ((2 * a)) ((2 * b))))
(parse '(ask (true) ((1 + 2)) ((2 + 3))))
(parse '(ask ((function (a b c) (a * (b / c)) (a b 8)) > 2) (a) ((8 / b))))
(parse '(ask (true) ((2 + 3) a) ((2 + 3) b)))

(process (parse '(ask (1 > 2) (true) (false))))
(process (parse '(ask (a > b) ((2 * a)) ((2 * b)))))
(process (parse '(ask (! true) ((1 + 2)) ((2 + 3)))))
(process (parse '(ask (true) ((1 + 2)) ((2 + 3)))))
(process (parse '(ask ((function (a b c) (a * (b / c)) (a b 8)) > 2) (a) ((8 / b)))))
(process (parse '(ask (true) ((2 + 3) a) ((2 + 3) b))))
