#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(parse '(function (a b) ((a * b)) (4 5)))
(parse '(function (a b c) ((a * (b * c))) (4 5 10)))

(process (parse '(function (a b) ((a * b)) (4 5))))
(process (parse '(function (a b c) ((a * (b * c))) (4 5 10))))
