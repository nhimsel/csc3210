#lang racket
(require "parser.rkt")
(require "util.rkt")
(require "interpreter.rkt")

(parse '(function (params a b) (math a + b) (4 5)))
(process (parse '(function (params a b) (math a + b) (4 5)))) ; -> 9
