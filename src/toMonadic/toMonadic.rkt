#lang racket

(provide to-monadic)

(require "../parser/pyparser.rkt"
         "../parser/nodes.rkt")

(define (to-monadic ast)
  (define (to-monadic-map ast)
    (map to-monadic  (py-module-statements ast)))
  (match ast
    [(py-assign (py-id a) (py-num n))
     (if (positive? n)
         (py-atomic-assign (py-atomic a) (py-atomic n))
         (negative-assignment-to-monadic ast))]
    [(py-if-exp (py-bool b) (py-plus (py-id x) (py-id y)))
     (if-exp-to-monadic ast)])

  (to-monadic-map ast))
