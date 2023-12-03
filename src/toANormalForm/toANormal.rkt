#lang racket

(provide program-to-a-normal-form)

(require "../parser/pyparser.rkt"
         "../parser/nodes.rkt"
         "a-nodes.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; this module takes the AST for the parse tree and turns it into
;;; a-normal-form.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (program-to-a-normal-form ast)
  (define (to-normal-form ast)
    (match ast
      ...))
  ...)

(define (a-normalize-assignment ast)
  ...)

(define (a-normalize-if-expression ast)
  ...)

(define (a-normalize-while-loop-expression ast)
  ...)

(define (a-normalize-if-statement ast)
  ...)

(define (a-normalize-function ast)
  ...)

(define (atomic-exp? ast)
  ...)
