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
      [(py-num n)
       (if (positive? n)
           (atomic ast)
           (a-normalize-negative-number ast))]

      [(py-id n)
       (atomic ast)]
      
      [(py-bool b)
       (atomic ast)]

      [(py-plus e e2)
       (a-normal-plus (to-normal-form e) (to-normal-form e2))]

      [(py-minus e e2)
       (a-normal-minus (to-normal-form e) (to-normal-form e2))]

      [(py-cmp e e2)
       (a-normal-compare (to-normal-form e) (to-normal-form e2))]

      [(py-if-exp cond-exp then-exp else-exp)
       (a-normalize-if-expression ast)]

      [(py-and e e2)
       (a-normal-and (to-normal-form e) (to-normal-form e2))]

      [(py-or e e2)
       (a-normal-or (to-normal-form e) (to-normal-form e2))]

      [(py-not e)
       (a-normal-not (to-normal-form e))]

      [(py-print e)
       (a-normal-print (to-normal-form e))]

      [(py-assign var e)
       (a-normal-assign (to-normal-form var) (to-normal-form e))]

      [_ (raise-argument-error 'Invalid-Exp-AST "ast?" ast)]))

  (map to-normal-form ast))

(define (a-normalize-negative-number ast)
  (match ast
    [(py-neg e)
     (let [(var-name (generate-name "temp_"))]
       (a-normal-assign (atomic (py-id var-name)) ast))]
    [_ (raise-argument-error 'Invalid-Exp "py-neg?" e)]))
