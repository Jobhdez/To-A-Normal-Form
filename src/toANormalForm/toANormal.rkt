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
;;; Examples:
;;;     x = 10 + -50
;;;     y = 4 + -60
;;;     z = 4 + 5
;;;     k = 5 + -3
;;;     s = x + y
;;;     u = z + k
;;;     print(s + u)
;;;     --->
;;;     --->
;;;     temp = -50
;;;     x = 10 + temp
;;;     temp2 = -60
;;;     y = 4 + temp2
;;;     z = 4 + 5
;;;     temp3 = -3
;;;     k = 5 + temp3
;;;     s = x + y
;;;     u = z + k
;;;     temp4 = s + u
;;;     print(temp4)
;;;
;;; Example 2:
;;;     10 + -30
;;;     --->
;;;     --->
;;;     temp = -30
;;;     10 + temp 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (program-to-a-normal-form ast)

  (define counter (make-parameter 0))

  (define (generate-temp-name name)
    (let* [(current-counter (counter))
           (name* (string-append name (number->string current-counter)))]
      (counter (+ current-counter 1))
      name*))
  
  (define (to-normal-form ast)
    (match ast
      [(py-num n)
       (atomic ast)]
      
      [(py-neg n)
       (atomic-assignment (atomic (py-id (generate-temp-name "temp_")))
                         ast)]

      [(py-id n)
       (atomic ast)]
      
      [(py-bool b)
       (atomic ast)]

      [(py-plus e e2)
       (match* (e e2)
         [((? py-num? a) (? py-neg? b))
          (a-normal-plus (to-normal-form a) (to-normal-form b))]

         [((? py-num? a) (? py-num? b))
          (a-normal-plus (to-normal-form a) (to-normal-form b))])]

      [(py-minus e e2)
       (a-normal-minus (to-normal-form e) (to-normal-form e2))]

      [(py-cmp e)
       (a-normal-compare (to-normal-form e))]

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

  (flatten (map to-normal-form (py-module-statements ast))))
       
(define (a-normal-assign var e)
  (match e
    [(list (atomic-assignment var2 (py-neg expr))
           (atomic-assignment var3 (atomic (py-num expr2))))
     (list (first e) (atomic-assignment var (atomic-plus var3 (atomic (py-num expr2)))))]

    [(atomic-plus expr expr2)
     (atomic-assignment var e)]
     
    [_ (raise-argument-error 'Invalid-Exp "ast?" e)]))

(define (a-normal-plus e e2)
  (match e2
    [(atomic-assignment var  (py-neg n))
     (match e
       [(atomic (py-num n2))
        (list e2 (atomic-assignment var (atomic (py-num n2))))])]
    
    [(atomic (py-num n))
     (match e
       [(atomic (py-num n2))
        (atomic-plus e e2)])]
    
    [_ "hello"]))

(define (a-normal-minus e e2)
  "hello")

(define (a-normal-compare e)
  "hello")

(define (a-normalize-if-expression c e e2)
  "hello")

(define (a-normal-and e e2)
  "hello")

(define (a-normal-or e e2)
  "hello")

(define (a-normal-not e)
  "hello")

(define (a-normal-print e)
  "hello")
