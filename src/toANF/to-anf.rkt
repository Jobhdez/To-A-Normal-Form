#lang racket

(provide syntax-ast-to-anf)

(require "../parser/pyparser.rkt"
         "../parser/nodes.rkt"
         "anf-ast.rkt")


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

(define (syntax-ast-to-anf ast)

  (define counter (make-parameter 0))

  (define (generate-temp-name name)
    (let* [(current-counter (counter))
           (name* (string-append name (number->string current-counter)))]
      (counter (+ current-counter 1))
      name*))
  
  (define (to-anf ast)
    (match ast
      [(py-num n)
       (atomic ast)]
      
      [(py-neg n)
       (atomic-assignment (atomic (py-id (generate-temp-name "temp_")))
                          (atomic ast))]
      [(py-id n)
       (atomic ast)]
      
      [(py-bool b)
       (atomic ast)]

      [(py-plus e e2)
       (match* (e e2)
         [((? py-num? a) (? py-neg? b))
          (let [(a-normal-num (to-anf a))
                (a-normal-neg (to-anf b))]
            (match a-normal-neg
              [(atomic-assignment var e3)
               (plusNegSeq a-normal-neg (atomic-plus a-normal-num var))]))]
         
         [((? py-neg? a) (? py-num? b))
          (let [(anf-neg (to-anf a))
                (anf-num (to-anf b))]
            (match anf-neg
              [(atomic-assignment var e3)
               (plusNegSeq anf-neg (atomic-plus anf-num var))]))]
           
         [((? py-num? a) (? py-num? b))
          (atomic-plus (to-anf a) (to-anf b))]

         [(_ _) (atomic-plus (to-anf e) (to-anf e2))])]

      [(py-minus e e2)
       (match* (e e2)
         [((? py-num? a) (? py-neg? b))
          (let [(anf-num (to-anf a))
                (anf-neg (to-anf b))]
            (match anf-neg
              [(atomic-assignment var e3)
               (minusNegSeq anf-neg (atomic-minus anf-num var))]))]
         
         [((? py-neg? a) (? py-num? b))
          (let [(anf-neg (to-anf a))
                (anf-num (to-anf b))]
            (match anf-neg
              [(atomic-assignment var e3)
               (minusNegSeq anf-neg (atomic-minus anf-num var))]))])]

      [(py-cmp e)
       (match e
         [(py-equiv (? py-bool? b1) (? py-bool? b2))
          (anf-equiv (to-anf b1) (to-anf b2))]

         [(py-equiv (? py-num? n1) (? py-num? n2))
          (anf-equiv (to-anf n1) (to-anf n2))]

         [(py-less (? py-bool? b1) (? py-bool? b2))
          (anf-equiv (to-anf b1) (to-anf b2))])]

      [(py-if-exp cond-exp then-exp else-exp)
       "Not Implemented"]

      [(py-and e e2)
       "Not Implemented"]

      [(py-or e e2)
       "not implemented"]

      [(py-not e)
       (match e
         [(? py-bool? bool)
          (anf-bool (to-anf bool))])]

      [(py-print e)
       (let* [(temp-var-name (generate-temp-name "temp_"))
              (anf-var (to-anf (py-id temp-var-name)))]
         (match e
           [(py-plus a b)
            (match* (a b)
              [((? py-num? aa) (? py-num? bb))
               (printSeq (atomic-assignment anf-var (to-anf e))
                         (atomic-print anf-var))]

              [((? py-num? aa) (? py-neg? bb))
               (let [(sum-neg-seq-anf (to-anf e))]
                 (match sum-neg-seq-anf
                   [(plusNegSeq (atomic-assignment var e2) (atomic-plus a-normal-num var))
                    (let [(intermediary (atomic (py-id (generate-temp-name "temp_"))))]
                      (list (atomic-assignment var e2)
                            (printSeq (atomic-assignment intermediary
                                                         (atomic-plus a-normal-num var))
                                      (atomic-print intermediary))))]))])]

           [(? py-neg? a)
            (let [(neg-anf (to-anf a))]
              (match neg-anf
                [(atomic-assignment var e)
                 (printSeq neg-anf (atomic-print var))]))]
           
           [(? py-num? a)
            (atomic-print (to-anf a))]))]

      [(py-assign var e)
       (let [(atomic-var (to-anf var))
             (atomic-exp (to-anf e))]
         (match atomic-exp
           [(plusNegSeq x y)
            (atomicSeq x (atomic-assignment atomic-var y))]

           [(atomic-plus (? atomic? a) (? atomic? b))
            (atomic-assignment atomic-var atomic-exp)]

           [(minusNegSeq x y)
            (atomicSeq x (atomic-assignment atomic-var y))]))]

      [_ (raise-argument-error 'Invalid-Exp-AST "ast?" ast)]))

  (flatten (map to-anf (py-module-statements ast))))

(struct plusNegSeq (assignment plus) #:transparent)

(struct atomicSeq (e e2) #:transparent)

(struct minusNegSeq (assignment minus) #:transparent)

(struct printSeq (assignment printstm) #:transparent)
