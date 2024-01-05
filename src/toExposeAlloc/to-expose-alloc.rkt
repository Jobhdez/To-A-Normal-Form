#lang racket

(provide syntax->expose)

(require "../parser/pyparser.rkt"
         "../parser/nodes.rkt")

(define (syntax->expose ast)
  (map syntax->expose* (py-module-statements ast)))

(define (syntax->expose* ast)
  (define counter (make-parameter 0))

  (define (generate-temp-var name)
    (let* [(current-counter (counter))
           (name* (string-append name (number->string current-counter)))]
      (counter (+ current-counter 1))
      name*))

  (define (generate-temp-v name)
    (let* [(current-counter (counter))
           (name* (string-append name (number->string current-counter)))]
      (counter (+ current-counter 1))
      name*))
  
  (match ast
    [(py-tuple exps)
     (define bytes* (+ (* (length exps) 8) 8))
     (define assignments (for/list ([e exps])
                           (py-assign (generate-temp-var "x") e)))
     (define if-exp (py-if-exp (py-cmp (py-less (py-plus (global-value (py-id "free_ptr")) (py-num bytes*)) (global-value (py-id "fromspace_end"))))
                               (py-num 0)
                               (collect (py-num bytes*))))
     (define v (py-id (generate-temp-v "v")))
     (define alloc-assign (py-assign v

                                     (allocate (length exps) (TupleType (py-id "int")))))
     
     (define assigns (for/list ([i assignments]
                                [index (in-naturals)])
                       (py-assign (py-tuple-index v index)
                                  (py-assign-var i))))
     (begin-expose (list assignments if-exp alloc-assign assigns v))]
    [_ ast]))

;;; ast
(struct global-value (e) #:transparent)
(struct allocate (e e2) #:transparent)
(struct collect (e) #:transparent)
(struct TupleType (id) #:transparent)
(struct begin-expose (exps) #:transparent)
