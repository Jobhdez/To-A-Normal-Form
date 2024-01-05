#lang racket

(provide (all-defined-out))

(struct py-module (statements) #:transparent)

(struct py-statements (statements) #:transparent)

(struct py-print  (expr) #:transparent)

(struct py-assign (var expr) #:transparent)

(struct py-while (expr statements) #:transparent)

(struct py-if (expr statements statements2) #:transparent)

(struct py-fun (name arguments statements) #:transparent)

(struct py-id (var) #:transparent)
(struct py-num (n) #:transparent)

(struct py-plus (n1 n2) #:transparent)

(struct py-minus (n1 n2) #:transparent)

(struct py-bool (b1) #:transparent)

(struct py-neg (n) #:transparent)

(struct py-cmp (e) #:transparent)

(struct py-and (e e2) #:transparent)

(struct py-or (e e2) #:transparent)

(struct py-not (e) #:transparent)

(struct py-tuple (t) #:transparent)

(struct py-tuple-index (e index) #:transparent)

(struct py-tuple-len (t) #:transparent)

(struct py-if-exp (if-exp cond-exp else-exp) #:transparent)

(struct py-greater (e e2) #:transparent)

(struct py-greater-eq (e e2) #:transparent)

(struct py-less (e e2) #:transparent)

(struct py-less-eq (e e2) #:transparent)

(struct py-equiv (e e2) #:transparent)

(struct py-not-equiv (e e2) #:transparent)
