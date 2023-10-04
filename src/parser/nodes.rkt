#lang racket

(provide (all-defined-out))

(struct py-module (statements) #:transparent)

(struct py-print  (expr) #:transparent)

(struct py-assign (var expr) #:transparent)

(struct py-while (statements expr statements2) #:transparent)

(struct py-if (expr statements statements2) #:transparent)

(struct py-fun (name arguments statements) #:transparent)

(struct py-id (var) #:transparent)

(struct py-num (n) #:transparent)

(struct py-plus (n1 n2) #:transparent)

(struct py-minus (n1 n2) #:transparent)

(struct py-bool (b1) #:transparent)

(struct py-neg (n) #:transparent)
