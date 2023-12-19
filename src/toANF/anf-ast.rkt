#lang racket

(provide (all-defined-out))

(struct atomic (atom) #:transparent)

(struct atomic-assignment (var expr) #:transparent)

(struct atomic-plus (e e2) #:transparent)

(struct atomic-print (e) #:transparent)

(struct atomic-minus (e e2) #:transparent)

(struct anf-bool (bool) #:transparent)

(struct anf-equiv (e e2) #:transparent)
