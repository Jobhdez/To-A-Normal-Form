#lang racket

(provide (all-defined-out))

(struct atomic (atom) #:transparent)

(struct atomic-assignment (var expr) #:transparent)

(struct atomic-plus (e e2) #:transparent)
