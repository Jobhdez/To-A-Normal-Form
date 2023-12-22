#lang racket

(define (make-tag tuple)
  (define (make-pointer-mask tuple)
    ;; for testing purposes `tuple` will be a list until I implement
    ;; tuples and the subsequent passes
    (define str "")
    (for ([i tuple])
      (if (number? i)
          (set! str (string-append str "0"))
          (set! str (string-append str "1"))))
    (reverse-bit str))
  
  (define (tuple-length->bits len)
    (let ((binary-str (number->string len 2)))
      (string-append
       (make-string (- 6 (string-length binary-str)) #\0)
       binary-str)))

  (define (tag-bits->decimal bits)
    (string->number bits 2))

  (define (reverse-bit bits)
    (list->string (reverse (string->list bits))))

  (define len (tuple-length->bits (length tuple)))
  (define pointer-mask (make-pointer-mask tuple))
  (define fwd-ptr "1")
  
  (tag-bits->decimal (string-append pointer-mask len fwd-ptr)))
