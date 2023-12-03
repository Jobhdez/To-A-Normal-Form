# serotonin
will be a compiler for a high level language. I did a similar one in common lisp: https://github.com/jobhdez/zettapy but I want to make one that is better.

So far, as of 10/4/23, you can parse programs like this:

```racket
pyparser.rkt> (define p3 (open-input-string "x = 4 y = 5 z = 4 if True then z + x else z+y"))
pyparser.rkt> (the-parser (lambda () (the-lexer/tokens p3)))
(py-module
 (list
  (py-assign (py-id 'x) (py-num 4))
  (py-assign (py-id 'y) (py-num 5))
  (py-assign (py-id 'z) (py-num 4))
  (py-if-exp (py-bool 'True) (py-plus (py-id 'z) (py-id 'x)) (py-plus (py-id 'z) (py-id 'y)))))
```
