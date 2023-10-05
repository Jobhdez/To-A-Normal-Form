# serotonin
will be a compiler for a high level language. I did a similar one in common lisp: https://github.com/jobhdez/zettapy but I want to make one that is better.

So far, as of 10/4/23, you can parse programs like this:

```racket
> (require "pyparser.rkt")
> (define p5 (open-input-string "True and True; if True and False; else 4"))
> (the-parser (lambda () (the-lexer/tokens p5)))
(py-module
 (py-if-exp
  (py-and (py-bool 'True) (py-bool 'True))
  (py-and (py-bool 'True) (py-bool 'False))
  (py-num 4)))
```
