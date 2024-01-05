#lang racket

(provide parse-expression
         the-parser
         the-lexer/tokens)

(require "nodes.rkt"
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))



#|
The concrete syntax of the language I will compile looks something like the following grammar:

Grammar:

exp ::= int | input_int() | - exp | exp + exp | exp - exp | (exp)
stmt ::= print(exp) | exp
exp ::= var
stmt ::= var = exp
cmp ::= == | != | < | <= | > | >=
exp ::= True | False | exp and exp | exp or exp | not exp
| exp cmp exp | exp if exp else exp
stmt ::= if exp: stmt+ else: stmt+
stmt ::= while exp: stmt+
cmp ::= is
exp ::= exp, … ,exp | exp[int] | len(exp)
type ::= int | bool | void | tuple[type+] | Callable[[type, … ], typ
exp ::= exp(exp, … )
stmt ::= return exp
def ::= def var(var:type, … ) -> type: stmt+
exp ::= lambda var, … : exp | arity(exp)
stmt ::= var : type = exp
LFun ::= def … stmt …
|#

(define (parse-expression expr)
  (define p (open-input-string expr))
  (the-parser (lambda () (the-lexer/tokens p))))

(define-tokens value-tokens (NUM ID PLUS MINUS ASSIGN PRINT DEF AND OR NOT IF ELSE WHILE MUL EQUIV NOTEQUIV GREATER LESS LESSEQ GREATEREQ TRUE FALSE COLON LPAREN RPAREN SEMICOLON RBRACKET LBRACKET LEN THEN))

(define-empty-tokens op-tokens (EOF))

(define the-lexer/tokens
  (lexer
   [(eof) 'EOF]
   ["+"     (token-PLUS (string->symbol lexeme))]
   ["-"     (token-MINUS (string->symbol lexeme))]
   ["*"     (token-MUL (string->symbol lexeme))]
   ["="     (token-ASSIGN (string->symbol lexeme))]
   ["=="    (token-EQUIV (string->symbol lexeme))]
   ["!="    (token-NOTEQUIV (string->symbol lexeme))]
   ["<"     (token-LESS (string->symbol lexeme))]
   [">"     (token-GREATER (string->symbol lexeme))]
   ["<="    (token-LESSEQ (string->symbol lexeme))]
   [">="    (token-GREATEREQ (string->symbol lexeme))]
   ["def"   (token-DEF (string->symbol lexeme))]
   ["and"   (token-AND (string->symbol lexeme))]
   ["or"    (token-OR (string->symbol lexeme))]
   ["not"   (token-NOT (string->symbol lexeme))]
   ["False" (token-FALSE (string->symbol lexeme))]
   ["True"  (token-TRUE (string->symbol lexeme))]
   ["print" (token-PRINT (string->symbol lexeme))]
   ["then" (token-THEN (string->symbol lexeme))]
   ["if"    (token-IF (string->symbol lexeme))]
   ["else"  (token-ELSE (string->symbol lexeme))]
   ["while" (token-WHILE (string->symbol lexeme))]
   ["len"   (token-LEN (string->symbol lexeme))]
   [":"     (token-COLON (string->symbol lexeme))]
   ["("     (token-LPAREN (string->symbol lexeme))]
   [")"     (token-RPAREN (string->symbol lexeme))]
   [";"     (token-SEMICOLON (string->symbol lexeme))]
   ["["     (token-LBRACKET (string->symbol lexeme))]
   ["]"     (token-RBRACKET (string->symbol lexeme))]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: (:or alphabetic #\_)
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (the-lexer/tokens input-port)]))

(define the-parser
  (parser
   [start py-module]
   [end EOF]
   [error void]
   [tokens value-tokens op-tokens]
   [grammar
    [py-module [(statements) (py-module (flatten (list $1)))]]
    [statements [(statement) $1]
                [(expr) $1]
                [(statement statements) (list $1 $2)]
                [(expr statements) (list $1 $2)]]
    [statement [(PRINT LPAREN expr RPAREN) (py-print $3)]
               [(expr) $1]
               [(ID ASSIGN expr) (py-assign (py-id $1) $3)]
               [(WHILE expr COLON statements SEMICOLON)
                (py-while $2 $4)]
               [(IF expr COLON statements ELSE COLON statements)
                (py-if $2 $4 $7)]
               [(DEF ID LPAREN args RPAREN COLON statements)
                (py-fun $2 $4 $7)]]
    [expr     [(ID) (py-id $1)]
              [(NUM) (py-num $1)]
              [(IF expr THEN expr ELSE expr)
               (py-if-exp $2 $4 $6)]
              [(MINUS NUM) (py-neg $2)]
              [(expr PLUS expr) (py-plus $1 $3)]
              [(expr MINUS expr) (py-minus $1 $3)]
              [(expr EQUIV expr)
               (py-cmp (py-equiv $1 $3))]
              [(expr NOTEQUIV expr)
               (py-cmp (py-not-equiv $1 $3))]
              [(expr LESS expr)
               (py-cmp (py-less $1 $3))]
              [(expr LESSEQ expr)
               (py-cmp (py-less-eq $1 $3))]
              [(expr GREATER expr) 
               (py-cmp (py-greater $1 $3))]
              [(expr GREATEREQ expr)
               (py-cmp (py-greater-eq $1 $3))]
              [(TRUE) (py-bool $1)]
              [(FALSE) (py-bool $1)]
              [(expr AND expr) (py-and $1 $3)]
              [(expr OR expr) (py-or $1 $3)]
              [(NOT expr) (py-not $2)]
              [(LPAREN elements RPAREN) (py-tuple $2)]
              [(expr LBRACKET NUM RBRACKET)
               (py-tuple-index $1 $3)]
              [(LEN LPAREN expr RPAREN)
                (py-tuple-len $3)]]
    [elements [(expr) (list $1)]
              [(expr elements) (cons $1 $2)]]
    [args    [(ID) (py-id $1)]
             [(ID args) (cons (py-id $1) $2)]]]))
