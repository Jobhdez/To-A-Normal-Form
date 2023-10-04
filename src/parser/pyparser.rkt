#lang racket

(provide the-parser)

(require "nodes.rkt"
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)




(define-tokens value-tokens (NUM ID PLUS MINUS ASSIGN PRINT FUN AND OR NOT IF ELSE WHILE MUL EQUIV NOTEQUIV GREAT LESS LESSEQ GREATEQ TRUE FALSE COLON LPAREN RPAREN))

(define-empty-tokens op-tokens (EOF))

(define lexer/tokens
  (lexer
   [(eof) 'EOF]
   ["+"     (token-PLUS (string->symbol lexeme))]
   ["-"     (token-MINUS (string->symbol lexeme))]
   ["*"     (token-MUL (string->symbol lexeme))]
   ["="     (token-ASSIGN (string->symbol lexeme))]
   ["=="    (token-EQUIV (string->symbol lexeme))]
   ["!="    (token-NOTEQUIV (string->symbol lexeme))]
   ["<"     (token-LESS (string->symbol lexeme))]
   [">"     (token-GREAT (string->symbol lexeme))]
   ["<="    (token-LESSEQ (string->symbol lexeme))]
   [">="    (token-GREATEQ (string->symbol lexeme))]
   ["fun"   (token-FUN (string->symbol lexeme))]
   ["and"   (token-AND (string->symbol lexeme))]
   ["or"    (token-OR (string->symbol lexeme))]
   ["not"   (token-NOT (string->symbol lexeme))]
   ["False" (token-FALSE (string->symbol lexeme))]
   ["True"  (token-TRUE (string->symbol lexeme))]
   ["print" (token-PRINT (string->symbol lexeme))]
   ["if"    (token-IF (string->symbol lexeme))]
   ["else"  (token-ELSE (string->symbol lexeme))]
   ["while" (token-WHILE (string->symbol lexeme))]
   [":"     (token-COLON (string->symbol lexeme))]
   ["("     (token-LPAREN (string->symbol lexeme))]
   [")"     (token-RPAREN (string->symbol lexeme))]
   [(:+ numeric) (token-NUM (string->number lexeme))]
   [(:: (:or alphabetic #\_)
        (:* (:or alphabetic numeric #\_)))
    (token-ID (string->symbol lexeme))]
   [whitespace (lexer/tokens input-port)]))

(define the-parser
  (parser
   [start py-module]
   [end EOF]
   [error void]
   [tokens value-tokens op-tokens]
   [grammar
    [py-module [(statements) (py-module $1)]]
    [statements [(statement) $1]
                [(expr) $1]
                [(statement statements) (cons $1 $2)]
                [(expr statements) (cons $1 $2)]]
    [statement [(PRINT LPAREN expr RPAREN) (py-print $3)]
               [(expr) $1]
               [(ID ASSIGN expr) (py-assign (py-id $1) $3)]
               [(statements WHILE expr COLON statements)
                (py-while $1 $3 $5)]
               [(IF expr COLON statements ELSE COLON statements)
                (py-if $2 $4 $7)]
               [(FUN ID LPAREN args RPAREN COLON statements)
                (py-fun $2 $4 $7)]]
    [expr     [(ID) (py-id $1)]
              [(NUM) (py-num $1)]
              [(MINUS NUM) (py-neg $2)]
              [(expr PLUS expr) (py-plus $1 $3)]
              [(expr MINUS expr) (py-minus $1 $3)]
              [(bool) (py-bool $1)]]
    [bool     [(TRUE) $1]
              [(FALSE) $1]]
    [args    [(ID) $1]
             [(ID args) (list $1 $2)]]]))
