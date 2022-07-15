#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)  
         syntax/readerr)

(port-count-lines-enabled #t)

(define-lex-abbrevs
  [letter     (:or (:/ "a" "z") (:/ #\A #\Z) )]
  [number      (:/ #\0 #\9)])


(define-tokens value-tokens (NUMBER IDENTIFIER STRING READ WRITE IF ADDOP LPAR RPAR))


(define-empty-tokens op-tokens (newline :=  = < > + - * / ^  EOF))


(define lex
  (lexer-src-pos
   ["$$"                                      
    'EOF]                                            

   [whitespace                   
    (return-without-pos (lex input-port))]                                              
                                                    
   [(:or ":=" "*" "/" "^" "=" ";")       
    (string->symbol lexeme)]                         

   ["read"      
    (token-READ (string->symbol lexeme))]

   ["write"      
    (token-WRITE (string->symbol lexeme))]

    ["if"      
    (token-IF (string->symbol lexeme))]
   
   [(:or "+" "-" )     
    (token-ADDOP (string->symbol lexeme))]

   [(:or "(" "{")     
    (token-LPAR (string->symbol lexeme))]

   [(:or ")" "}")       
    (token-RPAR (string->symbol lexeme))]
   
   [(:+ letter)                                       
    (token-IDENTIFIER (string->symbol lexeme))]
   
   [(:+ number)                                       
    (token-NUMBER (string->number lexeme))]))        



(define (string->tokens s)
  (port->tokens (open-input-string s)))

(define (port->tokens in)
  (define token (lex in))
  (if (eq? (position-token-token token) 'EOF)
      '()
      (cons token (port->tokens in))))

(map position-token-token (port->tokens (open-input-file "input5.txt")))

  
(define (match token_list expected)
  (cond [(eq? expected (first token_list)) (rest token_list)]
        [else "parse_error"]))

(define parse_error "Syntax error found.")

(define (program token_list)
  (cond
    [(eq? `LPAR (first token_list)) (match `$$ (match (stmt_list (match token_list `LPAR)) `RPAR))]
    [else parse_error]))

(define (stmt_list token_list)
  (cond
    [(eq? `IDENTIFIER (first token_list)) (match 'RPAR (match token_list `IDENTIFIER))]
    [(eq? `IF (first token_list)) (match 'RPAR (match token_list `IF))]
    [(eq? `READ (first token_list)) (match 'RPAR (match token_list `READ))]
    [(eq? `WRITE (first token_list)) (match 'RPAR  (match token_list `WRITE))]
    [else parse_error]))

(define (stmt token_list)
  (cond
    [(eq? `IDENTIFIER (first token_list)) (match (stmt_list (stmt (match token_list `IDENTIFIER))))]
    [(eq? `IF (first token_list)) (match (stmt_list (stmt (match token_list `IF))))]
    [(eq? `READ (first token_list)) (match (stmt_list (stmt (match token_list `READ))))]
    [(eq? `WRITE (first token_list)) (match (stmt_list (stmt (match token_list `WRITE))))]
    [else parse_error]))

(define (expr token_list)
  (cond
    [(eq? `IDENTIFIER (first token_list)) (match (etail (id (match token_list `IDENTIFIER))))]
    [(eq? `NUMBER (first token_list)) (match (etail (id (match token_list `NUMBER))))]
    [(eq? `ADDOP (first token_list)) (match (etail (id (match token_list `ADDOP))))]
    [else parse_error]))

(define (etail token_list)
  (cond
    [(eq? `ADDOP (first token_list)) (match (expr (numsign (match token_list `ADDOP))))]
    [else parse_error]))

(define (id token_list)
  (cond
    [(eq? `IDENTIFIER (first token_list)) (match numsign (match token_list `IDENTIFIER))]
    [else parse_error]))

(define (num token_list)
  (cond
    [(eq? `NUMBER (first token_list)) (match token_list `NUMBER)]
    [else parse_error]))

(define (numsign token_list)
  (cond
    [(eq? `ADDOP (first token_list)) (match token_list `ADDOP)]
    [else parse_error]))


(define (parse filename)
  (define token_list (port->tokens))
  (close-input-port)

  (cond
    [(empty? (program token_list filename))
     (printf "accept")])
)