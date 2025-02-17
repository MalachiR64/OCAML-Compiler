{
    open Parser
}

(* regular expression *)
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+ 

(* FSA *)
rule read =
parse
| white {read lexbuf}
| "*" {TIMES}
| "+" {PLUS}
| "/" {DIV}
| "-" {MINUS}
| "<" {LE}
| ">=" {GEQ}
| ">" {GE}
| "<=" {LEQ}
| "&&" {AND}
| "||" {OR}
| "(" {LPAREN}
| ")" {RPAREN}
| "[" { LBRACKET }
| "]" { RBRACKET }
| "let" {LET}
| "in" {IN}
| "=" {EQ}
| "if" {IF}
| "else" {ELSE}
| "then" {THEN}
| "true" {TRUE}
| "false" {FALSE}
| "fun" {FUN}
| "match" {MATCH}
| "with" {WITH}
| "|" {MID}
| "[]" {NIL}
| "::" {CONS}
| "int" {TINT}
| "bool" {TBOOL}
| ":" {COLON}
| "->" {ARROW}
| "list" {LIST}
| int {INT (int_of_string (Lexing.lexeme lexbuf))}
| id  {VAR (Lexing.lexeme lexbuf)}
| eof {EOF}



