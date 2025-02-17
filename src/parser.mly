%{
open Ast
%}

(* token declarations *)
%token PLUS 
%token TIMES 
%token LEQ 
%token AND 
%token OR 
%token GEQ
%token LE
%token GE
%token DIV
%token MINUS
%token IF 
%token THEN 
%token ELSE
%token LET 
%token IN
%token EQ
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TRUE
%token FALSE
%token FUN
%token MID
%token MATCH
%token WITH
%token CONS
%token NIL
%token COLON
%token ARROW
%token LIST
%token TINT
%token TBOOL
%token <int> INT
%token <string> VAR
%token EOF 

(* associative and priority *)
%nonassoc IN
%nonassoc ELSE
%nonassoc INT
%nonassoc LET
%nonassoc FUN
%nonassoc ARROW
%left EQ
%left PLUS, MINUS
%left DIV, TIMES
%left OR, AND


%start <Ast.expr> prog
%%


(* grammar *)

prog: 
e=expr; EOF {e};

expr:
| x = VAR { Var (x) }
| i = INT { Integer (i) }
| TRUE { Boolean (true) }
| FALSE { Boolean (false) }
| e1= expr ;  PLUS; e2=expr {Binop( Plus, e1, e2) } 
| e1= expr ;  TIMES; e2=expr {Binop( Times, e1, e2) }
| e1= expr ; LEQ; e2=expr {Binop(Leq, e1, e2)}
| e1= expr; AND; e2=expr {Binop(And, e1,e2) }
| e1= expr; OR; e2=expr {Binop(Or, e1,e2) }
| e1= expr ; DIV; e2=expr {Binop( Div, e1, e2) }
| e1= expr ; MINUS; e2=expr {Binop( Minus, e1, e2) }
| e1= expr ; LE; e2=expr {Binop(Le, e1, e2)}
| e1= expr ; EQ; e2=expr {Binop(Eq, e1, e2)}
| e1= expr ; GEQ; e2=expr {Binop(Geq, e1, e2)}
| e1= expr ; GE; e2=expr {Binop(Ge, e1, e2)}
| LPAREN; e1=expr; RPAREN {e1}
| LET; x= VAR; COLON; t1=typ;  EQ; e1=expr;  IN; e2=expr {Let (x, t1, e1,e2)}
| FUN; x= VAR; COLON; t1=typ; ARROW; e1=expr {Fun (x, t1, e1)}
| e1=expr; e2=expr {App (e1, e2)}
| IF; e1=expr; THEN; e2=expr; ELSE; e3=expr {If (e1,e2,e3)}
| NIL; COLON; t1=typ {Nil (t1)}
| e1=expr; CONS; e2=expr {Cons(e1,e2)}
| MATCH; e=expr; COLON; t1=typ; WITH; NIL; ARROW; e1=expr; MID; x1=VAR; CONS; x2=VAR; ARROW; e2=expr {Match(e,t1, e1,x1,x2,e2)}


typ:
| TINT { Tint }
| TBOOL {Tbool }
| t1=typ; ARROW; t2=typ {Tfun(t1,t2)}
| t1=typ; LIST {Tlist(t1)}
| LPAREN; t1=typ; RPAREN {t1}


