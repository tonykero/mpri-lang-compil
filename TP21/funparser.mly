%{

  open Lexing
  open Fun
  open Ops

%}

%token PLUS MINUS STAR SLASH MOD
%token LSL LSR EQ NEQ LT LE GT GE
%token AND OR NOT

%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token FUN ARROW
%token LET REC IN
%token IF THEN ELSE
%token LPAR RPAR LBRACKET RBRACKET
%token COMMA
%token EOF

%nonassoc IN ARROW
%nonassoc ELSE
%left AND OR
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left LSL LSR
%nonassoc LPAR IDENT CST BOOL

%start program
%type <Fun.program> program

%%

program:
| e=expression EOF { e }
;

simple_expression:
| n=CST { Cst(n) }
| b=BOOL { Bool(b) }
| id=IDENT { Var(id) }
| LPAR e=expression RPAR { e }
;
  
expression:
| e=simple_expression { e }
| op=unop e=simple_expression { Unop(op, e) }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| LPAR e=expression COMMA es=separated_nonempty_list(COMMA, expression) RPAR
    { Tpl(e :: es) }
| e=simple_expression LBRACKET k=CST RBRACKET { TplGet(e, k) }
| e1=expression e2=simple_expression { App(e1, e2) }
| FUN x=IDENT ARROW e=expression { Fun(x, e) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1, e2, e3) }
| LET f=IDENT xs=list(IDENT) EQ e1=expression IN e2=expression
    { LetIn(f, mk_fun xs e1, e2) }
| LET REC f=IDENT xs=list(IDENT) EQ e1=expression IN e2=expression
    { LetRec(f, mk_fun xs e1, e2) }
;

%inline unop:
| MINUS { Minus }
| NOT { Not }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| SLASH { Div }
| MOD { Rem }
| LSL { Lsl }
| LSR { Lsr }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }
;

