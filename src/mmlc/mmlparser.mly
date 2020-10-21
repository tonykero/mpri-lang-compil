%{

  open Lexing
  open Mml
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
%token LPAR RPAR LBRACE RBRACE
%token COLON SEMI DOT
%token TINT TBOOL TYPE
%token FST SND
%token COMMA
%token EOF

%nonassoc IN ARROW
%nonassoc ELSE
%left AND OR
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH MOD
%left LSL LSR
%nonassoc FST SND
%nonassoc LPAR IDENT CST BOOL

%start program
%type <Mml.program> program

%%

program:
| types=list(type_def) code=expression EOF { {types; code} }
;

ty:
| TINT { TInt }
| TBOOL { TBool }
| alpha=ty ARROW beta=ty { TFun(alpha, beta) }
| alpha=ty STAR beta=ty { TPair(alpha, beta) }
| id=IDENT { TNamed(id) }
| LPAR alpha=ty RPAR { alpha }
;

typed_ident:
| x=IDENT COLON alpha=ty { (x, alpha) }
| LPAR tid=typed_ident RPAR { tid }
;

type_def:
| TYPE id=IDENT EQ LBRACE fields=separated_list(SEMI, typed_ident) RBRACE
    { (id, fields) }
;

field_def:
| x=IDENT EQ e=expression { (x, e) }
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
| LPAR e1=expression COMMA e2=expression RPAR { Pair(e1, e2) }
| FST e=expression { Fst(e) }
| SND e=expression { Snd(e) }
| LBRACE fields=separated_list(SEMI, field_def) RBRACE { Struct(fields) }
| e=simple_expression DOT x=IDENT { StrGet(e, x) }
| e1=expression e2=simple_expression { App(e1, e2) }
| FUN LPAR tid=typed_ident RPAR ARROW e=expression { let x,t = tid in Fun(x, t, e) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1, e2, e3) }
(* Let/Letrec intègre sucre pour les fonctions *)
| LET f=IDENT txs=list(typed_ident) EQ e1=expression IN e2=expression
    { LetIn(f, mk_fun txs e1, e2) }
| LET REC f=IDENT txs=list(typed_ident) COLON alpha=ty EQ e1=expression IN e2=expression
    { LetRec(f, mk_fun_type txs alpha, mk_fun txs e1, e2) }
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

