%{

  open Lexing
  open Imp
  

%}

%token PLUS MINUS STAR SLASH PRCT
%token LSL LSR EQ NEQ LT LE GT GE
%token AND OR NOT

%token <int> CST
%token <bool> BOOL
%token <string> IDENT
%token VAR FUNCTION COMMA
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token SET IF ELSE WHILE FOR BREAK CONTINUE RETURN
%token EOF
%token SBRK AMPERSAND LBRACKET RBRACKET


%left AND OR
%left LT LE GT GE EQ NEQ
%left PLUS MINUS
%left STAR SLASH PRCT
%left LSL LSR
%nonassoc NOT
%nonassoc LBRACKET

%start program
%type <Imp.program> program

%%

program:
| globals=list(variable_decl)
    functions=list(function_def)
    EOF
    { {main=[Expr(Call("main", [Var("arg")]))]; functions; globals} }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum pos.pos_cnum
          in
          failwith message }
;

variable_decl:
| VAR id=IDENT SEMI { id }
;

fname:
| FUNCTION name=IDENT { name}
| FUNCTION MAIN { "main" }

function_def:
| MAIN BEGIN locals=list(variable_decl) code=list(instruction) END
  { let params = ["arg"] in let name = "main" in {name; code; params; locals} }
| name=fname LPAR params=separated_list(COMMA, IDENT) RPAR
    BEGIN locals=list(variable_decl) code=list(instruction) END
    { {name; code; params; locals} }
;

set_expr:
| id=IDENT SET e=expression { Set(id, e) }
;

instruction:
| s=set_expr SEMI { s }
| IF LPAR c=expression RPAR
    BEGIN s1=list(instruction) END
    ELSE BEGIN s2=list(instruction) END { If(c, s1, s2) }
| WHILE LPAR c=expression RPAR
    BEGIN s=list(instruction) END { While(c, s) }
| FOR LPAR init=set_expr COMMA cond=expression COMMA iter_set=set_expr RPAR
    BEGIN s=list(instruction) END { For(init, cond, iter_set, s) }
| BREAK SEMI  { Break }
| CONTINUE SEMI { Continue }
| RETURN e=expression SEMI { Return(e) }
| e=expression SEMI { Expr(e) }
| STAR e1=expression SET e2=expression SEMI { Write(e1, e2) }
| e1=expression LBRACKET e2=expression RBRACKET SET e3=expression SEMI
    { Write(array_access e1 e2, e3) }
;


expression:
| n=CST { Cst(n) }
| b=BOOL { Bool(b) }
| id=IDENT { Var(id) }
| LPAR e=expression RPAR { e }
| op=unop e=expression { Unop(op, e) }
| e1=expression op=binop e2=expression { 
  match e1, e2 with
    | Cst i1, Cst i2 -> let r = match op with
                        | Add -> Cst(i1 + i2)
                        | Sub -> Cst(i1 - i2)
                        | Mul -> Cst(i1 * i2)
                        | Div -> Cst(i1 / i2)
                        | Rem -> Cst(i1 mod i2)
                        | Eq  -> Bool(i1 == i2)
                        | Neq -> Bool(i1 != i2)
                        | Lt  -> Bool(i1 < i2)
                        | Le  -> Bool(i1 <= i2)
                        | Gt  -> Bool(i1 > i2)
                        | Ge  -> Bool(i1 >= i2)
                        | _ -> failwith("cst arithmetic/relational op not implemented")
                        in r
    | Bool b1, Bool b2 -> let r = match op with
                        | And -> Bool(b1 && b2)
                        | Or -> Bool(b1 || b2)
                        | _ -> failwith("cst logical op not implemented") in r
    | _ -> Binop(op, e1, e2)
    }
| f=IDENT LPAR params=separated_list(COMMA, expression) RPAR { Call(f, params) }
| STAR e=expression { Deref(e) }
| AMPERSAND id=IDENT { Addr(id) }
| SBRK LPAR e=expression RPAR { Sbrk(e) }
| LPAR e=expression RPAR LPAR params=separated_list(COMMA, expression) RPAR
    { match e with
      | Deref(e') -> PCall(e', params)
      | _ -> failwith "syntax error : call with function pointer should use *" }
| e1=expression LBRACKET e2=expression RBRACKET
    { Deref(array_access e1 e2) }
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
| PRCT { Rem }
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

