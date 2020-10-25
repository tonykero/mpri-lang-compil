open Fun
open Ops
open Printf

let pp_unop: unop -> string = function
  | Minus -> "-"
  | Not   -> "!"

let pp_binop: binop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Rem -> "%"
  | Lsl -> "<<"
  | Lsr -> ">>"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Le  -> "<="
  | Gt  -> ">"
  | Ge  -> ">="
  | And -> "&&"
  | Or  -> "||"
  | Land-> "&"
  | Lor -> "|"
  
let rec (pp_expression: Fun.expression -> string) = function
  | Cst(n) -> string_of_int n
  | Bool(b) -> if b then "true" else "false"
  | Var(x) -> x
  | Unop(op, e) ->
    sprintf "(%s%s)" (pp_unop op) (pp_expression e)
  | Binop(op, e1, e2) ->
    sprintf "(%s%s%s)" (pp_expression e1) (pp_binop op) (pp_expression e2)
  | Tpl(el) ->
    sprintf "(%s)" (pp_args el)
  | TplGet(e,i) ->
    sprintf "(%s[%d])" (pp_expression e) (i)
  | Fun(str, e) ->
    sprintf "(fun %s -> %s)" str (pp_expression e)
  | App(e1,e2) ->
    sprintf "(%s %s)" (pp_expression e1) (pp_expression e2)
  | If(c,e1,e2) -> 
    sprintf "(if (%s) then (%s) else (%s))" (pp_expression c) (pp_expression e1) (pp_expression e2)
  | LetIn(s,e1,e2) ->
    sprintf "let %s = %s in\n %s" s (pp_expression e1) (pp_expression e2)
  | LetRec(s,e1,e2) ->
    sprintf "let rec %s = %s\n in %s" s (pp_expression e1) (pp_expression e2)

and pp_args: expression list -> string = function
  | [] -> ""
  | [a] -> pp_expression a
  | a::args -> sprintf "%s,%s" (pp_expression a) (pp_args args)

let pp_program (prog:Fun.expression) out_channel =
  let print s = fprintf out_channel s in
  print "%s" (pp_expression prog)