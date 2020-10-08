open Imp
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
  
let rec pp_expression: expression -> string = function
  | Cst(n) -> string_of_int n
  | Bool(b) -> if b then "true" else "false"
  | Var(x) -> x
  | Unop(op, e) ->
    sprintf "(%s%s)" (pp_unop op) (pp_expression e)
  | Binop(op, e1, e2) ->
    sprintf "(%s%s%s)" (pp_expression e1) (pp_binop op) (pp_expression e2)
  | Call(f, args) ->
    sprintf "%s(%s)" f (pp_args args)
  | Deref(Binop(Add, Var t, Binop(Mul, e, Cst 4))) ->
    sprintf "%s[%s]" t (pp_expression e)
  | Deref(e) ->
    sprintf "*%s" (pp_expression e)
  | Addr(x) ->
    "&" ^ x
  | PCall(f, args) ->
    sprintf "(%s)(%s)" (pp_expression f) (pp_args args)
  | Sbrk(e) ->
    sprintf "sbrk(%s)" (pp_expression e)
      
and pp_args: expression list -> string = function
  | [] -> ""
  | [a] -> pp_expression a
  | a::args -> sprintf "%s,%s" (pp_expression a) (pp_args args)

let pp_program prog out_channel =
  let print s = fprintf out_channel s in
  let margin = ref 0 in
  let print_margin () = for i = 1 to 2 * !margin do print " " done in
  
  let rec pp_instruction = function
    | Putchar(e) ->
      print "putchar(%s);" (pp_expression e)
    | Set(x, e) ->
      print "%s = %s;" x (pp_expression e)
    | If(c, s1, s2) ->
      print "if (%s) {\n" (pp_expression c);
      incr margin; pp_seq s1; decr margin;
      print_margin(); print "} else {\n";
      incr margin; pp_seq s2; decr margin;
      print_margin(); print "}"
    | While(c, s) ->
      print "while (%s) {\n" (pp_expression c);
      incr margin; pp_seq s; decr margin;
      print_margin(); print "}"
    | Return(e) ->
      print "return(%s);" (pp_expression e)
    | Write(Binop(Add, Var t, Binop(Mul, k, Cst 4)), e) ->
      print "%s[%s] = %s;" t (pp_expression k) (pp_expression e)
    | Write(d, e) ->
      print "*%s = %s;" (pp_expression d) (pp_expression e)
    | Expr(e) ->
      print "%s;" (pp_expression e)

  and pp_seq = function
    | [] -> ()
    | i::seq -> print_margin(); pp_instruction i; print "\n"; pp_seq seq
  in

  let rec pp_params = function
    | [] -> ""
    | [x] -> x
    | x::params -> sprintf "%s,%s" x (pp_params params)
  in

  let pp_var x = print_margin(); print "var %s;\n" x in
  
  let rec pp_vars = function
    | [] -> ()
    | [x] -> pp_var x; print "\n"
    | x::vars -> pp_var x; pp_vars vars
  in
      
  let pp_function fdef =
    print "function %s(%s) {\n" fdef.name (pp_params fdef.params);
    incr margin;
    pp_vars fdef.locals;
    pp_seq fdef.code;
    decr margin;
    print "}\n\n"
  in

  pp_vars prog.globals;
  List.iter pp_function prog.functions;
  print "main {\n";
  incr margin;
  pp_seq prog.main;
  decr margin;
  print "}\n"
