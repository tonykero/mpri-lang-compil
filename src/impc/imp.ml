open Ops

type expression =
  | Cst     of int
  | Bool    of bool
  | Var     of string
  | Unop    of unop * expression
  | Binop   of binop * expression * expression
  | Call    of string * expression list
  | Deref   of expression                   (* *ptr         *)
  | Addr    of string                       (* &ptr         *)
  | PCall   of expression * expression list (* ( *ptr)(se)  *)
  | Sbrk    of expression                   (* sbrk(e)      *)
  | Array   of expression list              (* { e1,.., en} *)
  | Repeat  of expression * expression      (* [e1] * e2    *)
  | Comprehension of expression * string * expression (* [ expr for str in range(Cst) ] *)

type instruction =
  (*| Proc    of string * expression list*)
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | For     of instruction * expression * instruction * sequence
  | Break | Continue
  | Return  of expression
  | Write   of expression * expression  (* *ptr = e *)
  | Expr    of expression
and sequence = instruction list

let array_access (t: expression) (i: expression): expression =
  Binop(Add, t, Binop(Mul, i, Cst(4)))
  (*failwith "array access not implemented (imp.ml)"*)

type function_def = {
  name: string;
  code: sequence;
  params: string list;
  locals: string list;
}

type program = {
  main: sequence;
  functions: function_def list;
  globals: string list;
}
