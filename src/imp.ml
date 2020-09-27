type unop = Minus | Not
type binop =
  | Add | Sub | Mul | Div | Rem | Lsl | Lsr
  | Eq  | Neq | Lt  | Le  | Gt  | Ge
  | And | Or
      
type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of unop * expression
  | Binop of binop * expression * expression
  | Call  of string * expression list
      
type instruction =
  | Proc    of string * expression list
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | For     of instruction * expression * instruction * sequence
  | Break | Continue
  | Return  of expression
and sequence = instruction list

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
