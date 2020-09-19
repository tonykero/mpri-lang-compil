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
      
type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
and sequence = instruction list

type program = {
  main: sequence;
  globals: string list;
}
