type var =
  | Name of string
  | CVar of int

type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of var
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Tpl   of expression list
  | TplGet of expression * int
  | FunRef of string
  | Clos  of string * string list
  | App   of expression * expression
  | If    of expression * expression * expression
  | LetIn of string * expression * expression
  | LetRec of string * expression * expression
      
type function_def = {
  name: string;
  code: expression;
  param: string;
  free_vars: string list
}

type program = {
  functions: function_def list;
  code: expression;
}
