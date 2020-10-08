type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Tpl   of expression list
  | TplGet of expression * int
  | Fun   of string * expression
  | App   of expression * expression
  | If    of expression * expression * expression
  | LetIn of string * expression * expression
  | LetRec of string * expression * expression
      
type program = expression

let rec mk_fun xs e = match xs with
  | [] -> e
  | x::xs -> Fun(x, mk_fun xs e)
  
