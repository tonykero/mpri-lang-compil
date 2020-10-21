type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TPair of ty * ty
  | TNamed of string

type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Pair  of expression * expression
  | Fst   of expression
  | Snd   of expression
  | Struct of (string * expression) list
  | StrGet of expression * string
  | Fun   of string * ty * expression
  | App   of expression * expression
  | If    of expression * expression * expression
  | LetIn of string * expression * expression
  | LetRec of string * ty * expression * expression

type namedtuple = (string * ty) list
      
type program = {
  types: (string * namedtuple) list;
  code: expression;
}

let rec mk_fun xs e = match xs with
  | [] -> e
  | (x, t)::xs -> Fun(x, t, mk_fun xs e)
  
let rec mk_fun_type xs t = match xs with
  | [] -> t
  | (_, t)::xs -> TFun(t, mk_fun_type xs t)
