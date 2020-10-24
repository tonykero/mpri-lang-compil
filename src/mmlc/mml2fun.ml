open Mml

type sym_table_type = (string, ty) Hashtbl.t
let (sym_table: sym_table_type) = Hashtbl.create 64

let rec type_of expr = match expr with
        | Cst _         -> TInt
        | Bool _        -> TBool
        | Var s         -> Hashtbl.find sym_table s
        | Unop(op,e)    -> let rt = match op with
                            | Minus -> TInt
                            | Not   -> TBool
                            in if rt == (type_of e) then
                                    rt
                                else failwith "incompatible type in unary operation"
        | Binop(op,e1,e2)-> let rt = match op with
                            | Add | Sub | Mul | Div | Rem | Lsl | Lsr | Land | Lor
                                -> TInt
                            | Eq  | Neq | Lt  | Le  | Gt  | Ge | And | Or
                                -> TBool
                            in if (type_of e1) == (type_of e2) then
                                    if rt == (type_of e1) then
                                        rt
                                    else failwith "incompatible type in binary operation"
                                else failwith "type mismatch in binary operation"
        | Pair(e1,e2)   -> TPair(type_of e1, type_of e2)
        | Fst(e)        -> let r = match e with
                            | Pair(e1,_) -> type_of e1
                            | _ -> failwith "fst on non-pair type"
                            in r
        | Snd(e)        -> let r = match e with
                            | Pair(_,e2) -> type_of e2
                            | _ -> failwith "snd on non-pair type"
                            in r
        (*Struct & StrGet*)
        | Fun(s,t,e)    -> Hashtbl.add sym_table s t;TFun(t, (type_of e))
        | App(e1,e2)    -> let r = match (type_of e1) with 
                            | TFun(ta,tb) -> if ta == (type_of(e2)) then
                                                tb
                                            else failwith "incompatible type in function application"
                            | _ -> failwith "application on non-function type"
                            in r
        | If(c,e1,e2)   ->  if (type_of c) == TBool then
                                if (type_of e1) == (type_of e2) then
                                    type_of e1
                                else failwith "if branches gives different types"
                            else failwith "if condition is not a boolean"
        | LetIn(s,e1,e2) -> Hashtbl.add sym_table s (type_of e1);(type_of e2)
        | LetRec(s,t,e1,e2) -> Hashtbl.add sym_table s (TFun(t,type_of e1));(type_of e2)
        | _ -> failwith "TYPE_OF: expr not implemented"

let rec tr_expr expr = match expr with
        | Cst i             -> Fun.Cst(i)
        | Bool b            -> Fun.Bool(b)
        | Var str           -> Fun.Var(str)
        | Unop(o, e)        -> Fun.Unop(o, tr_expr e)
        | Binop(o,e1,e2)    -> Fun.Binop(o, tr_expr e1, tr_expr e2)
        | Pair(e1,e2)       -> Fun.Tpl([tr_expr e1; tr_expr e2])
        | Fst(e)            -> Fun.TplGet(tr_expr e, 0)
        | Snd(e)            -> Fun.TplGet(tr_expr e, 1)
        (* Struct & StrGet*)
        | Fun(s,t,e)        -> Fun.Fun(s,tr_expr e)
        | App(e1,e2)        -> Fun.App(tr_expr e1, tr_expr e2)
        | If(c,e1,e2)       -> Fun.If(tr_expr c, tr_expr e1, tr_expr e2)
        | LetIn(s,e1,e2)    -> Fun.LetIn(s, tr_expr e1, tr_expr e2)
        | LetRec(s,t,e1,e2) -> Fun.LetRec(s,tr_expr e1, tr_expr e2)
        | _ -> failwith "not implemented"

let translate_program (prog:Mml.program) = 
    let (expr:Fun.expression) = tr_expr prog.code in
        type_of prog.code;expr