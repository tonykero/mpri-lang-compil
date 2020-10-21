open Mml

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
        expr