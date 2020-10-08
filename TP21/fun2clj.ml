open Fun

let rec tr_expr expr = match expr with
        | Cst(i)            -> Clj.Cst(i)
        | Bool(b)           -> Clj.Bool(b)
        | Var(str)          -> Clj.Var(Clj.Name(str))
        | Unop(op,e)        -> Clj.Unop(op,tr_expr e)
        | Binop(op,e1,e2)   -> Clj.Binop(op,tr_expr e1, tr_expr e2)
        | Tpl(se)           -> Clj.Tpl(List.map tr_expr se)
        | TplGet(e,i)       -> Clj.TplGet(tr_expr e, i)
        | App(e1,e2)        -> Clj.App(tr_expr e1, tr_expr e2)
        | If(c,e1,e2)       -> Clj.If(tr_expr c,tr_expr e1,tr_expr e2)
        | LetIn(str,e1,e2)  -> Clj.LetIn(str,tr_expr e1,tr_expr e2)
        | LetRec(str,e1,e2) -> Clj.LetRec(str,tr_expr e1,tr_expr e2)
        | _ -> failwith "expression not implemented"

let translate_program prog = tr_expr prog