open Fun

let functions = ref []
let cur_fname = ref ""
let new_fun =
  let cpt = ref (-1) in
  fun () -> incr cpt; let name = Printf.sprintf "fun_%i" !cpt in
            name

let add_fun name param code free_vars =
            let (fundef:Clj.function_def) = {name;code;param;free_vars} in
            functions := !functions @ [fundef]

let strneq str = fun el -> not (el = str)

let rec get_free_vars expr = match expr with
        | Cst(_) -> []
        | Bool(_) -> []
        | Var(str) -> [str]
        | Unop(_,e) -> get_free_vars e
        | Binop(_,e1,e2) -> get_free_vars e1 @ get_free_vars e2
        | Tpl(se) -> (List.fold_right (fun expr vars -> get_free_vars expr @ vars ) se [])
        | TplGet(e,_) -> get_free_vars e
        | Fun(str, e) -> let free_vars = get_free_vars e in
                         List.filter (strneq str) free_vars
        | App(e1,e2)  -> get_free_vars e1 @ get_free_vars e2
        | If(c,e1,e2) -> get_free_vars c @ get_free_vars e1 @ get_free_vars e2
        | LetIn(str,e1,e2) ->   let free_vars = get_free_vars e1 @ get_free_vars e2 in
                                List.filter (strneq str) free_vars
        | LetRec(str,e1,e2) ->   let free_vars = get_free_vars e1 @ get_free_vars e2 in
                                List.filter (strneq str) free_vars

let rec tr_expr expr = match expr with
        | Cst(i)            -> Clj.Cst(i)
        | Bool(b)           -> Clj.Bool(b)
        | Var(str)          -> Clj.Var(Clj.Name(str))
        | Unop(op,e)        -> Clj.Unop(op,tr_expr e)
        | Binop(op,e1,e2)   -> Clj.Binop(op,tr_expr e1, tr_expr e2)
        | Tpl(se)           -> Clj.Tpl(List.map tr_expr se)
        | TplGet(e,i)       -> Clj.TplGet(tr_expr e, i)
        | Fun(str, e)       ->  let fname = new_fun () in
                                let old_cur_fname = !cur_fname in
                                let free_vars = List.filter (strneq str) (get_free_vars e) in
                                cur_fname := fname;
                                add_fun fname str (tr_expr e) free_vars;
                                cur_fname := old_cur_fname;
                                Clj.Clos(fname,free_vars)
        | App(e1,e2)        -> Clj.App(tr_expr e1, tr_expr e2)
        | If(c,e1,e2)       -> Clj.If(tr_expr c,tr_expr e1,tr_expr e2)
        | LetIn(str,e1,e2)  -> Clj.LetIn(str,tr_expr e1,tr_expr e2)
        | LetRec(str,e1,e2) -> Clj.LetRec(str,tr_expr e1,tr_expr e2)

let translate_program prog  = 
        let code        = tr_expr prog in
        let functions   = !functions in
        let (program:Clj.program) = { functions; code } in
        program