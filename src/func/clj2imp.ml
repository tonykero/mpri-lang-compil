open Clj



let new_var =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__var_%i" !cpt

let is_in el l = List.exists (fun e -> el = e) l

let globals = ref ["res"]
let add_var var = if is_in var !globals then ()
                else globals := var::!globals; ()

let rec to_expr expr = match expr with
        | Cst(i)            -> Imp.Cst(i)
        | Bool(b)           -> Imp.Bool(b)
        | Var(v)            -> let r = match v with
                                | Name str -> add_var str;Imp.Var(str)
                                | _ -> failwith "cvar" in r 
        | Unop(op, e)       -> Imp.Unop(op,to_expr e)
        | Binop(op, e1, e2) -> Imp.Binop(op, to_expr e1, to_expr e2)
        | Tpl(se)           -> Imp.Array(List.map to_expr se)
        | TplGet(e,i)       -> Imp.Deref(Imp.array_access (to_expr e) (Imp.Cst(i)))
        | _ -> failwith "CLJ2IMP: expression not implemented"
        
let rec to_instr expr = match expr with
        | LetIn(str,e1,e2) -> [Imp.Set(str, to_expr e1);] @ to_instr e2
        | _ -> [Imp.Set("res", to_expr expr)]

let translate_program prog =
    let code_instr  = to_instr prog in
    let main        = code_instr @ [Imp.Expr(Imp.Call("print_int", [Var("res")]))] in
    let functions   = [] in
    let globals     = !globals in
    let (r:Imp.program) = {main; functions; globals} in r