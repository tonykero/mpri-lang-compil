open Clj


let is_in el l = List.exists (fun e -> el = e) l

let globals = ref ["res"]
let add_var var = if is_in var !globals then ()
                else globals := var::!globals; ()

let new_var =
  let cpt = ref (-1) in
  fun () -> incr cpt; let var_name = Printf.sprintf "__var_%i" !cpt in
        add_var var_name;var_name

let rec set_instr str e = Imp.Set(str, e)

let rec to_instr expr = match expr with
        | Cst(i)            -> set_instr "res" (Imp.Cst i)
        | Bool(b)           -> set_instr "res" (Imp.Bool b)
        | Var(v)            -> let r = match v with
                                | Name str -> add_var str;set_instr "res" (Imp.Var(str))
                                | _ -> failwith "cvar" in r
        | _ -> failwith ""
and to_seq expr = match expr with
        | Unop(op, e)      ->   to_seq e
                        @       [(set_instr "res" (Imp.Unop(op, (Imp.Var("res")))))]
        | Binop(op, e1, e2) ->  let var = new_var () in
                                to_seq e1
                        @       [(set_instr var (Imp.Var "res"))]
                        @       to_seq e2
                        @       [(set_instr "res" (Imp.Binop(op, (Imp.Var(var)), Imp.Var("res"))))]
        
        | Tpl(se)           ->  let arr_size = List.length se in
                                let arr_var  = new_var () in
                                let arr_malloc = (Imp.array_create (Cst arr_size)) in
                                [(set_instr (arr_var) (arr_malloc))]
                        @       (List.fold_right2 (fun expr idx seq ->
                                                        to_seq expr
                                                @       [Imp.array_set (Imp.Var arr_var) (Cst idx) (Imp.Var "res") ]
                                                @       seq)
                                        se (List.init arr_size (fun id -> id)) [])
                        @       [(set_instr "res" (Imp.Var arr_var))]
        | TplGet(e,i)       ->  let ptr = (Imp.array_access (Imp.Var "res") (Imp.Cst(i))) in
                                to_seq e
                        @       [set_instr "res" (Imp.Deref ptr)]
        | LetIn(str,e1,e2) ->   to_seq e1
                        @       [(set_instr str (Imp.Var("res")))]
                        @       to_seq e2
        | If(c,e1,e2)      ->   to_seq c
                        @       [Imp.If(Imp.Var("res"), (to_seq e1), (to_seq e2))]
        | _ -> [to_instr expr]

let translate_program prog =
    let malloc_def:Imp.function_def = {
        name        = "malloc";
        code        = [Imp.Return(Imp.Sbrk(Imp.Var("size")))];
        params      = ["size"];
        locals      = [] 
        } in
    let code_instr  = to_seq prog in
    let main        = code_instr @ [Imp.Expr(Imp.Call("print_int", [Var("res")]))] in
    let functions   = [malloc_def] in
    let globals     = !globals in
    let (r:Imp.program) = {main; functions; globals} in r