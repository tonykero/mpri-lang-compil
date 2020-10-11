open Clj


let is_in el l = List.exists (fun e -> el = e) l

let globals     = ref ["res"]
let cur_locals  = ref[]
let cur_fname   = ref ""
let add_var var = if !cur_fname != ""
                    then (* in function *)
                        if is_in var !cur_locals then ()
                        else cur_locals := var::!cur_locals
                    else (* global *)
                        if is_in var !globals then ()
                        else globals := var::!globals

let new_var =
  let cpt = ref (-1) in
  fun () -> incr cpt; let var_name = Printf.sprintf "__var_%i" !cpt in
        add_var var_name;var_name

let rec set_instr str e = Imp.Set(str, e)

let rec to_instr expr = match expr with
        | Cst(i)            -> set_instr "res" (Imp.Cst i)
        | Bool(b)           -> set_instr "res" (Imp.Bool b)
        | Var(v)            -> let r = match v with
                                | Name str -> set_instr "res" (Imp.Var(str))
                                | _ -> failwith "cvar" in r
        | _ -> failwith "to_instr: CLJ2IMP: expression not implemented"
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
        | FunRef(str)       ->  [set_instr "res" (Imp.Addr(str))]
        | Clos(str, fvars)  ->  let arr_size = ((List.length fvars) + 1) in
                                let vars = List.map (fun str -> Imp.Var(str)) fvars in
                                [set_instr "res" (Imp.array_create (Cst arr_size))]
                            @   (List.fold_right2 (fun expr idx seq ->
                                                        let arr_set = Imp.array_set (Imp.Var "res") (Cst idx) expr in
                                                        [ arr_set ] @ seq
                                                    )
                                            ([Imp.Addr(str)] @ vars ) (List.init arr_size (fun id -> id)) [])
        | App(e1,e2)        ->  let ptr = new_var () in
                                let clos_var = new_var () in
                                to_seq e1
                            @   [set_instr ptr (Imp.array_get (Imp.Var "res") (Cst 0))]
                            @   [set_instr clos_var (Imp.Var("res"))]
                            @   to_seq e2
                            @   [set_instr "res" (Imp.PCall(Imp.Deref(Imp.Var(ptr)),[Imp.Var("res");Imp.Var(clos_var)]))]
        | LetIn(str,e1,e2)  ->  add_var str;to_seq e1
                        @       [(set_instr str (Imp.Var("res")))]
                        @       to_seq e2
        | LetRec(str,e1,e2)  ->  add_var str;to_seq e1
                        @       [(set_instr str (Imp.Var("res")))]
                        @       to_seq e2
        
        | If(c,e1,e2)       ->   to_seq c
                        @       [Imp.If(Imp.Var("res"), (to_seq e1), (to_seq e2))]
        | _ -> [to_instr expr]

let tr_fun_def fundef =
            let name = fundef.name in
            let old_cur_fname = !cur_fname in
            let old_cur_locals = !cur_locals in
            cur_fname  := name;
            cur_locals := [fundef.param]; (* add param so we know its position*)
            let code    = let fvars = fundef.free_vars in
                        (List.fold_right2 
                            (fun idx str code -> 
                                add_var str;
                                [set_instr str (Imp.array_get (Imp.Var "clos") (Cst idx))] @ code 
                            )
                            (List.init (List.length fvars) (fun idx -> idx+1))
                            fvars
                            []
                        )
                        @ (to_seq fundef.code)
                        @ [Imp.Return(Imp.Var("res"))]  in
            let params  = [fundef.param; "clos"] in 
            let locals  = List.tl (List.rev !cur_locals) in (* remove param from locals *)
            cur_fname := old_cur_fname;
            cur_locals := old_cur_locals;
            let (function_def:Imp.function_def) = {name;code;params;locals}
            in function_def

let translate_program prog =
    let malloc_def:Imp.function_def = {
        name        = "malloc";
        code        = [Imp.Return(Imp.Sbrk(Imp.Var("size")))];
        params      = ["size"];
        locals      = [] 
        } in
    let code_instr  = to_seq prog.code in
    let main        = code_instr @ [Imp.Expr(Imp.Call("print_int", [Var("res")]))] in
    let functions   = (List.fold_left (fun defs fundef -> defs @ [tr_fun_def fundef] )
                        [malloc_def] prog.functions
                        )
                        in
    let globals     = !globals in
    let (r:Imp.program) = {main; functions; globals} in r