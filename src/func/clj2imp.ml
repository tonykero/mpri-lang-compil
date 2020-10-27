open Clj
open Ops

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
let to_int      = set_instr "res" (Binop(Ops.Lsr, Binop(Ops.Sub, Imp.Var("res"), Imp.Cst(1)), Imp.Cst(1)))
let to_caml_int = set_instr "res" (Binop(Ops.Add, Binop(Ops.Lsl, Imp.Var("res"), Imp.Cst(1)), Imp.Cst(1)))

let match_int op ret = match op with
                        | Unop(o,_) -> if o == Minus then [ret] else []
                        | Binop(o,_,_) -> let r = match o with
                                            | Add | Sub | Mul | Div | Rem | Lsl | Lsr | Land | Lor
                                                -> [ret]
                                            | _ -> []
                                            in r
                        | _ -> []

let rec to_instr expr = match expr with
        | Cst(i)            -> set_instr "res" (Imp.Cst (2*i+1))
        | Bool(b)           -> set_instr "res" (Imp.Bool b)
        | Var(v)            -> let r = match v with
                                | Name str -> set_instr "res" (Imp.Var(str))
                                | _ -> failwith "cvar" in r
        | _ -> failwith "to_instr: CLJ2IMP: expression not implemented"
and to_seq expr = match expr with
        | Unop(op, e)      ->   to_seq e
                        @       match_int expr (to_int)
                        @       [(set_instr "res" (Imp.Unop(op, (Imp.Var("res")))))]
                        @       match_int expr (to_caml_int)
        | Binop(op, e1, e2) ->  let var = new_var () in
                                to_seq e1
                        @       match_int expr (to_int)
                        @       [(set_instr var (Imp.Var "res"))]
                        @       to_seq e2
                        @       match_int expr (to_int)
                        @       [(set_instr "res" (Imp.Binop(op, (Imp.Var(var)), Imp.Var("res"))))]
                        @       match_int expr (to_caml_int)
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
    let eq_1    = Imp.Binop(Ops.Eq, Imp.Var("x"), Imp.Cst(1)) in
    let eq_0    = Imp.Binop(Ops.Eq, Imp.Var("x"), Imp.Cst(0)) in
    let eq_01   = Imp.Binop(Ops.Or, eq_0, eq_1) in
    let is_int  = Imp.Binop(Ops.Land, Imp.Var("x"),Imp.Cst(1)) in
    
    let explode s =
        let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
        exp (String.length s - 1) [] in
    let print_str str = List.fold_right 
                            (fun (c:char) (seq:Imp.instruction list) ->
                                let ci:int = Char.code c in
                                [Imp.Expr(Imp.Call("putchar", [Imp.Cst(ci)]))] @ seq
                                )
                                (explode str) [] in
    let print_bool_def:Imp.function_def = {
        name        = "print_bool";
        code        = [Imp.If(Imp.Var("x"),print_str "true", print_str "false");
                        Imp.Return(Imp.Cst(0))];
        params      = ["x"];
        locals      = []
    } in
    let print_def:Imp.function_def = {
        name        = "print";
        code        =   [Imp.If(eq_01, 
                                (print_str "bool: ") @ [Imp.Expr(Imp.Call("print_bool", [Imp.Var("x")]))],
                                [Imp.If(is_int, 
                                    (print_str "int: ") @ [Imp.Expr(Imp.Call("print_int", [Binop(Lsr, Binop(Sub,Imp.Var("x"), Imp.Cst(1)), Imp.Cst(1))]))],
                                    (print_str "ptr: ") @ [Imp.Expr(Imp.Call("print_int", [Imp.Var("x")]))]
                                )]
                        );
                        Imp.Return(Imp.Cst(0))
                        ];
        params      = ["x"];
        locals      = []
        } in
    let code_instr  = to_seq prog.code in
    let main        = code_instr @ [Imp.Expr(Imp.Call("print", [Var("res")]))] in
    let functions   = (List.fold_left (fun defs fundef -> defs @ [tr_fun_def fundef] )
                        [malloc_def;print_def;print_bool_def] prog.functions
                        )
                        in
    let globals     = !globals in
    let (r:Imp.program) = {main; functions; globals} in r