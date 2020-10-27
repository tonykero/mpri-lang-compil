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
let to_int      = set_instr "res" (Binop(Ops.Div, Binop(Ops.Sub, Imp.Var("res"), Imp.Cst(1)), Imp.Cst(2)))
let to_caml_int = set_instr "res" (Binop(Ops.Add, Binop(Ops.Mul, Imp.Var("res"), Imp.Cst(2)), Imp.Cst(1)))

let match_int op ret = match op with
                        | Unop(o,_) -> if o == Minus then [ret] else []
                        | Binop(o,_,_) -> let r = match o with
                                            | Add | Sub | Mul | Div | Rem | Lsl | Lsr | Land | Lor
                                                -> [ret]
                                            | _ -> []
                                            in r
                        | _ -> []

let bool_to_int b = if b then 1 else 0

let rec to_instr expr = match expr with
        | Cst(i)            -> set_instr "res" (Imp.Cst (2*i+1))
        | Bool(b)           -> set_instr "res" (Imp.Cst (2*(bool_to_int b)+1))
        | Var(v)            -> let r = match v with
                                | Name str -> set_instr "res" (Imp.Var(str))
                                | _ -> failwith "cvar" in r
        | _ -> failwith "to_instr: CLJ2IMP: expression not implemented"
and to_seq expr = match expr with
        | Unop(op, e)      ->   to_seq e
                        @       [to_int]
                        @       [(set_instr "res" (Imp.Unop(op, (Imp.Var("res")))))]
                        @       [to_caml_int]
        | Binop(op, e1, e2) ->  let var = new_var () in
                                to_seq e1
                        @       [to_int]
                        @       [(set_instr var (Imp.Var "res"))]
                        @       to_seq e2
                        @       [to_int]
                        @       [(set_instr "res" (Imp.Binop(op, (Imp.Var(var)), Imp.Var("res"))))]
                        @       [to_caml_int]
        | Tpl(se)           ->  let arr_size = List.length se in
                                let arr_var  = new_var () in
                                let arr_malloc = (Imp.array_create (Cst (arr_size+1))) in
                                [(set_instr (arr_var) (arr_malloc))]
                                (*store size in header*)
                        @       [Imp.array_set (Imp.Var arr_var) (Cst 0) (Binop(Lsl, (Cst arr_size), Cst 1)) ]
                        @       [set_instr (arr_var) (Binop(Add, Imp.Var(arr_var), Cst 4))]
                        
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
                                [set_instr "res" (Imp.array_create (Cst (arr_size+1)))]
                            (*store size in header*)
                            @   [Imp.array_set (Imp.Var "res") (Cst 0) (Binop(Add, Binop(Mul, (Cst arr_size), Cst 2), Cst 1)) ]
                            @   [set_instr ("res") (Binop(Add, Imp.Var("res"), Cst 4))]
                            
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
                        @       [to_int]
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
    let is_int  = Imp.Binop(Or,Imp.Binop(Ops.Eq, Imp.Var("x"),Imp.Cst(0)),Imp.Binop(Ops.Land, Imp.Var("x"),Imp.Cst(1))) in
    (* https://stackoverflow.com/a/10069969 *)
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
    let i_init i= Imp.Set("i", i) in
    let iltsize = Imp.Binop(Lt, Imp.Var("i"), Imp.Var("size")) in
    let iinc = Imp.Set("i", Binop(Add, Imp.Var("i"), Cst 1)) in
    let first_elem = Imp.array_get (Imp.Var("x")) (Cst 0) in
    let print_ptr_def:Imp.function_def = {
        name        = "print_ptr";
        code        = [Imp.Set("size", Binop(Lsr, Imp.Deref(Binop(Sub, Imp.Var("x"), Cst 4)), Cst 1));
                        Imp.Set("is_clos", Binop(Land, Imp.Deref(Binop(Sub, Imp.Var("x"), Cst 4)), Cst 1))]
                    @ (print_str "(")
                    @ [i_init (Imp.Var("is_clos"))]
                    @ [Imp.If(Imp.Var("is_clos"),
                        (print_str "closure: ")
                    @   [Imp.Expr(Imp.Call("print_int", [first_elem ]))]
                    @   (print_str ", ")
                    , [])]
                    @ [Imp.While(iltsize, [
                        Imp.Expr(Imp.Call("print", [Imp.array_get (Imp.Var("x")) (Imp.Var("i")) ]))]
                    @   [Imp.If(Binop(Neq, Imp.Var("i"), Binop(Sub, Imp.Var("size"), Cst 1)), print_str ", ", [])]
                    
                    @   [iinc]
                    )]
                    @  print_str ")"
                    @  [Imp.Return(Imp.Cst(0))]
        ;
        params = ["x"];
        locals = ["i";"size";"is_clos"];
    } in
    (* if x negative*)
    (* x = -((-x + (2^30)) % (2^30-1) + 1) *)
    let max_ = Imp.Binop(Lsl, Cst 1, Cst 29) in 
    let (neg_x:Imp.expression) = Unop(Minus, Binop(Add, Cst 1, Binop(Rem,Binop(Add, Unop(Minus,Imp.Var("x")), max_),Binop(Sub, max_, Cst 1)))) in
    let print_def:Imp.function_def = {
        name        = "print";
        code        =   [Imp.If(is_int, 
                                    (print_str "int: ")
                                    @ [Imp.Set("x", Binop(Div, Binop(Sub,Imp.Var("x"), Imp.Cst(1)), Imp.Cst(2)))]
                                    @ [Imp.If(Binop(Gt, Imp.Var("x"), Binop(Sub, max_, Cst 1)), [Imp.Set("x", neg_x)], [])]
                                    @ [Imp.Expr(Imp.Call("print_int", [Imp.Var("x")]))],
                                    
                                    [Imp.Expr(Imp.Call("print_ptr", [Imp.Var("x")]))]
                                );
                        Imp.Return(Imp.Cst(0))
                        ];
        params      = ["x"];
        locals      = []
        } in
    let code_instr  = to_seq prog.code in
    let main        = code_instr @ [Imp.Expr(Imp.Call("print", [Var("res")]))] in
    let functions   = (List.fold_left (fun defs fundef -> defs @ [tr_fun_def fundef] )
                        [malloc_def;
                        print_def;
                        print_bool_def;
                        print_ptr_def] prog.functions
                        )
                        in
    let globals     = !globals in
    let (r:Imp.program) = {main; functions; globals} in r