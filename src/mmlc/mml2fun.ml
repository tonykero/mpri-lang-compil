open Mml


let (namedtuples:(string*namedtuple) list ref) = ref ([])

type sym_table_type = (string, ty) Hashtbl.t
let (sym_table: sym_table_type) = Hashtbl.create 64

let rec index_of_i el l i =
        if List.nth l i = el then i
          else index_of_i el l (i+1)
let index_of el l = index_of_i el l 0

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
        | Struct(l)     ->  let map_f:(string*expression) -> (string*ty) = (fun p -> (fst p,type_of (snd p))) in
                            let r = try 
                                let ntuple:namedtuple = List.map map_f l in
                                let ntuple_list = List.map (fun p -> snd p) !namedtuples in
                                let index = index_of ntuple ntuple_list in
                                let struct_name = fst (List.nth (!namedtuples) index) in
                                TNamed(struct_name)
                            with _ -> failwith "tuple type not found"
                            in r
        | StrGet(e,str) -> let r = match e with
                                | Var(s) -> let var_ty = Hashtbl.find sym_table s in
                                            let rt = match var_ty with
                                                | TNamed(struct_name) ->    let ntuple_name_list = List.map (fun p -> fst p) !namedtuples in
                                                                            let r = try
                                                                                let index = index_of struct_name ntuple_name_list in
                                                                                let ntuple = snd (List.nth !namedtuples index) in
                                                                                let var_name_list = List.map (fun p -> fst p) ntuple in
                                                                                let index2 = index_of str var_name_list in
                                                                                snd (List.nth ntuple index2)
                                                                            with _ -> failwith (Printf.sprintf "incompatible type of var %s in tuple %s" str struct_name)
                                                                            in r
                                                | _ -> failwith (Printf.sprintf "var %s is not a named tuple" s) 
                                            in rt
                                | _ -> failwith (Printf.sprintf "accessing %s of non-struct" str)
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
        | Struct(l)         -> let expr_list = List.map (fun pair -> tr_expr (snd pair)) l in
                                    Fun.Tpl(expr_list)
        | StrGet(e,str)     ->  let r = match e with 
                                | Var(s) -> let var_ty = Hashtbl.find sym_table s in
                                            let rt = match var_ty with
                                                | TNamed(struct_name) ->    let ntuple_name_list = List.map (fun p -> fst p) !namedtuples in
                                                                            let r = try
                                                                                let index = index_of struct_name ntuple_name_list in
                                                                                let ntuple = snd (List.nth !namedtuples index) in
                                                                                let var_name_list = List.map (fun p -> fst p) ntuple in
                                                                                let index2 = index_of str var_name_list in
                                                                                Fun.TplGet(tr_expr e, index2)
                                                                            with _ -> failwith (Printf.sprintf "%s not a member of tuple %s" str struct_name)
                                                                            in r
                                                | _ -> failwith (Printf.sprintf "var %s is not a named tuple" s) 
                                            in rt
                                | _ -> failwith (Printf.sprintf "accessing %s of non-struct" str)
                                in r 
        (* Struct & StrGet*)
        | Fun(s,t,e)        -> Fun.Fun(s,tr_expr e)
        | App(e1,e2)        -> Fun.App(tr_expr e1, tr_expr e2)
        | If(c,e1,e2)       -> Fun.If(tr_expr c, tr_expr e1, tr_expr e2)
        | LetIn(s,e1,e2)    -> Fun.LetIn(s, tr_expr e1, tr_expr e2)
        | LetRec(s,t,e1,e2) -> Fun.LetRec(s,tr_expr e1, tr_expr e2)
        | _ -> failwith "not implemented"

let translate_program (prog:Mml.program) = 
    namedtuples := prog.types;
    ignore(type_of prog.code);
    let (expr:Fun.expression) = tr_expr prog.code in
        expr