open Imp
open Mips

(* associate function name to fixed-length array of size 2,
    containing  parameter names
            and local variable names
*)
type sym_table_type = (string, string list array) Hashtbl.t
let (sym_table: sym_table_type) = Hashtbl.create 64
let current_function = ref("")

let params_from fun_name = (Hashtbl.find sym_table !current_function).(0)
let locals_from fun_name = (Hashtbl.find sym_table !current_function).(1)
let idx_to_local idx = (idx+2)*(-4)  (*[-inf;-8]*)
let idx_to_param idx = (idx+1)*(4)   (*[4;+inf]*)
(* abandon push & pop for absolute adressing
 * but use it for function arguments/result
*)
let push reg = comment (Printf.sprintf "PUSH %s" reg) @@ sw reg 0 sp  @@ subi sp sp 4
let pop  reg = comment (Printf.sprintf "POP %s" reg) @@ addi sp sp 4 @@ lw reg 0 sp

(* replaces push & pop with absolute addressing *)
let addr = ref (-4)
let incr_offset () = addr := !addr + 4;!addr

(* registers + stack extension *)
let regs = [|t2;t3;t4;t5;t6;t7;t8;t9|]

let save reg addr =
    if (!current_function = "") then (*not in function*)
        let idx = addr/4 in
        if idx < (Array.length regs) then
              comment (Printf.sprintf "STORE %s in $t%d" reg (idx+2))
          @@  move regs.(idx) reg
        else  comment (Printf.sprintf "STORE %s at %d" reg (-(addr-7*4)))
          @@  sw reg (addr-7*4) sp
      else
            push reg

let load reg addr = 
    if (!current_function = "") then (*not in function*)
        let idx = addr/4 in
        if idx < (Array.length regs) then
              comment (Printf.sprintf "LOAD %s from $t%d" reg (idx+2))
          @@  move reg regs.(idx)
        else  comment (Printf.sprintf "LOAD %s at %d" reg (-(addr-7*4)))
          @@  lw reg (addr-7*4) sp
      else
        pop reg

(* generate new label *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt

(* keep track of current label test and label end in a loop 
   for break & continue instructions
*)
let cur_loop_test = ref ("")
let cur_loop_end = ref ("")

(* utils *)
let int_of_bool b = if b then 1 else 0
let rec index_of_i el l i =
        if List.nth l i = el then i
          else index_of_i el l (i+1)
let index_of el l = index_of_i el l 0
let in_list el l = (List.exists (fun e -> e = el) l)

type addr_type = 
    | Str of string
    | Ex of Imp.expression

let rec call_procedure (addr:addr_type) se = 
        let nbr_param = (List.length se) in
        let offset = incr_offset () in
            save t1 offset
        @@  subi sp sp offset (*switch to relative addressing*)
        @@  (List.fold_left
              (fun code expr -> tr_expr expr @@ push t0 @@ code )
              nop se
            )
            @@ let r = match addr with
                | Str(s) -> la t1 s
                | Ex(e) -> tr_expr e @@ move t1 t0 in r
        (* [An, ..., A1], sp = $A1+1*)
        @@  jalr t1             (* [An, ..., A1, res], sp = &res + 1*)
        @@  pop t0              (*  t0 = res *)
        @@  addi sp sp (nbr_param*4)       (* n + 4, sp = &An-1*)
                                (* [], sp=(&res)+1*)
        @@  addi sp sp offset   (*back to absolute addressing with res on top*)
        @@  load t1 offset
and tr_lazy_op op e1 e2 = 
  let asm_pre   =   tr_expr e1 in
  let end_label = new_label () in
  match op with
    | And -> Some   (asm_pre
                @@  beqz t0 end_label (*if false -> false*)
                @@  tr_expr e2        (*if e1 true -> e2*)
                @@  label end_label)
    | Or -> Some    (asm_pre
                @@  bnez t0 end_label (*if true -> true*)
                @@  tr_expr e2        (*if e1 false -> e2*)
                @@  label end_label
                @@  nop)
    | _ -> None
and tr_binop op e1 e2=
  let maybe_lazy_op = tr_lazy_op op e1 e2 in
  let r = match maybe_lazy_op with
    | None -> let offset = incr_offset () in
              let asm_pre =     save t1 offset
                            @@  tr_expr e2
                            @@  move t1 t0
                            @@  tr_expr e1
                          in
              let asm_op = match op with
                | Add ->    add t0 t0 t1
                | Sub ->    sub t0 t0 t1
                | Mul ->    mul t0 t0 t1
                | Div ->    div t0 t0 t1
                | Rem ->    rem t0 t0 t1
                | Eq  ->    seq t0 t0 t1
                | Neq ->    sne t0 t0 t1
                | Lt  ->    slt t0 t0 t1
                | Le  ->    sle t0 t0 t1
                | Gt  ->    sgt t0 t0 t1
                | Ge  ->    sge t0 t0 t1
                | And ->   and_ t0 t0 t1
                | Or  ->    or_ t0 t0 t1
                | _ -> failwith("Binary operator not implemented")
                in
              let asm_end = load t1 offset
              in    asm_pre @@ asm_op @@ asm_end
    | _ -> Option.get maybe_lazy_op in r
and tr_binop_ri op e1 i =
    let asm_pre =   tr_expr e1
              in
    let asm_op = match op with
      | Add -> addi t0 t0 i
      | Sub -> subi t0 t0 i
      | _   -> failwith "Unexpected operator with imm value"
              in
    let asm_end = nop
              in
              asm_pre @@ asm_op @@ asm_end
and tr_binop_li op i e1 = let r = match op with
              | Sub -> let offset = incr_offset () in
                          save t1 offset
                      @@  tr_expr e1
                      @@  move t1 t0
                      @@  li t0 i
                      @@  sub t0 t0 t1
                      @@  load t1 offset
              | _ -> tr_binop_ri op e1 i
            in r
and tr_expr e =
  match e with
    | Cst i   ->      li t0 i
    | Bool b  ->      (tr_expr (Cst (int_of_bool b)))
    | Var str ->      if (!current_function) != "" then  (* if we're in a function*)
                        let params = params_from sym_table in
                        let locals = locals_from sym_table in
                        if (in_list str locals) then (*symbol is local*)
                            let idx = (index_of str locals) in
                            lw t0 (idx_to_local idx) fp       (*[-inf;-8] relative to fp*)
                        else if (in_list str params) then (*symbol is param*)
                            let idx = (index_of str params) in
                            lw t0 (idx_to_param idx) fp          (*[4;+inf] relative to fp*)
                        else (* assume symbol is global*)
                            la t0 str
                        @@  lw t0 0 t0
                      else (* assume symbol is global*)
                          la t0 str
                      @@  lw t0 0 t0 

    | Binop(op, e1, e2) -> let r = match op with (* check for immediate op*)
                            | Add | Sub as op -> let r = match e1, e2 with
                                                  | _, Cst i -> tr_binop_ri op e1 i
                                                  | Cst i, _ -> tr_binop_li op i e2
                                                  | _ -> tr_binop op e1 e2
                                                in r
                            | _ ->  tr_binop op e1 e2 in r
    | Unop(op, e) -> let r = match op with
                    | Minus ->    tr_expr e
                              @@  sub t0 zero t0
                    | Not ->      tr_expr e
                              @@  not_ t0 t0
                    in r
    | Call(id, se) -> call_procedure (Str(id)) se
    | Sbrk(e) ->    tr_expr e
                @@  move a0 t0
                @@  li v0 9
                @@  syscall
                @@  move t0 v0
    | Deref(e) ->   tr_expr e
                @@  lw t0 0 t0
    | Addr(str) ->  la t0 str
    | PCall(addr, se) -> call_procedure (Ex(addr)) se
    | _ -> failwith "Expression not implemented"
and tr_instr i =
  match i with
    | Write(ea, e) ->   let offset = incr_offset () in
                        save t1 offset
                    @@  tr_expr ea
                    @@  move t1 t0
                    @@  tr_expr e
                    @@  sw t0 0 t1
                    @@  load t1 offset

    | Expr(e) -> tr_expr e
    | Set(str, e)->     let offset = incr_offset () in
                        save t1 offset
                    @@  tr_expr e
                    @@  if (!current_function != "") then (*in a function*)
                            let params = params_from !current_function in
                            let locals = locals_from !current_function in
                            if in_list str locals then 
                                let idx = index_of str locals in
                                sw t0 (idx_to_local idx) fp
                            else if in_list str params then
                                let idx = index_of str params in
                                sw t0 (idx_to_param idx) fp
                            else
                                la t1 str
                            @@  sw t0 0 t1
                        else
                              la t1 str
                          @@  sw t0 0 t1
                    @@  load t1 offset
    | If(cond, se1, se2) ->   let else_label = new_label () in
                              let end_label  = new_label () in
                              tr_expr cond
                          (*if false -> else*)
                          @@  beqz t0 else_label
                          (*if true*)
                          @@  tr_seq se1
                          @@  b end_label
                          (*if false*)
                          @@  label else_label
                          @@  tr_seq se2
                          @@  label end_label
    | While(cond, se) ->  let test_label  = new_label () in
                          let start_label = new_label () in
                          let end_label   = new_label () in
                          let old_loop_test = !cur_loop_test in
                          let old_loop_end = !cur_loop_end in
                              cur_loop_test := test_label;
                              cur_loop_end := end_label;
                          let ret = 
                              b test_label
                          @@  label start_label
                          @@  tr_seq se
                          @@  label test_label
                          @@  tr_expr cond
                          @@  bnez t0 start_label
                          @@  label end_label
                          in 
                          cur_loop_test := old_loop_test;
                          cur_loop_end  := old_loop_end; 
                          ret
    (*for is just syntaxic sugar*)
    | For(init, cond, iter, s) -> (tr_seq ([init] @ [While(cond, s @ [iter])]) )
    | Break -> b !cur_loop_end
    | Continue -> b !cur_loop_test
    | Return e ->     (*[An, ..., A1, old_fp,ra,x0,...,xn, garbage], fp=&old_fp, sp=&garbage*)
                      tr_expr e     (*[An, ..., A1, old_fp,ra,x0,...,xn, garbage],, t0 = res, fp=&old_fp, sp=&garbage*)
                  @@  subi sp fp 8  (*sp =&ra+1*)
                  @@  pop ra        (*[An, ..., A1, old_fp], t0 = res fp=&old_fp, ra restored*)
                  @@  pop fp        (*[An, ..., A1], t0 = res fp restored*)
                  @@  push t0    (*[An, ..., A1, res],t0 = res , sp=&res+1, fp=old_fp*)
                  @@  jr ra
      
and tr_seq = function
  | []   -> nop
  | [i]  -> tr_instr i
  | i::s -> tr_instr i @@ tr_seq s

    
let translate_program prog =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ la t0 "arg"
    @@ sw v0 0 t0
    @@ label "init_end"
  and close =
    li v0 10
    @@ syscall
  and built_ins =
    label "atoi"
    @@ move t0 a0
    @@ li   t1 0
    @@ li   t2 10
    @@ label "atoi_loop"
    @@ lbu  t3 0 t0
    @@ beq  t3 zero "atoi_end"
    @@ li   t4 48
    @@ blt  t3 t4 "atoi_error"
    @@ li   t4 57
    @@ bgt  t3 t4 "atoi_error"
    @@ addi t3 t3 (-48)
    @@ mul  t1 t1 t2
    @@ add  t1 t1 t3
    @@ addi t0 t0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ move v0 t1
    @@ jr   ra
    @@ comment "print_int"
    @@ label "print_int"
    @@ move t0 sp
    @@ push fp
    @@ push ra
    @@ move fp t0
    @@ lw a0 4 fp
    @@ li v0 1
    @@ syscall
    (*epilogue*)
    @@ pop ra
    @@ pop fp
    @@ push a0
    @@ jr ra

    @@ comment "putchar"
    @@ label "putchar"
    @@ move t0 sp
    @@ push fp
    @@ push ra
    @@ move fp t0
    @@ lw a0 4 fp
    @@ li v0 11
    @@ syscall
    @@ pop ra
    @@ pop fp
    @@ push a0
    @@ jr ra

    @@ comment "power"
    @@ label "power"
    @@ move t0 sp
    @@ push fp
    @@ push ra
    @@ move fp t0
    @@ lw s0 8 fp
    @@ lw s1 4 fp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    (*epilogue*)
    @@ pop ra
    @@ pop fp
    @@ push t0
    @@ jr ra
    
  in

  let main_code = tr_seq prog.main in
  let text = init @@ main_code @@ close @@ built_ins
          @@(List.fold_right
            (fun fun_def code ->  let fname = fun_def.name in
                                  let fcode = fun_def.code in
                                  let old_f = !current_function in
                                  Hashtbl.add sym_table fname [|fun_def.params;fun_def.locals|];
                                  current_function := fname;
                                  let ret = 
                                  label fname
                              @@  move t0 sp    (*tmp save sp*)
                              @@  push fp       (*[An,...A1,old_fp]*)
                              @@  push ra       (*[An,...A1,old_fp,ra] sp = &ra+1*)
                              @@  move fp t0    (*[An, ..., A1, old_fp,ra] fp = &old_fp, sp =&ra+1*)
                              @@  subi sp sp ((List.length fun_def.locals)*4) (*reserve space for locals*)
                              @@  tr_seq fcode
                              @@  code in
                                  current_function := old_f; ret
              )
              prog.functions nop
              )
  and data = (List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals (label "arg" @@ dword [0])) 
  in
  
  { text; data }
