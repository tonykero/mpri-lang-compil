open Imp
open Mips

type sym_table_type = (string, string list) Hashtbl.t

let (sym_table: sym_table_type) = Hashtbl.create 64
let current_function = ref("")

(* abandon push & pop for absolute adressing
 * but use it for function arguments/result
*)
let push reg = comment (Printf.sprintf "PUSH %s" reg) @@ sw reg 0 sp  @@ subi sp sp 4
let pop  reg = comment (Printf.sprintf "POP %s" reg) @@ addi sp sp 4 @@ lw reg 0 sp

let push_fp reg = comment (Printf.sprintf "PUSH %s" reg) @@ sw reg 0 fp  @@ subi fp fp 4
let pop_fp  reg = comment (Printf.sprintf "POP %s" reg) @@ addi fp fp 4 @@ lw reg 0 fp

(* replaces push & pop with absolute addressing *)
let addr = ref (-4)
let incr_offset () = addr := !addr + 4;!addr
let regs = [|t2;t3;t4;t5;t6;t7;t8;t9|]

let save reg addr =
    if (!current_function = "") then (*not in function*)
        let idx = addr/4 in
        if idx+2 <= 9 then
              comment (Printf.sprintf "STORE %s in $t%d" reg (idx+2))
          @@  move regs.(idx) reg
        else  comment (Printf.sprintf "STORE %s at %d" reg (addr-7*4))
          @@  sw reg (addr-7*4) sp
      else
            push reg

let load reg addr = 
    if (!current_function = "") then (*not in function*)
        let idx = addr/4 in
        if idx+2 <= 9 then
              comment (Printf.sprintf "LOAD %s from $t%d" reg (idx+2))
          @@  move reg regs.(idx)
        else  comment (Printf.sprintf "LOAD %s at %d" reg (addr-7*4))
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
          else index_of_i el l i+1
let index_of el l = index_of_i el l 0


let rec tr_binop op e1 e2=
  let offset = incr_offset () in
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
                        let params = (Hashtbl.find sym_table !current_function) in
                          if (List.exists (fun e -> e = str) params) then (*symbol is local*)
                              let idx = (index_of str params) in
                              lw t0 ((idx+1)*4) fp
                          else (*symbol is global*)
                              la t0 str
                          @@  lw t0 0 t0
                      else (* symbol is global*)
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
    | Call(id, se) -> let r = match id with 
                        | "print_int" -> if (List.length se) = 1 then
                                            let e = List.nth se 0 in
                                            let offset = !addr in
                                                  tr_expr e
                                              @@  addi sp sp offset
                                              @@  push t0
                                              @@  jal "print_int"
                                              @@  subi sp sp 4
                                              @@  pop t0
                                              @@  subi sp sp offset
                                          else failwith "print_int takes only one argument"
                        | "power" ->      if List.length se = 2 then
                                          let xe = List.nth se 0 in (*t0*)
                                          let ne = List.nth se 1 in (*t1*)
                                          let offset = incr_offset () in
                                                save t1 offset
                                            @@  tr_expr ne
                                            @@  move t1 t0
                                            @@  tr_expr xe
                                            @@  addi sp sp offset
                                            @@  push t1
                                            @@  push t0
                                            @@  jal "power"
                                            @@  subi sp sp 4
                                            @@  pop t0
                                            @@  subi sp sp offset
                                            @@  load t1 offset

                                          else failwith "power takes two argument"
                        | _ ->      if List.length se = 1 then
                                          let e1 = List.nth se 0 in
                                          let offset = incr_offset () in
                                          save t1 offset
                                      @@  tr_expr e1
                                      @@  addi sp sp offset
                                      @@  push t0
                                      @@  la t1 id
                                      @@  jalr t1
                                      @@  subi sp sp 4
                                      @@  pop t0
                                      @@  subi sp sp offset
                                      @@  load t1 offset
                                      else nop
                                    
                                        in r
and tr_instr i =
  match i with
    | Putchar(e) ->     tr_expr e
                    @@  move a0 t0
                    @@  li v0 11
                    @@  syscall
    | Set(str, e)->     let offset = incr_offset () in
                        save t1 offset
                    @@  tr_expr e
                    @@  la t1 str
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
    | Return e ->     
                      tr_expr e
                  @@  sw t0 0 fp
                  @@  jr ra
    | _ -> failwith "instr not implemented"
      
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
    @@ move fp sp
    @@ lw a0 4 fp
    @@ li v0 1
    @@ syscall
    @@ sw a0 0 fp
    @@ jr ra
    
    @@ comment "power"
    @@ label "power"
    @@ move fp sp
    @@ lw s0 8 fp
    @@ lw s1 4 fp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ sw t0 0 fp
    @@ jr ra
  in

  let main_code = tr_seq prog.main in
  let text = init @@ main_code @@ close @@ built_ins
          @@(List.fold_right
            (fun fun_def code ->  let fname = fun_def.name in
                                  let fcode = fun_def.code in
                                  let old_f = !current_function in
                                  Hashtbl.add sym_table fname fun_def.params;
                                  current_function := fname;
                                  let ret = 
                                  label fname
                              @@  move fp sp(*
                              @@  move a0 fp
                              @@  li v0 1
                              @@  syscall*)
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
