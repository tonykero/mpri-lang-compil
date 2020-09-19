open Imp
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt

let int_of_bool b = if b then 1 else 0

let rec tr_binop op e1 e2= 
  let asm_pre =     tr_expr e1
                @@  tr_expr e2
                @@  pop t1
                @@  pop t0
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
    | _ -> failwith("Not implemented")
    in
  let asm_end = push t0
  in    asm_pre @@ asm_op @@ asm_end
and tr_binop_ri op e1 i =
    let asm_pre =   tr_expr e1
                @@  pop t0
              in
    let asm_op = match op with
      | Add -> addi t0 t0 i
      | Sub -> subi t0 t0 i
      | _   -> failwith "Unexpected operator with imm value"
              in
    let asm_end = push t0
              in
              asm_pre @@ asm_op @@ asm_end
and tr_binop_li op i e1 = let r = match op with
              | Sub ->    li t0 i
                      @@  tr_expr e1
                      @@  pop t1
                      @@  sub t0 t0 t1
                      @@  push t0
              | _ -> tr_binop_ri op e1 i
            in r
and tr_expr e =
  match e with
    | Cst i   ->      li t0 i
                  @@  push t0
    | Bool b  ->      (tr_expr (Cst (int_of_bool b)))
    | Var str ->      la t1 str
                  @@  lw t0 0 t1
                  @@  push t0
    | Binop(op, e1, e2) -> let r = match op with (* check for immediate op*)
                            | Add | Sub as op -> let r = match e1, e2 with
                                                  | _, Cst i -> tr_binop_ri op e1 i
                                                  | Cst i, _ -> tr_binop_li op i e2
                                                  | _ -> tr_binop op e1 e2
                                                in r
                            | _ ->  tr_binop op e1 e2 in r
    | Unop(op, e) -> let r = match op with
                    | Minus ->    tr_expr e
                              @@  pop t0
                              @@  sub t0 zero t0
                              @@  push t0
                    | Not ->      tr_expr e
                              @@  pop t0
                              @@  not_ t0 t0
                              @@  push t0 in r
    (*| _ -> failwith "expr not implemented"*)
      
let rec tr_instr i =
  match i with
    | Putchar(e) ->     tr_expr e
                    @@  pop a0
                    @@  li v0 11
                    @@  syscall
    | Set(str, e)->     tr_expr e
                    @@  pop t0
                    @@  la t1 str
                    @@  sw t0 0 t1
    | If(cond, se1, se2) ->   let else_label = new_label () in
                              let end_label  = new_label () in
                              tr_expr cond
                          @@  pop t0
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
                          let start_label   = new_label () in
                              b test_label
                          @@  label start_label
                          @@  tr_seq se
                          @@  label test_label
                          @@  tr_expr cond
                          @@  pop t0
                          @@  bnez t0 start_label
    (*| _ -> failwith "instr not implemented"*)
      
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
  in

  let main_code = tr_seq prog.main in
  let text = init @@ main_code @@ close @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals (label "arg" @@ dword [0])
  in
  
  { text; data }
