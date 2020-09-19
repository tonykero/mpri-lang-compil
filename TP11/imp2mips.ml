open Imp
open Mips
(* abandon push & pop for absolute adressing
let push reg = comment (Printf.sprintf "PUSH %s" reg) @@ sw reg 0 sp  @@ subi sp sp 4
let pop  reg = comment (Printf.sprintf "POP %s" reg) @@ addi sp sp 4 @@ lw reg 0 sp
*)

let regs = [|t2;t3;t4;t5;t6;t7;t8;t9|]

let addr = ref (-4)
let incr_offset () = addr := !addr + 4;!addr

let save reg addr =
    let idx = addr/4 in
    if idx+2 <= 9 then
          comment (Printf.sprintf "STORE %s in $t%d" reg (idx+2))
      @@  move regs.(idx) reg
    else  comment (Printf.sprintf "STORE %s at %d" reg (addr-7*4))
      @@  sw reg (addr-7*4) sp

let load reg addr = 
    let idx = addr/4 in
    if idx+2 <= 9 then
          comment (Printf.sprintf "LOAD %s from $t%d" reg (idx+2))
      @@  move reg regs.(idx)
    else  comment (Printf.sprintf "LOAD %s at %d" reg (addr-7*4))
      @@  lw reg (addr-7*4) sp


let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt

let int_of_bool b = if b then 1 else 0

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
    | _ -> failwith("Not implemented")
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
    | Var str ->      la t0 str
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
                              @@  not_ t0 t0 in r
    (*| _ -> failwith "expr not implemented"*)
      
let rec tr_instr i =
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
                          let start_label   = new_label () in
                              b test_label
                          @@  label start_label
                          @@  tr_seq se
                          @@  label test_label
                          @@  tr_expr cond
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
