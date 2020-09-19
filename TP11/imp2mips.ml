open Imp
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt
        
let rec tr_expr e =
  match e with
    | Cst i   ->      li t0 i
                  @@  push t0
    | Var str ->      la t1 str
                  @@  lw t0 0 t1
                  @@  push t0
    | _ -> failwith "not implemented"
      
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
    | _ -> failwith "not implemented"
      
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
