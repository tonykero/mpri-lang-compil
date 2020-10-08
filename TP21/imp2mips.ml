open Imp
open Ops
open Mips

let push reg = sw reg 0 sp  @@ subi sp sp 4
let pop  reg = addi sp sp 4 @@ lw reg 0 sp

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "__label_%i" !cpt
        
type allocation_info =
  | Local of int
  | Param of int

module STbl = Map.Make(String)
      
type allocation_context = {
  allocation: allocation_info STbl.t;
  nb_locals: int
}

let empty_allocation_context = { allocation = STbl.empty;
                                 nb_locals = 0; }
    
let mk_allocation_context fdef =
  let nb_params = List.length fdef.params in
  let nb_locals = List.length fdef.locals in

  let allocation = List.fold_right
    (let count = ref nb_params in
     fun id alloc -> STbl.add id (decr count; Param !count) alloc)
    fdef.params
    STbl.empty
  in
  let allocation = List.fold_right
    (let count = ref (-1) in
     fun id alloc -> STbl.add id (incr count; Local !count) alloc)
    fdef.locals
    allocation
  in
  
  { allocation; nb_locals }

(* Intègre une optimisation : le résultat est placé par défaut dans $t0
   plutôt que systématiquement sur la pile *)
let rec tr_expr e ctx = match e with
  | Cst(n) ->
    li t0 n
        
  | Bool(b) ->
    if b then li t0 (-1) else li t0 0

  | Var(id) -> begin
    try
      match STbl.find id ctx.allocation with
        | Param n -> lw t0 (4*(n+1)) fp
        | Local n -> lw t0 (-4*(n+2)) fp
    with
      | Not_found -> la t0 id @@ lw t0 0 t0
  end
          
  | Unop(uop, e) ->
    let op = match uop with
      | Minus -> neg
      | Not -> not_
    in
    tr_expr e ctx @@ op t0 t0
        
  | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add  -> add
      | Sub  -> sub
      | Mul  -> mul
      | Div  -> div
      | Rem  -> rem
      | Lsl  -> sll
      | Lsr  -> srl
      | Eq   -> seq
      | Neq  -> sne
      | Lt   -> slt
      | Le   -> sle
      | Gt   -> sgt
      | Ge   -> sge
      | And  -> and_
      | Or   -> or_
      in
    tr_expr e2 ctx @@ push t0 @@ tr_expr e1 ctx @@ pop t1 @@ op t0 t0 t1      

  | Call(id, params) ->
    let params_code =
      List.fold_right
        (fun e code -> code @@ tr_expr e ctx @@ push t0)
        params nop
    in
    params_code @@ jal id
    @@ addi sp sp (4 * List.length params)

  | Deref(e) ->
    tr_expr e ctx @@ lw t0 0 t0

  | Addr(id) ->
    la t0 id

  | PCall(f, params) ->
    let params_code =
      List.fold_right (fun e code -> code @@ tr_expr e ctx @@ push t0) params nop
    in
    params_code @@ tr_expr f ctx @@ jalr t0 
    @@ addi sp sp (4 * List.length params)

  | Sbrk(e) ->
    tr_expr e ctx @@ move a0 t0 @@ li v0 9 @@ syscall @@ move t0 v0

      
let rec tr_instr i ctx = match i with
  | Putchar(e) ->
    tr_expr e ctx @@ move a0 t0 @@ li v0 11 @@ syscall
        
  | Set(id, e) ->
    let set_code =
      try
        match STbl.find id ctx.allocation with
          | Param n -> sw t0 (4*(n+1)) fp
          | Local n -> sw t0 (-4*(n+2)) fp
      with
        | Not_found -> la t1 id @@ sw t0 0 t1
    in
    tr_expr e ctx @@ set_code
        
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    tr_expr c ctx
    @@ bnez t0 then_label
    @@ tr_seq s2 ctx
    @@ b end_label
    @@ label then_label
    @@ tr_seq s1 ctx
    @@ label end_label
        
  | While(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    b test_label
    @@ label code_label
    @@ tr_seq s ctx
    @@ label test_label
    @@ tr_expr c ctx
    @@ bnez t0 code_label
        
  | Return(e) ->
    tr_expr e ctx
    @@ move sp fp    (* Désallocation de la pile *)
    @@ lw ra (-4) fp (* Récupération de l'adresse de retour *)
    @@ lw fp 0 fp    (* Restauration du pointeur de base de l'appelant *)
    @@ jr ra

  | Write(d, e) ->
    tr_expr e ctx
    @@ push t0
    @@ tr_expr d ctx
    @@ pop t1
    @@ sw t1 0 t0

  | Expr(e) ->
    tr_expr e ctx
      
      

and tr_seq s ctx = match s with
    | []   -> nop
    | [i]  -> tr_instr i ctx
    | i::s -> tr_instr i ctx @@ tr_seq s ctx


let tr_function fdef =
  let context = mk_allocation_context fdef in
  push fp
  @@ push ra
  @@ addi fp sp 8
  @@ addi sp sp (-4 * context.nb_locals)
  @@ tr_seq fdef.code context
  (* @@ ici, erreur, on n'a pas croisé de return *)
  (* Pour éviter trop de corruption, on renvoie 0 *)
  @@ li t0 0
  @@ move sp fp    (* Désallocation de la pile *)
  @@ lw ra (-4) fp (* Récupération de l'adresse de retour *)
  @@ lw fp 0 fp    (* Restauration du pointeur de base de l'appelant *)
  @@ jr ra

    
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
    comment "built-in atoi"
    @@ label "atoi"
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

    @@ comment "built-in print_int"
    @@ label "print_int"
    @@ lw a0 4 sp
    @@ li v0 1
    @@ syscall
    @@ sw a0 0 sp
    @@ subi sp sp 4
    @@ jr ra
  
    @@ comment "built-in power"
    @@ label "power"
    @@ lw s0 8 sp
    @@ lw s1 4 sp
    @@ li t0 1
    @@ b "power_loop_guard"
    @@ label "power_loop_code"
    @@ mul t0 t0 s1
    @@ subi s0 s0 1
    @@ label "power_loop_guard"
    @@ bgtz s0 "power_loop_code"
    @@ sw t0 0 sp
    @@ subi sp sp 4
    @@ jr ra

  in

  let main_code = tr_seq prog.main empty_allocation_context in
  let function_codes = List.fold_right
    (fun fdef code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  let text = init @@ main_code @@ close @@ function_codes @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals (label "arg" @@ dword [0])
  in
  
  { text; data }
