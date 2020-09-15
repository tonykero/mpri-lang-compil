(**
   Compilation et langages de programmation
   ----------------------------------------
   Thibaut Balabonski @ Université Paris-Saclay, 2020.


   9 Septembre 2020. Ouverture.
   Qu'est-ce qui définit un langage de programmation ?
*)


(******************************************************************************
   Objectifs pour le TP

   1. Ajouter quelques éléments au langage :
      - autres opérateurs arithmétiques ou logiques 
      - constantes booléennes true/false
      - branchements conditionnels if/then/else
    
        ( Added - / >)
        (Added printi built-in)
        (Added bool constants + implict conversion in binop)
        (Added If(e){i} and If(e){i1}else{i2} constructs, else{i} construct raise exception)
        (Added and or)

   2. En cas d'erreur de lexique, de grammaire ou de type, produire un
      message d'erreur plus instructif, donnant par exemple la position de
      l'erreur (numéro de ligne et numéro de colonne) et une indication
      sur ce qui a été rencontré/ce qui était attendu.
      Ceci peut demander d'ajouter des informations à mettre à jour
      régulièrement dans les structures de données utilisées lors des analyses
      lexicale ou syntaxique, ou lors de l'évaluation.

        (Fait)

   3. Modifier la grammaire des expressions arithmétiques pour se rapprocher
      des règles habituelles.
*******************************************************************************)


(**  Analyse lexicale *)

type token =
  | MAIN
  | BEGIN | END  (* {, } *)
  | IDENT of string
  | INT of int
  | SEMI  (* ; *)
  | PRINT | PRINTI | WHILE
  | SET  (* := *)
  | LP | RP (* (, ) *)
  | ADD | SUB | MULT | DIV | LT | GT | EQ | NEQ  (* < *)
  | TRUE | FALSE
  | AND | OR | NOT
  | IF | ELSE
  | EOF | BOF  (* fin de fichier, début de fichier *)

type input_buffer = {
  (* Mot d'entrée et sa longueur *)
  input: string;
  length: int;
  (* Début du mot analysé, et position courante *)
  mutable start_pos: int;
  mutable next_pos: int;

  mutable col_pos: int;
  mutable line_pos: int;
}

(* Initialisation *)
let init_buffer s = {
  input = s;
  length = String.length s;
  start_pos = 0;
  next_pos = 0;
  
  col_pos = 0;
  line_pos = 1;
}

exception Eof
  
(* Lire le caractère courant *)
let next_char b =
  if   b.next_pos < b.length
  then b.input.[b.next_pos]
  else raise Eof

(* Faire avancer le curseur *)
let shift b = b.col_pos <- b.col_pos + 1;b.next_pos <- b.next_pos + 1
let newline b = b.col_pos <- 0; b.line_pos <- b.line_pos + 1

let sprint_location b = (Printf.sprintf "at col %d line %d" b.col_pos b.line_pos)

(* Marquer le début du lexème en cours d'analyse *)
let init_pos b = b.start_pos <- b.next_pos

(* Obtenir le lexème analysé *)
let current_word b =
  String.sub b.input b.start_pos (b.next_pos - b.start_pos)

    
let rec read_token b =
  try 
  let prev_char = next_char b in
  match prev_char with
    (* Un seul caractère : on avance, et on renvoie le lexème correspondant. *)
    | '{' -> shift b; BEGIN
    | '}' -> shift b; END
    | '(' -> shift b; LP
    | ')' -> shift b; RP
    | '+' -> shift b; ADD
    | '-' -> shift b; SUB
    | '*' -> shift b; MULT
    | '/' -> shift b; DIV
    | '<' -> shift b; LT
    | '>' -> shift b; GT
    | ';' -> shift b; SEMI
    (* Lexème potentiellement formé de plusieurs caractères : transition vers
       un nouvel état, c'est-à-dire appel d'une autre fonction.
       Si besoin, on initialise le curseur de début de lexème à ce moment. *)
    | ':' -> shift b; read_eq b prev_char
    | '=' -> shift b; read_eq b prev_char
    | '!' -> shift b; read_eq b prev_char
    | c when 'a' <= c && c <= 'z' -> init_pos b; shift b; read_word b
    | c when '0' <= c && c <= '9' -> init_pos b; shift b; read_int b
    (* On ignore les blancs (espaces, tabulations, retours chariot, etc.).
       Dans ce cas, on relance immédiatement l'analyse à partir du caractère
       suivant avec un nouvel appel à [read_token]. *)
    | ' ' -> shift b; read_token b
    | '\n' -> shift b; newline b; read_token b
    (* Tout autre caractère est une erreur. *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c %s" c (sprint_location b))
  with
    | Eof -> EOF

and read_eq b p =
  match next_char b with
    (* On attend := *)
    | '=' -> shift b; (match p with
                      | ':' -> SET (* := *)
                      | '=' -> EQ  (* == *)
                      | '!' -> NEQ (* != *)
                      (* Échec sinon *)
                      | c   -> failwith (Printf.sprintf "Unrecognized character: %c expected one of : = ! at %s" c (sprint_location b))
                      )
    (* Échec sinon *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c expected = at %s" c (sprint_location b))

(* Reconnaissance d'un entier *)
and read_int b =
  match next_char b with
    (* Tant qu'on lit des chiffres, on avance et on continue avec le même état,
       c'est-à-dire la même fonction. *)
    | c when '0' <= c && c <= '9' -> shift b; read_int b
    (* Sinon on renvoie l'entier reconnu, et on préserve le prochain
       caractère. *)
    | _ -> INT (int_of_string (current_word b))
      
(* Lecture d'un mot *)
and read_word b =
  match next_char b with
    | c when 'a' <= c && c <= 'z' -> shift b; read_word b
    | _ -> (match current_word b with
	(* On commence par vérifier si le mot est un mot-clé. *)
	| "main" -> MAIN
	| "print" -> PRINT
	| "printi" -> PRINTI
  | "while" -> WHILE
  | "true" -> TRUE
  | "false" -> FALSE
  | "if" -> IF
  | "else" -> ELSE
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
	(* Sinon, c'est un identificateur. *)
	| id -> IDENT id
    )
(* Pour retrouver le mot courant, on aurait aussi pu ajouter un accumulateur
   à la fonction [read_word]. *)
      

(* Point débogage : de quoi afficher les lexèmes reconnus. *)
open Printf
let token_to_string = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | BEGIN -> "BEGIN"
  | BOF -> "START"
  | END -> "END"
  | EOF -> "EOF"
  | IDENT s -> sprintf "IDENT %s" s
  | INT i -> sprintf "INT %d" i
  | LP -> "LP"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | MAIN -> "MAIN"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | PRINT -> "PRINT"
  | PRINTI -> "PRINTI"
  | RP -> "RP"
  | SEMI -> "SEMI"
  | SET -> "SET"
  | WHILE -> "WHILE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | IF -> "IF"
  | ELSE -> "ELSE"
(** Syntaxe abstraite *)

(* Programme principal : un bloc de code. *)
type program = instruction

(* On a quatre formes d'instructions, dont la composition d'instructions et
   une structure de contrôle. *)
and instruction =
  | Print    of expression                (* [Print(e)]    ===  print(e)      *)
  | Printi   of expression                (* [Print(e)]    ===  print(e)      *)
  | Set      of string * expression       (* [Set(id, e)]  ===  id := e       *)
  | While    of expression * instruction  (* [While(e, i)] ===  while (e) {b} *)
  | Sequence of instruction * instruction (* [Sequence(i1, i2)] === i1 ; i2   *)
  | If       of expression * instruction
  | Else     of instruction
  | If_Else  of expression * instruction * instruction
  | None
(* Une expression est une constante, une variable, ou une opération binaire. *)
and expression =
  | Literal       of int
  | Literal_bool  of bool
  | Location  of string
  | Binop     of binop * expression * expression
  | Unop      of unop * expression
and binop =
  | Add | Sub | Mult | Div
  | Lt | Gt | Eq | Neq
  | And | Or
and unop =
  | Not
  | Sub


(** Analyse syntaxique  *)
      
type token_buffer = {
  (* Entrée brute, d'où lire les lexèmes *)
  input: input_buffer;
  (* Lexème courant, initialisé à [BOF] *)
  mutable next_token: token;
}

(* Consulter le lexème courant *)
let next_token b = b.next_token

(* Faire avancer le curseur : lire le prochain lexème de l'entrée *)
let shift b = b.next_token <- read_token b.input

(* Initialisation *)
let init_token_buffer s =
  { input = init_buffer s; next_token = BOF }
  
(* [parse_program: token_buffer -> program] *)
(* [main] <-  BOF main [block] EOF *)
let rec parse_program b =
  (* On a prévu que les premiers lexèmes soient toujours BOF et main *)
  expect_token BOF b;
  expect_token MAIN b;
  (* On rencontre ensuite le non terminal [block] : appel récursif dont
     on récupère le résultat. *)
  let code = parse_block b in
  (* Enfin, on doit finir par EOF *)
  expect_token EOF b;
  (* Une fois la règle reconnue, on renvoie la construction correspondante,
     de type [program].
  *)
  code

(* Vérification de l'identité du prochain terminal. *)
and expect_token t b =
  let actual = next_token b in
  if t = actual
  then shift b
  else failwith (sprintf "Syntax error : expected %s but got %s %s" (token_to_string t)
                                                                    (token_to_string actual)
                                                                    (sprint_location b.input))
      
(* [parse_block: token_buffer -> instruction] *)
(* [block] <-  BEGIN [instr] END *)
and parse_block b =
  expect_token BEGIN b;
  let i = parse_instr b in
  expect_token END b;
  i

and parse_instr b =
  let i1 = parse_atom_instr b in
  match next_token b with
    | SEMI -> shift b; if next_token b == END then
                        Sequence(i1, None)
    else let i2 = parse_instr b in Sequence(i1, i2)
    | _    -> i1 
      
and parse_atom_instr ?(if_c=false) b =
  match next_token b with
    | PRINT -> shift b; expect_token LP b; let e = parse_expr b in
					   expect_token RP b;
					   Print(e)
    | PRINTI -> shift b; expect_token LP b; let e = parse_expr b in
					   expect_token RP b;
					   Printi(e)
    | IDENT id -> shift b; expect_token SET b; let e  = parse_expr b in
                                               Set(id, e)
    | WHILE -> shift b; expect_token LP b; let e = parse_expr b in
                                           expect_token RP b;
                                           let i = parse_block b in
                                           While(e, i)
    | IF -> shift b; expect_token LP b; let e = parse_expr b in
                                        expect_token RP b;
                                        let i = parse_block b in
                                        (match parse_atom_instr ~if_c:true b with
                                        | Else(i2) -> If_Else(e, i, i2)
                                        | _ -> If(e, i)
                                        )
    | ELSE -> shift b; if if_c then Else(parse_block b)
                        else failwith (sprintf "Else block without previous If block %s" (sprint_location b.input))
    | t -> failwith (sprintf "Bad instruction : %s not allowed %s" (token_to_string t) (sprint_location b.input))
			  
(* [expect_ident: token_buffer -> string] *)
and expect_ident b =
  match next_token b with
    | IDENT s -> shift b; s
    | t    -> failwith "Ident expected"



    
(* logical    = relational (and|or relational)*                     *)
(* relational = term ((<|>|==|!=) term)* *)
(* term       = factor ((+|-) factor)*                  *)
(* factor     = atom ((*|/) atom)*                      *)
(* atom       = (-|not) atom_expr                       *)*)
and parse_atom b = 
  match next_token b with
  | NOT -> shift b; Unop(Not, parse_atom_expr b)
  | SUB -> shift b; Unop(Sub, parse_atom_expr b)
  | _ -> parse_atom_expr b
and parse_factor b = 
    let e1 = parse_atom b in
    match next_token b with
    | MULT | DIV as op ->
      shift b; let e2 = parse_atom b in
          let op = match op with
            | MULT -> Mult
            | DIV -> Div
            | _ -> assert false
            in Binop(op, e1, e2)
    | _ -> e1
and parse_term b = 
let e1 = parse_factor b in
match next_token b with
| ADD | SUB as op ->
  shift b; let e2 = parse_factor b in
      let op = match op with
        | ADD -> Add
        | SUB -> Sub
        | _ -> assert false
        in Binop(op, e1, e2)
| _ -> e1
and parse_relational b = 
    let e1 = parse_term b in
    match next_token b with
    | LT | GT | EQ | NEQ as op ->
        shift b; let e2 = parse_term b in
            let op = match op with
              | LT -> Lt
              | GT -> Gt
              | EQ -> Eq
              | NEQ -> Neq
              | _ -> assert false
              in Binop(op, e1, e2)
    | _ -> e1
and parse_expr b =
  let e1 = parse_relational b in
  match next_token b with
    | AND | OR as op ->
      shift b; let e2 = parse_relational b in
	       let op = match op with
		      | AND  -> And
		      | OR  -> Or
          | _ -> assert false
          in Binop(op, e1, e2)
    | _ -> e1;
    (*| _ -> failwith (sprintf "bad token %s %s" (token_to_string (next_token b)) (sprint_location b.input))*)

and parse_atom_expr b =
  match next_token b with
    | INT i -> shift b; Literal i
    | TRUE -> shift b; Literal_bool true
    | FALSE -> shift b; Literal_bool false
    | IDENT id -> shift b; Location id
    | LP -> shift b; let e = parse_expr b in
                     let _ = expect_token RP b in
                     e
    | t -> failwith (sprintf "Bad expression : %s not allowed %s" (token_to_string t) (sprint_location b.input))

(* Point débogage : un afficheur pour la syntaxe abstraite. *)
let print_literal i = sprintf "%d" i
let print_location id = id
let int_of_bool b = if b then 1 else 0
let print_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "=="
  | Neq -> "!="
  | And -> "and"
  | Or -> "or"
let print_unop = function
  | Not -> "not"
  | Sub -> "-"
let rec print_expr = function
  | Literal lit -> print_literal lit
  | Literal_bool lit -> (sprintf "%b" lit)
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expr e1) (print_binop op) (print_expr e2)
  | Unop(op, e) -> sprintf "(%s %s)" (print_unop op) (print_expr e)
let offset o = String.make (2*o) ' '
let rec print_o_instr o i = (offset o) ^ (print_instr o i)
and print_instr o = function
  | Print(e) -> sprintf "print(%s)" (print_expr e)
  | Printi(e) -> sprintf "printi(%s)" (print_expr e)
  | Set(id, e) -> sprintf "%s := %s" id (print_expr e)
  | While(e, i) -> sprintf "while (%s) {\n%s%s}" (print_expr e) (print_instr (o+1) i) (offset o)
  | Sequence(i1, i2) -> sprintf "%s;\n%s" (print_o_instr o i1) (print_o_instr o i2)
  | If(e, i) -> sprintf "if (%s) {\n%s}" (print_expr e) (print_instr o i)
  | Else(i) -> sprintf "else {\n%s}" (print_instr o i)
  | If_Else(e, i1, i2) -> sprintf "if (%s) {\n%s} else {\n%s}" (print_expr e) (print_o_instr o i1) (print_o_instr o i2)
  | None -> sprintf ""
let print_program p = sprintf "main {\n%s}" (print_o_instr 1 p) 


(** Interprétation *)

type value =
  | Int of int
  | Bool of bool
  
module State = Map.Make(String)
type state = value State.t

let do_arithmetic op i1 i2 =
    match op with
      | Add -> Int (i1 + i2)
      | Sub -> Int (i1 - i2)
      | Mult -> Int (i1 * i2)
      | Div -> Int (i1 / i2)
      | Gt -> Bool (i1 > i2)
      | Lt -> Bool (i1 < i2)
      | _ -> failwith (sprintf "Unknown arithmetic op %s" (print_binop op))
let do_logical op i1 i2 = 
    match op with
      | And -> Bool (i1 && i2)
      | Or -> Bool (i1 || i2)
      | _ -> failwith (sprintf "Unknown logical op %s" (print_binop op))
  
(* [eval_main: main -> unit] *)
let rec eval_program p x =
  eval_instruction (State.singleton "arg" (Int x)) p

(* [eval_instruction: state -> instruction -> state] *)
and eval_instruction env = function
  | Print (e) ->
    let i = match eval_expression env e with
      | Int i -> i
      | _ -> failwith (sprintf "Expected integer value in print(%s)" (print_expr e))
    in
    Printf.printf "%c" (char_of_int i); env
  | Printi (e) ->
    let i = match eval_expression env e with
      | Int i -> i
      | _ -> failwith (sprintf "Expected integer value in printi(%s)" (print_expr e))
    in
    Printf.printf "%d\n" i; env
  | Set (id, e) -> State.add id (eval_expression env e) env
  | While (c, i) as iw ->
    let b = match eval_expression env c with
      | Bool b -> b
      | _ -> failwith (sprintf "Expected boolean value in condition while(%s)" (print_expr c))
    in
    if b
    then let env = eval_instruction env i in
	 eval_instruction env iw
    else env
  | If(e, i) -> let b = match eval_expression env e with
                | Bool b -> b
                | _ -> failwith (sprintf "Expected boolean value in condition if(%s)" (print_expr e))
                in
                if b then eval_instruction env i
                else env
  | Else(i) -> env (*ignore solo else block*)
  | If_Else(e, i1, i2) -> let b = match eval_expression env e with
                          | Bool b -> b
                          | _ -> failwith (sprintf "Expected boolean value in condition if(%s)" (print_expr e))
                          in
                          if b then eval_instruction env i1
                          else eval_instruction env i2
  | Sequence (i1, i2) ->
    let env = eval_instruction env i1 in
    eval_instruction env i2
  | None -> env
(* [eval_expression: state -> expression -> int] *)
and eval_expression env = function
  | Literal i -> Int i
  | Literal_bool b -> Bool b
  | Location id -> State.find id env
  | Binop(op, e1, e2) -> let v1 = eval_expression env e1 in
			 let v2 = eval_expression env e2 in
       let v = match op with
        (*integer-only*)
         | Add | Sub | Mult | Div | Gt | Lt -> let v = match v1, v2 with
                                              | Int i1, Int i2 -> (do_arithmetic op i1 i2)
                                              | _ , Int i2  -> failwith (sprintf "Expected integer values in expr %s" (print_expr e1))
                                              | Int i1 , _  -> failwith (sprintf "Expected integer values in expr %s" (print_expr e2))
                                              | _ -> failwith (sprintf "Expected integer values in %s" (print_expr (Binop(op, e1, e2))))  
                                              in v
        (*boolean-only*)
        | And | Or -> let v = match v1, v2 with
                                | Bool i1, Bool i2 -> (do_logical op i1 i2)
                                | _ , Bool i2  -> failwith (sprintf "Expected boolean values in expr %s" (print_expr e1))
                                | Bool i1 , _  -> failwith (sprintf "Expected boolean values in expr %s" (print_expr e2))
                                | _ -> failwith (sprintf "Expected boolean values in %s" (print_expr (Binop(op, e1, e2))))
                                in v
        (*both*)
         | Eq   -> Bool (v1 == v2)
         | Neq   -> Bool (v1 != v2)
                              in v
  | Unop(op, e) -> let v1 = (eval_expression env e) in
                   let v = match op with
                            | Not -> let v = match v1 with
                                            | Bool b -> Bool (not b)
                                            | _ -> failwith (sprintf "Expected boolean value in expr %s" (print_expr e))
                                            in v
                            | Sub -> let v = match v1 with
                                            | Int i -> Int (-i)
                                            | _ -> failwith (sprintf "Expected integer value in expr %s" (print_expr e))
                                            in v
                            in v

(* Test final : interprétation du programme donné au début. *)
let main s x =
  let b = init_token_buffer s in
  let p = parse_program b in
  print_endline (print_program p);
  eval_program p x
  

let prog =
"main {
  continue := true;
  i := 0;

  while (continue) {
    continue := false;
    j := 0;
    while (j < arg+1) {
      if (-(i*i + j*j) > -arg*arg) {
        print(46);
        continue := true;
      } else {
        print(35);
      };
      print(32);
      j := j+1;
    };
    print(10);
    i := i+1;
  };
}
"

let _ = main prog 36

(* L'exécution doit produire l'arc de cercle suivant :

. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . . . # # # # # # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . . . # # # # # # # # # # # # # # # # # # # # 
. . . . . . . . . . . . . . . # # # # # # # # # # # # # # # # # # # # # # 
. . . . . . . . . . . . # # # # # # # # # # # # # # # # # # # # # # # # # 
. . . . . . . . . # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
main {
    b := false;
    a := true;
    if((b) and not a) {
      printi(-1);
      printi(2 + 3 * 5);
      printi( (-(2+3)) * 5 );
      printi( 2 + 3 * (5 - 1) )
    } else {
      printi(1 - -1)
    }
}
*)

(**
   Langage Imp : échantillon étendu
   --------------------------------
   Si vous modifiez ce fichier pour intégrer toutes les constructions
   citées dans la première suggestion de travail à la maison, vous pourrez
   interpréter la variante suivante du programme précédent.

   main {
     continue := true;
     i := 0;

     while (continue) {
       continue := false;
       j := 0;
       while (j < arg+1) {
         if (i*i + j*j < arg*arg) {
           print(46);
           continue := true;
         } else {
           print(35);
         };
         print(32);
         j := j+1;
       };
       print(10);
       i := i+1;
     };
   }
*)
  
