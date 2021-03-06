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

   2. En cas d'erreur de lexique, de grammaire ou de type, produire un
      message d'erreur plus instructif, donnant par exemple la position de
      l'erreur (numéro de ligne et numéro de colonne) et une indication
      sur ce qui a été rencontré/ce qui était attendu.
      Ceci peut demander d'ajouter des informations à mettre à jour
      régulièrement dans les structures de données utilisées lors des analyses
      lexicale ou syntaxique, ou lors de l'évaluation.

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
  | PRINT | WHILE
  | SET  (* := *)
  | LP | RP (* (, ) *)
  | ADD | MULT | LT  (* < *)
  | EOF | BOF  (* fin de fichier, début de fichier *)

type input_buffer = {
  (* Mot d'entrée et sa longueur *)
  input: string;
  length: int;
  (* Début du mot analysé, et position courante *)
  mutable start_pos: int;
  mutable next_pos: int;
}

(* Initialisation *)
let init_buffer s = {
  input = s;
  length = String.length s;
  start_pos = 0;
  next_pos = 0;
}

exception Eof
  
(* Lire le caractère courant *)
let next_char b =
  if   b.next_pos < b.length
  then b.input.[b.next_pos]
  else raise Eof

(* Faire avancer le curseur *)
let shift b = b.next_pos <- b.next_pos + 1
  
(* Marquer le début du lexème en cours d'analyse *)
let init_pos b = b.start_pos <- b.next_pos

(* Obtenir le lexème analysé *)
let current_word b =
  String.sub b.input b.start_pos (b.next_pos - b.start_pos)

    
let rec read_token b =
  try  match next_char b with
    (* Un seul caractère : on avance, et on renvoie le lexème correspondant. *)
    | '{' -> shift b; BEGIN
    | '}' -> shift b; END
    | '(' -> shift b; LP
    | ')' -> shift b; RP
    | '+' -> shift b; ADD
    | '*' -> shift b; MULT
    | '<' -> shift b; LT
    | ';' -> shift b; SEMI
    (* Lexème potentiellement formé de plusieurs caractères : transition vers
       un nouvel état, c'est-à-dire appel d'une autre fonction.
       Si besoin, on initialise le curseur de début de lexème à ce moment. *)
    | ':' -> shift b; read_eq b
    | c when 'a' <= c && c <= 'z' -> init_pos b; shift b; read_word b
    | c when '0' <= c && c <= '9' -> init_pos b; shift b; read_int b
    (* On ignore les blancs (espaces, tabulations, retours chariot, etc.).
       Dans ce cas, on relance immédiatement l'analyse à partir du caractère
       suivant avec un nouvel appel à [read_token]. *)
    | ' ' | '\n' -> shift b; read_token b
    (* Tout autre caractère est une erreur. *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)
  with
    | Eof -> EOF

and read_eq b =
  match next_char b with
    (* On attend := *)
    | '=' -> shift b; SET
    (* Échec sinon *)
    | c   -> failwith (Printf.sprintf "Unrecognized character: %c" c)

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
	| "while" -> WHILE
	(* Sinon, c'est un identificateur. *)
	| id -> IDENT id
    )
(* Pour retrouver le mot courant, on aurait aussi pu ajouter un accumulateur
   à la fonction [read_word]. *)
      

(* Point débogage : de quoi afficher les lexèmes reconnus. *)
open Printf
let token_to_string = function
  | ADD -> "ADD"
  | BEGIN -> "BEGIN"
  | BOF -> "START"
  | END -> "END"
  | EOF -> "EOF"
  | IDENT s -> sprintf "IDENT %s" s
  | INT i -> sprintf "INT %d" i
  | LP -> "LP"
  | LT -> "LT"
  | MAIN -> "MAIN"
  | MULT -> "MULT"
  | PRINT -> "PRINT"
  | RP -> "RP"
  | SEMI -> "SEMI"
  | SET -> "SET"
  | WHILE -> "WHILE"

(** Syntaxe abstraite *)

(* Programme principal : un bloc de code. *)
type program = instruction

(* On a quatre formes d'instructions, dont la composition d'instructions et
   une structure de contrôle. *)
and instruction =
  | Print    of expression                (* [Print(e)]    ===  print(e)      *)
  | Set      of string * expression       (* [Set(id, e)]  ===  id := e       *)
  | While    of expression * instruction  (* [While(e, i)] ===  while (e) {b} *)
  | Sequence of instruction * instruction (* [Sequence(i1, i2)] === i1 ; i2   *)
      
(* Une expression est une constante, une variable, ou une opération binaire. *)
and expression =
  | Literal  of int
  | Location of string
  | Binop    of binop * expression * expression
and binop =
  | Add | Mult | Lt


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
  if t = next_token b
  then shift b
  else failwith (sprintf "Syntax error : %s expected" (token_to_string t))
      
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
    | SEMI -> shift b; let i2 = parse_instr b in Sequence(i1, i2)
    | _    -> i1
      
and parse_atom_instr b =
  match next_token b with
    | PRINT -> shift b; expect_token LP b; let e = parse_expr b in
					   expect_token RP b;
					   Print(e)
    | IDENT id -> shift b; expect_token SET b; let e  = parse_expr b in
                                               Set(id, e)
    | WHILE -> shift b; expect_token LP b; let e = parse_expr b in
                                           expect_token RP b;
                                           let i = parse_block b in
                                           While(e, i)
    | t -> failwith "Bad instruction"
			  
(* [expect_ident: token_buffer -> string] *)
and expect_ident b =
  match next_token b with
    | IDENT s -> shift b; s
    | t    -> failwith "Ident expected"

and parse_expr b =
  let e1 = parse_atom_expr b in
  match next_token b with
    | ADD | MULT | LT as op ->
      shift b; let e2 = parse_expr b in
	       let op = match op with
		 | ADD  -> Add
		 | MULT -> Mult
		 | LT   -> Lt
		 | _    -> assert false
               in Binop(op, e1, e2)
    | _ -> e1

and parse_atom_expr b =
  match next_token b with
    | INT i -> shift b; Literal i
    | IDENT id -> shift b; Location id
    | LP -> shift b; let e = parse_expr b in
                     let _ = expect_token RP b in
                     e
    | t -> failwith (sprintf "Bad expression : %s not allowed" (token_to_string t))

(* Point débogage : un afficheur pour la syntaxe abstraite. *)
let print_literal i = sprintf "%d" i
let print_location id = id
let print_binop = function
  | Add -> "+"
  | Mult -> "*"
  | Lt -> "<"
let rec print_expr = function
  | Literal lit -> print_literal lit
  | Location id -> print_location id
  | Binop(op, e1, e2) -> sprintf "( %s %s %s )" (print_expr e1) (print_binop op) (print_expr e2)

let offset o = String.make (2*o) ' '
let rec print_o_instr o i = (offset o) ^ (print_instr o i)
and print_instr o = function
  | Print(e) -> sprintf "print(%s)" (print_expr e)
  | Set(id, e) -> sprintf "%s := %s" id (print_expr e)
  | While(e, i) -> sprintf "while (%s) {\n%s%s}" (print_expr e) (print_instr (o+1) i) (offset o)
  | Sequence(i1, i2) -> sprintf "%s;\n%s" (print_o_instr o i1) (print_o_instr o i2)

let print_program p = sprintf "main {\n%s}" (print_o_instr 1 p) 


(** Interprétation *)

type value =
  | Int of int
  | Bool of bool
  
module State = Map.Make(String)
type state = value State.t
  
(* [eval_main: main -> unit] *)
let rec eval_program p x =
  eval_instruction (State.singleton "arg" (Int x)) p

(* [eval_instruction: state -> instruction -> state] *)
and eval_instruction env = function
  | Print (e) ->
    let i = match eval_expression env e with
      | Int i -> i
      | _ -> failwith "Expected integer value"
    in
    Printf.printf "%c" (char_of_int i); env
  | Set (id, e) -> State.add id (eval_expression env e) env
  | While (c, i) as iw ->
    let b = match eval_expression env c with
      | Bool b -> b
      | _ -> failwith "Expected boolean value"
    in
    if b
    then let env = eval_instruction env i in
	 eval_instruction env iw
    else env
  | Sequence (i1, i2) ->
    let env = eval_instruction env i1 in
    eval_instruction env i2

(* [eval_expression: state -> expression -> int] *)
and eval_expression env = function
  | Literal i -> Int i
  | Location id -> State.find id env
  | Binop(op, e1, e2) -> let v1 = eval_expression env e1 in
			 let v2 = eval_expression env e2 in
                         let i1, i2 = match v1, v2 with
                           | Int i1, Int i2 -> i1, i2
                           | _ -> failwith "Expected integer values"
                         in
			 let v = match op with
			   | Add  -> Int (i1 + i2)
			   | Mult -> Int (i1 * i2)
			   | Lt   -> Bool (i1 < i2)
			 in
                         v

(* Test final : interprétation du programme donné au début. *)
let main s x =
  let b = init_token_buffer s in
  let p = parse_program b in
  eval_program p x

let prog =
"main {
    continue := 1 < 2;
    i := 0;

    while (continue) {
        j := 0;
        while (((i*i) + (j*j)) < (arg*arg)) {
            print(46);
            print(32);
            j := j+1
        };
        continue := 0 < j;
        while (j < (arg+1)) {
            print(35);
            print(32);
            j := j+1
        };
        print(10);
        i := i+1
    }
}"

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
  
