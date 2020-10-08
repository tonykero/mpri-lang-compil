{

  open Lexing
  open Funparser

    let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "fun",   FUN;
        "let",   LET;
        "rec",   REC;
        "in",    IN;
        "if",    IF;
        "then",  THEN;
        "else",  ELSE;
        "true",  BOOL true;
        "false", BOOL false;
        "mod",   MOD;
        "not",   NOT;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | ident as id
      { keyword_or_ident id }
  | "->"
      { ARROW }
  | "="
      { EQ }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | ">>"
      { LSR }
  | "<<"
      { LSL }
  | "<>"
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | ">"
      { GT }
  | ">="
      { GE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | ","
      { COMMA }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { failwith "unfinished comment" }
