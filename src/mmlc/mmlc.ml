let usage = "usage: ./mmlc file.mml"

let spec = []
  
let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".mml") then
        raise (Arg.Bad "no .mml extension");
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Mmlparser.program Mmllexer.token lb
  in
  close_in c;
  let progfun = Mml2fun.translate_program prog in
  let output_file = (Filename.chop_suffix file ".mml") ^ ".fun" in
  let out = open_out output_file in
  Funpp.pp_program progfun out;
  close_out out;
  exit 0
    
