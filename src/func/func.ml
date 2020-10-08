let usage = "usage: ./func file.fun"

let spec = []
  
let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".fun") then
        raise (Arg.Bad "no .fun extension");
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Funparser.program Funlexer.token lb
  in
  close_in c;
  let progimp = Fun2imp.translate_program prog in
  let output_file = (Filename.chop_suffix file ".fun") ^ ".imp" in
  let out = open_out output_file in
  Imppp.pp_program progimp out;
  close_out out;
  exit 0
    
