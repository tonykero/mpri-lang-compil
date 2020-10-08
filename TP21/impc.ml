open Format

let usage = "usage: ./impc file.imp"

let spec = []
  
let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".imp") then
        raise (Arg.Bad "no .imp extension");
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
  let cm = open_in "malloc.imp" in
  let lbm = Lexing.from_channel cm in
  let malloc = Impparser.program Implexer.token lbm in
  close_in cm;
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Impparser.program Implexer.token lb in
  close_in c;
  let prog = Imp.merge malloc prog in
  let asm = Imp2mips.translate_program prog in
  let output_file = (Filename.chop_suffix file ".imp") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
