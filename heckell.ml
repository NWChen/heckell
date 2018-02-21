open Pprinting

let () = 
  let lexbuf = Lexing.from_channel stdin in
  let _ = Parser.program Scanner.tokenize lexbuf in ()
