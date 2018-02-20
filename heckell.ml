open Pprinting

let () = 
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.tokenize lexbuf in 
  print_string (string_of_program ast)