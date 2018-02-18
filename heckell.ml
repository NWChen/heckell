open Pprinting

let () = 
  let lexbuf = Lexing.from_channel stdin in
  let _ = Printf.printf("lexbuf\n") in
  let ast = Parser.program Scanner.tokenize lexbuf in
  let _ = Printf.printf("parser scanner\n") in
  print_string (Pprinting.string_of_program ast)