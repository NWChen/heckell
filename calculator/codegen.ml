open Ast
module StringMap = Map.Make (String);;

let rec eval map expr =
  match expr with
    Lit(x) -> (x, map)

  | Binop(e1, op, e2) ->
    let pair1 = eval map e1 in (*and pair2 = eval m2 e2 in*)
    let map2 = snd pair1 in 
    let pair2 = eval map2 e2 in

    let v1 = fst pair1 and v2 = fst pair2 in

    let _ = print_endline (string_of_int v1) in

    ( match op with 
      Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2, (snd pair2))

let rec eval map stmt =
  match stmt with
  | Decl(var, typ) ->
    () (* TODO *)

  | Seq(e1, e2) ->
    let pair = eval map e1 in
      eval (snd pair) e2

  | Asn(var, e) -> 
    let pair = eval map e in
    let res = fst pair and map = snd pair in
    let map = if StringMap.mem var map then map else StringMap.add var res map in (res, map)

let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lex_buf in
  let (result, _) = eval StringMap.empty expr in 
  print_endline (string_of_int result)
