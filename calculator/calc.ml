(*
 * Problem 3
 * calc.ml
 *)

open Ast
module StringMap = Map.Make (String);;

let rec eval map expr =
  match expr with
    Lit(x) -> (x, map)
  | Seq(e1, e2) ->
    let pair = eval map e1 in (* `pair` is an `int * StringMap` tuple *)
      eval (snd pair) e2        (* `snd pair` is the map in `pair`; we continue evaluating with this new map *)
  | Asn(var, e) -> 
    let pair = eval map e in
    let res = fst pair and map = snd pair in (* `map` here shadows the earlier definition of `map` *)
    let map = if StringMap.mem var map then map else StringMap.add var res map in (* update `map` accordingly *)
      (res, map)
  | Var(v) -> (StringMap.find v map, map)      (* we can assume a Var has already been assigned, and thus has an entry in `map` *)
  | Binop(e1, op, e2) ->
    let pair1 = eval map e1 and pair2 = eval map e2 in
    let v1 = fst pair1 and v2 = fst pair2 in
    match op with
      Add -> (v1 + v2, map)
      | Sub -> (v1 - v2, map)
      | Mul -> (v1 * v2, map)
      | Div -> (v1 / v2, map)

let () =
  let lex_buf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lex_buf in
  let (result, _) = eval StringMap.empty expr in 
  print_endline (string_of_int result)

(*
 *
 * CODE DEMONSTRATION
 * ==================
 * Command:
 * Result
 *
 * $ make
 * 
 * $ echo "a = 3, six = num = 6, a * num + six" | ./calc
 * 24
 *
 * $ echo "3 + x = 4" | ./calc
 * 7
 *
 * $ echo "101" | ./calc
 * 101
 *
 * $ echo "foo = 101" | ./calc
 * 101
 *
 * $ echo "d = o = g = 3, d * o / g + d - o - g" | ./calc
 * 0
 *
 *)
