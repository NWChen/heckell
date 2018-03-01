(* Semantic checking *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check_binds (kind: string) (to_check: bind list) =
  let check_it checked binding =
      let dup_err = "duplicate " ^ kind ^ " " ^ snd binding (* `snd binding` is the name of the duplicated variable *)
      in match binding with
        (_, n1) -> match checked with
              ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
            | _ -> binding :: checked (* add binding to list of checked bindings, named `checked` *)
  in let sorted = List.sort compare to_check in
    let _ = List.fold_left check_it [] sorted 
      in to_check
in

(* globals time *)
let globals' = 
    check_binds "global" globals
in

(* check type of identifier *)
let type_of_identifier s = 
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
in

(* ensure types in an expr evaluation match *)
let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
in

(* ??? *)
let rec expr = 
      Lit l -> (Int, SLit l)
    | Id s -> (type_of_identifier s, SId s)
    | Assign(var, e) as ex ->
        let lt = type_of_identifier var (* TODO type of identifier? *)
        and (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex in
        check_assign lt rt err, SAssign(var, (rt, e'))
    | 
in 



(*
  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) = 
    let check_it checked binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check) 
       in to_check
    *)
