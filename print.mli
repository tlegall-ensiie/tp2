(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

open Ast

let pp_variable = Format.pp_print_string

let rec pp_arith_expr fmt = function
  | Cons z -> Z.pp_print fmt z
  | Var x -> pp_variable fmt x
  | Add(e1,e2) ->
    Format.fprintf fmt "%a + %a" pp_arith_expr e1 pp_arith_expr e2
  | Mul(e1,e2) ->
    Format.fprintf fmt "%a * %a" pp_mul_expr e1 pp_mul_expr e2
  | Div (e1, e2) ->
    Format.fprintf fmt "%a / %a" pp_div_expr e1 pp_arith_expr e2
  | Opp e -> Format.fprintf fmt "-(%a)" pp_arith_expr e

and pp_mul_expr fmt e =
  match e with
  | Cons _ | Var _ | Mul _ | Opp _ -> pp_arith_expr fmt e
  | Add _ | Div _ -> Format.fprintf fmt "(%a)" pp_arith_expr e

and pp_div_expr fmt e =
  match e with
  | Cons _ | Var _ | Div _ | Opp _ -> pp_arith_expr fmt e
  | Add _ | Mul _ -> Format.fprintf fmt "(%a)" pp_arith_expr e


let pp_cmp_op fmt = function
  | Eq -> Format.pp_print_string fmt "="
  | Lt -> Format.pp_print_string fmt "<"
  | Gt -> Format.pp_print_string fmt ">"
  | Ne -> Format.pp_print_string fmt "<>"
  | Le -> Format.pp_print_string fmt "<="
  | Ge -> Format.pp_print_string fmt ">="

let rec pp_boolean_expr fmt = function
  | BCons b -> Format.pp_print_bool fmt b
  | Comp (op, e1, e2) ->
    Format.fprintf fmt "%a %a %a" pp_arith_expr e1 pp_cmp_op op pp_arith_expr e2
  | And (b1,b2) ->
    Format.fprintf fmt "(%a) && (%a)" pp_boolean_expr b1 pp_boolean_expr b2
  | Or (b1,b2) ->
    Format.fprintf fmt "(%a) || (%a)" pp_boolean_expr b1 pp_boolean_expr b2
  | Not b -> Format.fprintf fmt "!(%a)" pp_boolean_expr b

let rec pp_instruction fmt = function
  | Skip -> Format.pp_print_string fmt "skip"
  | Set (x,a) -> Format.fprintf fmt "%a := %a" pp_variable x pp_arith_expr a
  | Seq (i1, i2) ->
    Format.fprintf fmt "%a;@;%a" pp_instruction i1 pp_instruction i2
  | If (b, t, e) ->
    Format.fprintf fmt "@[<v 0>@[<v 2>if %a then@;%a@]@;else@[<v 2>%a@]@;fi@]"
      pp_boolean_expr b pp_instruction t pp_instruction e
  | Loop(c, b) ->
    Format.fprintf fmt "@[<v 0>@[<v 2>while %a do@;%a@]@;done@]"
      pp_boolean_expr c pp_instruction b
