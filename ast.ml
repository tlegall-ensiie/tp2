(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

type variable = string

type arith_expr =
  | Cons of Z.t (** numerical constant. *)
  | Var of variable
  | Add of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
  | Div of arith_expr * arith_expr
  | Opp of arith_expr

(** The comparison operators. *)
type cmp_op = Eq | Lt | Gt | Ne | Le | Ge

type boolean_expr =
  | BCons of bool (** boolean constant. *)
  | Comp of cmp_op * arith_expr * arith_expr
  | And of boolean_expr * boolean_expr
  | Or of boolean_expr * boolean_expr
  | Not of boolean_expr

type instruction =
  | Skip (** do nothing. *)
  | Set of variable * arith_expr
  | Seq of instruction * instruction
  | If of boolean_expr * instruction * instruction
  | Loop of boolean_expr * instruction
