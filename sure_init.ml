(* This file is distributed under the MIT license.
   See file LICENSE for more details.
 *)

(* MUTUALLY RECURSIVE DEFINITIONS:

   An arithmetic expression [e] is *safe* if all
   variables that appears in [e] are surely assigned

   A variable [v] is *surely assigned* if the last
   time its was assigned by [v:=e], [e] was safe

 *)

open Ast

module StringSet = Set.Make(String)

(* functions to compute the set of variables appearing in an arithmetic
   or boolean expression, and in an instruction *)
let rec appears_in_arith = function
  | Cons _ -> StringSet.empty
  | Var v -> StringSet.singleton v
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) ->
    StringSet.union (appears_in_arith e1) (appears_in_arith e2)
  | Opp e -> appears_in_arith e

let rec appears_in_bool = function
  | BCons _ -> StringSet.empty
  | Comp(_, e1, e2) ->
    StringSet.union (appears_in_arith e1) (appears_in_arith e2)
  | And (e1,e2) | Or(e1, e2) ->
    StringSet.union (appears_in_bool e1) (appears_in_bool e2)
  | Not e -> appears_in_bool e

let rec appears_in_instr = function
  | Skip -> StringSet.empty
  | Set(v,e) -> StringSet.add v (appears_in_arith e)
  | Seq(i1,i2) -> StringSet.union (appears_in_instr i1) (appears_in_instr i2)
  | If(b,i1,i2) -> StringSet.union (appears_in_bool b) (StringSet.union (appears_in_instr i1) (appears_in_instr i2))
  | Loop(b,i) -> StringSet.union (appears_in_bool b) (appears_in_instr i)


(* [s] is assumed to be the set of all variables that are NOT surely assigned *)
let rec is_a_safe_expression (s:StringSet.t) = function
  | Cons _ -> true
  | Var v -> not (StringSet.mem v s)
  | Add(e1,e2) | Mul(e1,e2) | Div(e1,e2) ->
    (is_a_safe_expression s e1) && (is_a_safe_expression s e2)
  | Opp e -> is_a_safe_expression s e


let show_node node vars =
  Printf.printf "S%d -> { " node;
  StringSet.iter (fun s -> Printf.printf "%s " s) vars;
  Printf.printf "}\n%!"

let graph_file = ref "cfg.dot"
let input_file = ref ""

let usage = "Usage: sure_init [-o cfg.dot] input_file"

let () =
  Arg.parse
    [ "-o", Arg.Set_string graph_file, "dot file output (default cfg.dot)" ]
    (fun s -> input_file := s)
    usage

let prog =
  if !input_file = "" then begin
    print_endline usage;
    exit 1
  end;
  let inchan = open_in !input_file in
  let res = Parser.program Lexer.main (Lexing.from_channel inchan) in
  close_in inchan;
  res

(* set that contains all the variables of the analyzed program *)
let top_set : StringSet.t = 
  appears_in_instr prog

module Sure_init =
  struct
    type t = StringSet.t
    type edge = Stan.Cfg.edge
    let join = StringSet.inter
    let equal = StringSet.equal
    let analyze e vars =
      (* [vars] is assumed to be the set of all variables that are NOT surely assigned *)
      match (Stan.Cfg.E.label e) with
      | Stan.Tskip -> vars
      | Stan.Ttest b -> vars
      | Stan.Tset (x, e) ->
         if is_a_safe_expression vars e
         then
           StringSet.remove x vars
         else
           StringSet.add x vars
 
    let widening = StringSet.inter
    let bot = top_set
    let is_forward = true
  end

module Analyzer = Stan.Analysis(Sure_init)

let graph, res = Analyzer.analyze prog top_set

let () =
  let outchan = open_out !graph_file in
  Stan.output_graph outchan graph;
  Analyzer.M.iter (fun n v -> show_node n (StringSet.diff top_set v)) res
