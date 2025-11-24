(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

open Ast

module StringSet = Set.Make(String)

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

module Live_var =
  struct
    type t = StringSet.t
    type edge = Stan.Cfg.edge
    let join = StringSet.union
    let equal = StringSet.equal
    let analyze e vars =
      match (Stan.Cfg.E.label e) with
      | Stan.Tskip -> vars
      | Stan.Ttest b -> StringSet.union vars (appears_in_bool b)
      | Stan.Tset (x, e) ->
        StringSet.union (StringSet.remove x vars) (appears_in_arith e)
    let widening = join
    let bot = StringSet.empty
    let is_forward = false
  end

module Analyzer = Stan.Analysis(Live_var)

let show_node node vars =
  Printf.printf "S%d -> { " node;
  StringSet.iter (fun s -> Printf.printf "%s " s) vars;
  Printf.printf "}\n%!"

let graph_file = ref "cfg.dot"
let input_file = ref ""

let usage = "Usage: live_var [-o cfg.dot] input_file"

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

let graph, res = Analyzer.analyze prog StringSet.empty

let () =
  let outchan = open_out !graph_file in
  Stan.output_graph outchan graph;
  Analyzer.M.iter show_node res
