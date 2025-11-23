(* This file is distributed under the MIT license.
   See file LICENSE for more details.
 *)

(* MUTUALLY RECURSIVE DEFINITIONS:

   An arithmetic expression [e] is *safe* if all
   variables that appears in [e] are safely assigned

   A variable [v] is *safely assigned* if the last
   time its was assigned by [v:=e], [e] was safe

 *)


(* we do a MAY analysis *)

open Ast

module StringSet = Set.Make(String)


(* [s] is assumed to be the set of all variables that are assumed to be safely assigned *)
let rec is_a_safe_expression (s:StringSet.t) (e:Ast.arith_expr) =
failwith "writeme" [@@ ocaml.warning "-27"]



let show_node node vars =
  Printf.printf "S%d -> { " node;
  StringSet.iter (fun s -> Printf.printf "%s " s) vars;
  Printf.printf "}\n%!"

let graph_file = ref "cfg.dot"
let input_file = ref ""

let usage = "Usage: safe_def [-o cfg.dot] input_file"

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


module Sure_init =
  struct
    type t = StringSet.t
    type edge = Stan.Cfg.edge
    let join = failwith "writeme" [@@ ocaml.warning "-27"]
    let equal = StringSet.equal
    let analyze e vars =
      (* [vars] the set of all variables assumed to be safely assigned *)
      match (Stan.Cfg.E.label e) with
      | _ -> failwith "writeme" [@@ ocaml.warning "-27"]
 
    let widening = join
    let bot = failwith "writeme" [@@ ocaml.warning "-27"]
    let is_forward = failwith "writeme" [@@ ocaml.warning "-27"]
  end

module Analyzer = Stan.Analysis(Sure_init)

let graph, res = Analyzer.analyze prog StringSet.empty

let () =
  let outchan = open_out !graph_file in
  Stan.output_graph outchan graph;
  Analyzer.M.iter show_node res
