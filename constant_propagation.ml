(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

open Ast

module Env = Map.Make(String)

type value = Z of Z.t | Unknown

type env = value Env.t

let empty_env = Env.empty

let pretty_opt out =
  function
  | Unknown -> Printf.fprintf out "?"
  | Z z -> output_string out (Z.to_string z)

let pretty_var out name v = Printf.fprintf out "%s = %a\n" name pretty_opt v

let pretty_env out env = Env.iter (pretty_var out) env

let join_value _ v1 v2 = failwith "writeme" [@@ ocaml.warning "-27"]

let join_env env1 env2 = Env.union join_value env1 env2

let included_env env1 env2 =
  let included_val v1 v2 =
    match v1, v2 with
      | _, Unknown -> true
      | Z z1, Z z2 -> Z.equal z1 z2
      | Unknown, _ -> false
  in
  let included_var x v1 =
    match Env.find_opt x env2 with
      | Some v2 -> included_val v1 v2
      | None -> false
  in
  Env.for_all included_var env1

let equal_env env1 env2 = included_env env1 env2 && included_env env2 env1

let find_val env x =
  match Env.find_opt x env with
    | None | Some Unknown -> None
    | Some v -> Some v

let safe_div z1 z2 =
  if Z.equal Z.zero z2 then None else Some (Z.div z1 z2)

let rec eval_arith env e = failwith "writeme" [@@ ocaml.warning "-27"]
and eval_binop env op e1 e2 =
  match eval_arith env e1, eval_arith env e2 with
    | Some z1, Some z2 -> op z1 z2
    | None, _ | _, None -> None

let cmp op z1 z2 =
  match op with
    | Eq -> Z.equal z1 z2
    | Lt -> Z.lt z1 z2
    | Le -> Z.leq z1 z2
    | Ge -> Z.geq z1 z2
    | Gt -> Z.gt z1 z2
    | Ne -> not (Z.equal z1 z2)

let rec eval_bool env e = failwith "writeme" [@@ ocaml.warning "-27-39"]

let eval_expr env e = failwith "writeme" [@@ ocaml.warning "-27"]

let update_var x e env =
  match eval_expr env e with
    | None -> Env.add x Unknown env
    | Some v -> Env.add x v env

module Constant_propagation =
  struct
    type t = env
    type edge = Stan.Cfg.edge
    let join = join_env
    let equal = equal_env
    let analyze e env =
      if Env.is_empty env then empty_env
      else begin
          match (Stan.Cfg.E.label e) with
            | Stan.Tskip -> env
            | Stan.Ttest b ->
                (match eval_bool env b with
                   | None | Some true -> env
                   | Some false -> empty_env)
            | Stan.Tset (x, e) -> update_var x e env
        end

    let widening = join
    let bot = empty_env
    let is_forward = true
  end

module Analyzer = Stan.Analysis(Constant_propagation)

let show_node node env =
  Printf.printf "S%d -> { " node;
  pretty_env stdout env;
  Printf.printf "}\n%!"

let graph_file = ref "cfg.dot"
let input_file = ref ""

let usage = "Usage: constant_propagation [-o cfg.dot] input_file"

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

let vars = Stan.vars_of_program prog

let env =
  Stan.StringSet.fold (fun v acc -> Env.add v Unknown acc) vars Env.empty

let graph, res = Analyzer.analyze prog env

let () =
  let outchan = open_out !graph_file in
  Stan.output_graph outchan graph;
  Analyzer.M.iter show_node res
