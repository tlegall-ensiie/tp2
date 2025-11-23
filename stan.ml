(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

open Ast

module StringSet = Set.Make(String)

(** labels on the edges of the Cfg *)
type transition = Tskip | Tset of variable * arith_expr | Ttest of boolean_expr

let string_of_transition = function
  | Tskip -> "skip"
  | Tset(x,e) ->
    Format.asprintf "@[%a := %a@]" Print.pp_variable x Print.pp_arith_expr e
  | Ttest e ->
    Format.asprintf "@[?(%a)@]" Print.pp_boolean_expr e

module Node = struct
  type t = int
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = Stdlib.(=)
end

module Edge = struct
  type t = transition
  let compare = Stdlib.compare
  let default = Tskip
end

module Cfg = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

module Show_cfg =
  Graph.Graphviz.Dot(
    struct
      include Cfg
      let graph_attributes _ = []
      let vertex_name v = "S" ^ string_of_int (Cfg.V.label v)
      let default_vertex_attributes _ = []
      let vertex_attributes _ = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes e =
        [`Label (string_of_transition (Cfg.E.label e))]
    end)

let output_graph = Show_cfg.output_graph

type cfg = { init_node: int; last_node: int; graph: Cfg.t }

let graph_of_program p =
  let rec aux n1 n2 g = function
    | Skip -> (n2, Cfg.add_edge_e g (Cfg.E.create n1 Tskip n2))
    | Set(l,e) ->
      (n2, Cfg.add_edge_e g (Cfg.E.create n1 (Tset (l,e)) n2))
    | Seq (i1, i2) ->
      let n, g = aux n1 n2 g i1 in aux n (n+1) g i2
    | If (c, t, e) ->
      let g = Cfg.add_edge_e g (Cfg.E.create n1 (Ttest c) n2) in
      let nt, g = aux n2 (n2+1) g t in
      let g = Cfg.add_edge_e g (Cfg.E.create n1 (Ttest (Not c)) (nt + 1)) in
      let ne, g = aux (nt + 1) (nt + 2) g e in
      let g = Cfg.add_edge_e g (Cfg.E.create nt Tskip (ne + 1)) in
      let g = Cfg.add_edge_e g (Cfg.E.create ne Tskip (ne + 1)) in
      (ne + 1, g)
    | Loop (c, b) ->
      let g = Cfg.add_edge_e g (Cfg.E.create n1 (Ttest c) n2) in
      let n, g = aux n2 (n2 + 1) g b in
      let g = Cfg.add_edge_e g (Cfg.E.create n Tskip n1) in
      let g = Cfg.add_edge_e g (Cfg.E.create n1 (Ttest (Not c)) (n+1)) in
      (n+1, g)
  in
  let last_node, graph = aux 0 1 Cfg.empty p in
  { init_node = 0; last_node; graph }

let rec vars_of_arith_expr vars = function
  | Cons _ -> vars
  | Var x -> StringSet.add x vars
  | Add (e1,e2) | Mul(e1,e2) | Div (e1, e2) ->
      let vars = vars_of_arith_expr vars e1 in
      let vars = vars_of_arith_expr vars e2 in
      vars
  | Opp e -> vars_of_arith_expr vars e

let rec vars_of_boolean_expr vars = function
  | BCons _ -> vars
  | Comp (_, a1, a2) ->
      let vars = vars_of_arith_expr vars a1 in
      let vars = vars_of_arith_expr vars a2 in
      vars
  | And (b1, b2) | Or(b1, b2) ->
      let vars = vars_of_boolean_expr vars b1 in
      let vars = vars_of_boolean_expr vars b2 in
      vars
  | Not b -> vars_of_boolean_expr vars b

let vars_of_program p =
  let rec aux vars = function
    | Skip -> vars
    | Set (x,e) ->
        let vars = StringSet.add x vars in
        let vars = vars_of_arith_expr vars e in
        vars
    | Seq (i1, i2) ->
        let vars = aux vars i1 in
        let vars = aux vars i2 in
        vars
    | If (c,t,e) ->
        let vars = vars_of_boolean_expr vars c in
        let vars = aux vars t in
        let vars = aux vars e in
        vars
    | Loop (c,b) ->
        let vars = vars_of_boolean_expr vars c in
        let vars = aux vars b in
        vars
  in aux StringSet.empty p

module Wto = Graph.WeakTopological.Make(Cfg)

module type Input =
sig
  include Graph.ChaoticIteration.Data with type edge = Cfg.edge
  val bot: t
  val is_forward: bool
end

module Cfg_oper = Graph.Oper.P(Cfg)

module Analysis (I: Input) =
  struct
    include Graph.ChaoticIteration.Make(Cfg)(I)
    let analyze prog init =
      let cfg = graph_of_program prog in
      let g = if I.is_forward then cfg.graph else Cfg_oper.mirror cfg.graph in
      let start = if I.is_forward then cfg.init_node else cfg.last_node in
      let mk_init v = if v = start then init else I.bot in
      let wto = Wto.recursive_scc g start in
      let res = recurse g wto mk_init Graph.ChaoticIteration.FromWto 0 in
      cfg.graph,res
  end
