(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

open Ast

module StringSet: Set.S with type elt = string

(** the label of the edges in the cfg *)
type transition =
  | Tskip   (** Nop *)
  | Tset of variable * arith_expr
  (** [Tset(x,e)] assigns the value of [e] to [x] *)

  | Ttest of boolean_expr
  (** the edge is taken iff the expression evaluates to [true] *)

(** string representation of an edge label. *)
val string_of_transition: transition -> string

(** Cfg. Nodes are simply labelled with integers. *)
module Cfg: Graph.Sig.P with type V.label = int and type E.label = transition

(** output the cfg in dot format on the given channel. *)
val output_graph: out_channel -> Cfg.t -> unit

(** graph with initial and final nodes. *)
type cfg = { init_node: int; last_node: int; graph: Cfg.t }

(** computes the cfg of a program. *)
val graph_of_program: instruction -> cfg

(** computes the sets of arithmetic variables
    that appear in the program *)
val vars_of_program: instruction -> StringSet.t

(** Argument to the Dataflow Analysis functor below. *)
module type Input =
  sig
    (** must provide:
        - the type t of the data that are propagated on the graph
        - the join operator to merge data coming from several nodes
        - the equal operator (equality over type t)
        - the analyze operator, i.e. the transfer function over a given edge
        - the widening operator (can be equal to join if the analysis is
        guaranteed to terminate without widening)
     *)
    include Graph.ChaoticIteration.Data with type edge = Cfg.edge

    (** initial value associated to all the nodes. *)
    val bot: t

    (** [true] for a forward analysis, [false] for a backward analysis. *)
    val is_forward: bool
  end

(** Dataflow analysis functor. *)
module Analysis(I: Input):
sig
  module M: Map.S with type key = int

  (** [analyze prog init] performs a dataflow analysis over [prog], starting
      with state [init] for the initial (final for a backward analysis) node.
      It returns the cfg and a map associating to each node its computed state.
   *)
  val analyze: instruction -> I.t -> Cfg.t * I.t M.t
end
