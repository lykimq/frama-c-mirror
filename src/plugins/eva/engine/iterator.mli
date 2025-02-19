(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Mark the analysis as aborted: it will be stopped at the next safe point. *)
val signal_abort: kill:bool -> unit

(** Reset the signal sent by [signal_abort], if any. *)
val signal_reset: unit -> unit

(** Provided [stmt] is an 'if' construct, [fst (condition_truth_value stmt)]
    (resp. snd) is true if and only if the condition of the 'if' has been
    evaluated to true (resp. false) at least once during the analysis. *)
val condition_truth_value: stmt -> bool * bool

module Computer
    (* Abstractions with the evaluator. *)
    (Abstract: Abstractions.S_with_evaluation)
    (* Set of states of abstract domain. *)
    (States : Powerset.S with type state = Abstract.Dom.t)
    (* Transfer functions for statement on the abstract domain. *)
    (_ : Transfer_stmt.S with type state = Abstract.Dom.t
                          and type value = Abstract.Val.t)
    (* Initialization of local variables. *)
    (_: Initialization.S with type state := Abstract.Dom.t)
    (* Transfer functions for the logic on the abstract domain. *)
    (_ : Transfer_logic.S with type state = Abstract.Dom.t
                           and type states = States.t)
    (_: sig
       val treat_statement_assigns: assigns -> Abstract.Dom.t -> Abstract.Dom.t
     end)
  : sig

    val compute:
      save_results:bool ->
      kernel_function -> kinstr -> Abstract.Dom.t ->
      (Partition.key * Abstract.Dom.t) list * Eval.cacheable

  end

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
