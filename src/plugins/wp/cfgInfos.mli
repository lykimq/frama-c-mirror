(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* Compute Kernel-Function & CFG Infos for WP *)

type t

module Cfg = Interpreted_automata

(** Memoized *)
val get : Kernel_function.t ->
  ?smoking:bool -> ?bhv:string list -> ?prop:string list ->
  unit -> t
val clear : unit -> unit

val body : t -> Cfg.automaton option
val annots : t -> bool
val doomed : t -> WpPropId.prop_id Bag.t
val calls : t -> Kernel_function.Set.t
val smoking : t -> Cil_types.stmt -> bool
val unreachable : t -> Cfg.vertex -> bool
val terminates_deps : t -> Property.Set.t

val is_entry_point : Kernel_function.t -> bool
(** @return true iff the given argument should always be considered as the main
            entry point, in particular: lib-entry is inactive.
    @since 28.0-Nickel
*)

val is_recursive : Kernel_function.t -> bool
val in_cluster : caller:Kernel_function.t -> Kernel_function.t -> bool

val trivial_terminates : int ref

(**************************************************************************)
