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

open VCS

(** - [valid]: Play provers with valid result (default: true)
    - [failed]: Play provers with invalid result (default: true)
    - [scratch]: Discard existing script (default: false)
    - [provers]: Additional list of provers to {i try} when stuck
    - [depth]: Strategy search depth (default: 0)
    - [width]: Strategy search width (default: 0)
    - [backtrack]: Strategy backtracking (default: 0)
    - [auto]: Strategies to try (default: none)
*)
type 'a process =
  ?valid:bool ->
  ?failed:bool ->
  ?scratch:bool ->
  ?provers:prover list ->
  ?depth:int ->
  ?width:int ->
  ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?strategies:bool ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> prover -> result -> unit) ->
  ?success:(Wpo.t -> prover option -> unit) ->
  Wpo.t -> 'a

val prove : unit Task.task process
val spawn : unit process

val search :
  ?depth:int ->
  ?width:int ->
  ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?provers:prover list ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> prover -> result -> unit) ->
  ?success:(Wpo.t -> prover option -> unit) ->
  ProofEngine.tree ->
  ProofEngine.node ->
  unit

val explore :
  ?depth:int ->
  ?strategy:ProofStrategy.strategy ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> prover -> result -> unit) ->
  ?success:(Wpo.t -> prover option -> unit) ->
  ProofEngine.tree ->
  ProofEngine.node ->
  unit

val get : Wpo.t -> [ `Script | `Proof | `Saved | `None ]
