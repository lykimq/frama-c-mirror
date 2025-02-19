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

(** Equality used in the goal, simpler to prove than polymorphic equality *)
val add_specific_equality:
  for_tau:(Lang.tau -> bool) ->
  mk_new_eq:Lang.F.binop ->
  unit

(** Return NoResult if it is already proved by Qed *)
val prove :
  ?mode:VCS.mode ->
  ?timeout:float ->
  ?steplimit:int ->
  ?memlimit:int ->
  prover:Why3Provers.t -> Wpo.t -> VCS.result Task.task

(**************************************************************************)
