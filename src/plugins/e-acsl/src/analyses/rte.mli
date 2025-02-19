(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2025                                               *)
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

(** Accessing the RTE plug-in easily. *)

open Cil_types

val stmt: ?warn:bool -> kernel_function -> stmt -> code_annotation list
(** RTEs of a given stmt, as a list of code annotations. *)

val exp: ?warn:bool -> kernel_function -> stmt -> exp -> code_annotation list
(** RTEs of a given exp, as a list of code annotations. *)

val get_state_selection_with_dependencies: unit -> State_selection.t
(** Equivalent to [State_selection.with_dependencies RteGen.Api.self]
    if the RTE plug-in is enabled, empty otherwise. *)

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
