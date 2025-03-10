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

exception NoAssigns

val get_assigns_from :
  loc:Cil_types.location ->
  Env.t ->
  Cil_types.logic_var list ->
  Cil_types.logic_var ->
  Cil_types.exp list
(* @returns the list of expressions that are allowed to be used to assign the
   the result of a logic function *)

val get_assigned_var :
  loc:Cil_types.location -> is_gmp:bool -> Cil_types.varinfo -> Cil_types.term
(* @returns the expression that gets assigned when the result of the function is
   passed as an additional argument *)
