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

open Cil_types

(** Utilities function for aggregate types. *)

val get_array_typ_opt:
  typ -> (typ * exp option * attributes) option
(** @return the content of the array type if [ty] is an array, or None
    otherwise. *)

(** Represent the different types of aggregations. *)
type t =
  | StructOrUnion
  | Array
  | NotAggregate

val get_t: typ -> t
(** [get_t ty] returns [Array] if [ty] is an array type,
    [StructOrUnion] if [ty] is a struct or an union type and [NotAggregate]
    otherwise. *)
