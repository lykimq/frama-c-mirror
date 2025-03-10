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

(** Initial abstract state at the beginning of a call. From most precise to
    less precise. *)
type call_init_state =
  | ISCaller (** information from the caller is propagated in the callee. May be
                 more precise, but problematic w.r.t Memexec because it increases
                 cache miss dramatically. *)
  | ISFormals (** empty state, except for the equalities between a formal and
                  the corresponding actual. Lesser impact on Memexec. *)
  | ISEmpty (** completely empty state, without impact on Memexec. *)


type t
val key: t Abstract_domain.key
val project: t -> Equality.Set.t

module type Context = Abstract.Context.External
module type Value = Abstract.Value.External

module Make (Context : Context) (Value : Value with type context = Context.t) :
  Abstract_domain.S with type context = Context.t
                     and type value = Value.t
                     and type location = Precise_locs.precise_location
                     and type state = t

val registered : Abstractions.Domain.registered
