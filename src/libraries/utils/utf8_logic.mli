(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(** UTF-8 string for logic symbols. *)

(** given an unicode code point, returns the corresponding utf-8 encoding. *)
val from_unichar: int -> string

val forall : string
val exists : string
val eq : string
val neq : string
val le : string
val ge : string
val implies : string
val iff : string
val conj : string
val disj : string
val neg : string
val x_or : string
val inset : string
val emptyset : string
val top: string
val bottom:string
val union:string
val minus: string
val boolean: string
val integer: string
val real: string
val pi: string
val infinity: string

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
