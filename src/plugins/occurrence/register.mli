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

(** Register the plugin in the Frama-C kernel. *)

open Cil_types

val self: State.t

val get_last_result:
  unit -> ((kernel_function option * kinstr * lval) list * varinfo) option

val get: (varinfo -> (kernel_function option * kinstr * lval) list)
(** Return the occurrences of the given varinfo.
    An occurrence [ki, lv] is a left-value [lv] which uses the location of
    [vi] at the position [ki]. *)

val print_all: (unit -> unit)
(** Print all the occurrence of each variable declarations. *)

type access_type = Read | Write | Both

val classify_accesses: kernel_function option * kinstr *lval -> access_type
