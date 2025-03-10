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

(** Stored messages for persistence between sessions. *)

val iter: (Log.event -> unit) -> unit
(** Iter over all stored messages. The messages are passed in emission order. *)

val fold: ('a -> Log.event -> 'a) -> 'a -> 'a
(** Fold over all stored messages. The messages are passed in emission order. *)

val dump_messages: unit -> unit
(** Dump stored messages to standard channels *)

val self: State.t
(** Internal state of stored messages *)

val reset_once_flag : unit -> unit
(** Reset the [once] flag of pretty-printers. Messages already printed
    will be printed again.
    @since Boron-20100401 *)

val nb_errors: unit -> int
val nb_warnings: unit -> int
val nb_messages: unit -> int
(** Number of stored warning messages, error messages, or all
    messages.*)

val add_global_hook: (unit -> unit) -> unit
(** Register a global hook (not projectified) on message addition. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
