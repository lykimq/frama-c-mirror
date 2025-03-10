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

(** Build a dynamic printer that bind all pretty-printers to the
    object obtained by (P()) *)

module Make_pp
    (_: sig val printer: unit -> Printer_api.extensible_printer_type end):
  Printer_api.S_pp

(** Build a full pretty-printer from a pretty-printing class.
    @since Fluorine-20130401 *)

module Make
    (_: sig class printer: unit -> Printer_api.extensible_printer_type end):
  Printer_api.S

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
