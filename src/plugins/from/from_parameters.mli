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

include Plugin.S

(** Option -deps *)
module ForceDeps: Parameter_sig.With_output

(** Option -calldeps.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module ForceCallDeps: Parameter_sig.With_output

(** Option -show-indirect-deps *)
module ShowIndirectDeps: Parameter_sig.Bool

(** Option -from-verify-assigns. *)
module VerifyAssigns: Parameter_sig.Bool


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
