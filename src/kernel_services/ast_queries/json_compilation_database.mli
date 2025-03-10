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

(** [get_flags f] returns the preprocessing flags associated to file [f]
    in the JSON compilation database (when enabled), or the empty string
    otherwise. If not empty, the flags always start with a space. *)
val get_flags : Datatype.Filepath.t -> string list

(** [get_dir f] returns the preprocessing directory associated to file [f]
    in the JSON compilation database.
    @since 25.0-Manganese
*)
val get_dir : Datatype.Filepath.t -> Datatype.Filepath.t option

(** [has_entry f] returns true iff [f] has an entry in the JSON compilation
    database. Must only be called if a JCDB file has been specified.
    @since 22.0-Titanium
*)
val has_entry : Datatype.Filepath.t -> bool
