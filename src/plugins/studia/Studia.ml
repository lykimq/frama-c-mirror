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

(** Computations of the statements that write a given memory zone. *)
include
  (struct
    module Writes = Writes
    module Reads = Reads
  end :
   sig
     module Writes: sig
       type t =
         | Assign of Cil_types.stmt
         (** Direct assignment. *)
         | CallDirect of Cil_types.stmt
         (** Modification by a called leaf function. *)
         | CallIndirect of Cil_types.stmt
         (** Modification inside the body of a called function. *)
         | GlobalInit of Cil_types.varinfo * Cil_types.initinfo
         (** Initialization of a global variable. *)
         | FormalInit of
             Cil_types.varinfo *
             (Cil_types.kernel_function * Cil_types.stmt list) list
         (** Initialization of a formal parameter, with a list of callsites. *)

       val compare: t -> t -> int

       val compute: Locations.Zone.t -> t list
       (** [compute z] finds all the statements that modifies [z], and for each
           statement, indicates whether the modification is direct or indirect. *)

     end

     (** Computations of the statements that read a given memory zone. *)
     module Reads: sig
       type t =
         | Direct of Cil_types.stmt
         (** Direct read by a statement. *)
         | Indirect of Cil_types.stmt
         (** Indirect read through a function call. *)

       val compute: Locations.Zone.t -> t list
       (** [compute z] finds all the statements that read [z]. The [effects]
           information indicates whether the read occur on the given statement,
           or through an inner call for [Call] instructions. *)

     end
   end)

(** {2 Internal use only} *)

module Options = Options
