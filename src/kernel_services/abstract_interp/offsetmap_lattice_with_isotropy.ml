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

(** Type of the arguments of functor {!Offsetmap.Make} *)

open Lattice_type

module type S = sig

  include Join_Semi_Lattice
  include With_Cardinal_One with type t := t

  (** Hints for the widening. *)
  type widen_hint

  (** [widen ~size ~hint v1 v2] is an over-approximation of [join v1 v2].
      [size] is the size (in bits) of the widened value, and [hint] is some
      hint for the widening. *)
  val widen : ?size:Integer.t -> ?hint:widen_hint -> t -> t -> t

  val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter


  (** {1 Isotropy} *)

  (** Are the bits independent? *)
  val is_isotropic : t -> bool

  val topify_with_origin : Origin.t -> t -> t
  (** Force a value to be isotropic, when a loss of imprecision occurs.
      The resulting value must verify {!is_isotropic}. *)


  (** {1 Reading bits of values} *)

  val extract_bits :
    topify:Origin.kind ->
    start:Integer.t -> stop:Integer.t -> size:Integer.t ->
    t ->
    bool * t
  (** Extract the bits between [start] and [stop] in the value of type [t],
      assuming this value has [size] bits. Return the corresponding value, and
      a boolean indicating that an imprecision occurred during the operation.
      In the latter case, the origin of the imprecision is flagged as having kind
      [topify]. *)

  val shift_bits:
    topify:Origin.kind ->
    offset:Integer.t ->
    size:Integer.t ->
    t -> t
  (** Left-shift the given value, of size [size], by [offset] bits.
      [topify] indicates which operation caused this shift to take place,
      for imprecision tracking. *)

  val merge_distinct_bits:
    topify:Origin.kind ->
    conflate_bottom:bool ->
    t -> t -> t
  (** Merge the bits of the two given values, that span disjoint bit ranges
      by construction. (So either an abstraction of [+] or [|] are correct
      implementations.)

      The [conflate_bottom] argument deals with {!bottom}
      values in either of the arguments. If [conflate_bottom] holds, any
      pre-existing {!bottom} value must result in {!bottom}. Otherwise,
      the {!bottom} value is ignored.

      [topify] indicates which operation caused this merge to take place,
      for imprecision tracking.
  *)

  val merge_neutral_element: t
  (** Value that can be passed to {!merge_distinct_bits} as the starting value.
      This value must be neutral wrt. merging of values. *)

  val anisotropic_cast : size:Integer.t -> t -> t
  (** Optionnally change the representation of the given value, under the
      assumption that it fits in [size] bits. Returning the value argument
      is alwas correct. *)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
