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

open Cil_types

(** Builds a [Instantiator] module (used by [Transform]) from a [Generator_sig] *)

(** Signature for a new instantiator generator.

    In order to support a new function, this module must be implemented and
    registered to the [Transform] module.
*)
module type Generator_sig = sig
  (** [Hashtbl] module used by the [Make_instantiator] module to generate the
      function cache. The [key] ([override_key]) must identify a function
      override.
  *)
  module Hashtbl: Datatype.Hashtbl
  type override_key = Hashtbl.key

  (** Name of the implemented function *)
  val function_name: string

  (** [well_typed_call ret fct args] must return true if and only if the
      corresponding call is well typed in the sens that the generator can
      produce a function override for the corresponding return value and
      parameters, according to their types and/or values.
  *)
  val well_typed_call: lval option -> varinfo -> exp list -> bool

  (** [key_from_call ret fct args] must return an identifier for the
      corresponding call. [key_from_call ret1 fct1 args1] and
      [key_from_call ret2 fct2 args2] must equal if and only if the same
      override function can be used for both call. Any call for which
      [well_typed_call] returns true must be able to give a key.
  *)
  val key_from_call: lval option -> varinfo -> exp list -> override_key

  (** [retype_args key args] must returns a new argument list compatible with
      the function identified by [override_key]. [args] is the list of arguments
      that were given to the call for with [Transform] is currently replacing.
      Most of the time, it will consists in removing/changing some casts. But
      note that arguments can be removed (for example if the override is built
      because some constant have specialized).
  *)
  val retype_args: override_key -> exp list -> exp list

  (** [args_for_original key args] must transform the list of parameters of the
      override function such that the new list can be given to the original
      function. For example, if for a call:
      [foo(x, 0, z)]
      The [Instantiator] module generates an override:
      [void foo_spec(int p1, int p3);]
      The received list of expression is [ p1 ; p3 ] and thus the returned list
      should be [ p1 ; 0 ; p3 ] (so that the replaced call [foo_spec(x, z)] has
      the same behavior).
      Note that there is no need to introduce casts to the original type, it is
      introduced by [Make_instantiator].
  *)
  val args_for_original: override_key -> exp list -> exp list

  (** [generate_prototype key] must generate the name and the type corresponding
      to [key].
  *)
  val generate_prototype: override_key -> (string * typ)

  (** [generate_spec key loc fundec] must generate the specification of the
      [fundec] generated from [key]. The location is the one generated by the
      [Transform] visitor. Note that it must return a [funspec] but should
      {b not} register it in [Annotations] tables.
  *)
  val generate_spec: override_key -> location -> fundec -> funspec
end

(** Signature of a instantiator.

    Module built by the [Transform] module from a [Generator_sig].
    Used to perform the different replacements in the AST. This
    should be only used by the [Transform] module.
*)
module type Instantiator = sig
  (** Plugin option that allows to check whether the instantiator is enabled. *)
  module Enabled: Parameter_sig.Bool

  (** Same as [Generator_sig.override_key] *)
  type override_key

  (** Same as [Generator_sig.override_key] *)
  val function_name: string

  (** Same as [Generator_sig.override_key] *)
  val well_typed_call: lval option -> varinfo -> exp list -> bool

  (** Same as [Generator_sig.override_key] *)
  val key_from_call: lval option -> varinfo -> exp list -> override_key

  (** Same as [Generator_sig.override_key] *)
  val retype_args: override_key -> exp list -> exp list

  (** [get_override key] returns the function generated for [key].
      This value is cached so that if it does not exist, it is created using the
      different [Generator_sig] generation functions, else the cached version is
      returned.
  *)
  val get_override: override_key -> fundec

  (** [get_kfs ()] returns all generated kernel functions for the current
      project.
  *)
  val get_kfs: unit -> kernel_function list

  (** [clear ()] clears the internal table of the instantiator.
  *)
  val clear: unit -> unit
end

(** Generates a [Instantiator] from a [Generator_sig] adding all necessary stuff for
    cache and function definition generation, as well as specification
    registration. This should only be used by [Transform].
*)
module Make_instantiator (_: Generator_sig) : Instantiator
