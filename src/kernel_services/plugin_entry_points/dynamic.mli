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

(** Value accesses through dynamic typing.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(** {2 Registration} *)
(* ************************************************************************* *)

val register:
  ?comment:string ->
  plugin:string ->
  string -> 'a Type.t -> 'a -> 'a
(** [register ~plugin name ty v] registers [v] with the name
    [name], the type [ty] and the plug-in [plugin].
    @raise Type.AlreadyExists if [name] already exists. In other words you
    cannot register a value with the same name twice.
    @before 26.0-Iron there was a labeled argument [journalized], that has
            been removed when Journalization has been removed.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(** {2 Access} *)
(* ************************************************************************* *)

exception Incompatible_type of string
exception Unbound_value of string

val get: plugin:string -> string -> 'a Type.t -> 'a
(** [get ~plugin name ty] returns the value registered with the name
    [name], the type [ty] and the plug-in [plugin]. This plug-in will be
    loaded if required.
    @raise Unbound_value if the name is not registered
    @raise Incompatible_type if the name is not registered
    with a compatible type
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val iter: (string -> 'a Type.t -> 'a -> unit) -> unit
val iter_comment : (string -> string -> unit) -> unit
(** @since Oxygen-20120901 *)

(* ************************************************************************* *)
(** {2 Dedicated access to plug-in parameters} *)
(* ************************************************************************* *)

(** Module to use for accessing parameters of plug-ins.
    Assume that the plug-in is already loaded.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Parameter : sig

  (** Set of common operations on parameters. *)
  module type Common = sig
    type t
    val get: string -> unit -> t
    val set: string -> t -> unit
    val clear: string -> unit -> unit
    val is_set: string -> unit -> bool
    val is_default: string -> unit -> bool
  end

  (** retrieve the representation of the corresponding parameter. *)
  val get_parameter: string -> Typed_parameter.t

  (** retrieve the state related to the corresponding parameter.
      @raise Not_found if the option does not correspond to an actual
      parameter
      @since Oxygen-20120901 *)
  val get_state: string -> State.t

  (**/**)
  val get_name: string -> string -> string -> string
  (** Not for casual users *)
  (**/**)

  (** Boolean parameters.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
  module Bool: sig
    include Common with type t = bool
    val on: string -> unit -> unit
    (** Set the parameter to [true]. *)

    val off : string -> unit -> unit
    (** Set the parameter to [false]. *)
  end

  (** Integer parameters. *)
  module Int : sig
    include Common with type t = int
    val incr : string -> unit -> unit
  end

  (** String parameters. *)
  module String : Common with type t = string

  (** Filepath parameters. *)
  module Filepath : Common with type t = Datatype.Filepath.t

  (** Set of string parameters. *)
  module StringSet : sig
    include Common with type t = Datatype.String.Set.t
    val add: string -> string  -> unit
    val remove: string -> string -> unit
    val is_empty: string -> unit -> bool
    val iter: string -> (string -> unit) -> unit
  end

  (** List of string parameters. *)
  module StringList : sig
    include Common with type t = string list
    val add: string -> string  -> unit
    val append_before: string -> string list -> unit
    (** @since Neon-20140301 *)

    val append_after: string -> string list -> unit
    (** @since Neon-20140301 *)

    val remove: string -> string -> unit
    val is_empty: string -> unit -> bool
    val iter: string -> (string -> unit) -> unit
  end

end

(* ************************************************************************* *)
(** {2 Dynamically Loaded Modules} *)
(* ************************************************************************* *)

(** loads a list of Findlib packages
    @since 23.0-Vanadium
*)
val load_packages: string list -> unit

(** Load the module specification. See -load-module option. *)
val load_module: string -> unit

val load_plugin: string -> unit

(**/**)
val load_plugin_path: unit -> unit
(** Load all plugins in the path set with [set_module_load_path].
    Must be invoked only once from boot during extending stage.
    @since Magnesium-20151001 new API. *)
(**/**)

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)
