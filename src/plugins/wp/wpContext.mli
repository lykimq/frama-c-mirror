(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Model Registration *)

type model
type scope = Global | Kf of Kernel_function.t
type rollback = unit -> unit
type hypotheses = MemoryContext.partition -> MemoryContext.partition

val register :
  id:string ->
  ?descr:string ->
  configure:(unit -> rollback) ->
  ?hypotheses:hypotheses ->
  unit -> model
(** Model registration. The model is identified by [id] and described by
    [descr] (that defaults to [id]). The [configure] function is called on
    [WpContext.on_context] call, it must prepare and set the different
    [Context.values] related to the model. It must return the function that
    allows to rollback on the original state. The [hypotheses] function must
    return the hypotheses made by the model.
*)

val get_descr : model -> string
val get_emitter : model -> Emitter.t

val compute_hypotheses : model -> Kernel_function.t -> MemoryContext.partition

type context = model * scope
type t = context

module S :
sig
  type t = context
  val id : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module MODEL :
sig
  type t = model
  val id : t -> string
  val descr : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val repr : t
end

module SCOPE :
sig
  type t = scope
  val id : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module MINDEX : Hashtbl.S with type key = model

val is_defined : unit -> bool
val on_context : context -> ('a -> 'b) -> 'a -> 'b
val get_model : unit -> model
val get_ident : unit -> string
val get_scope : unit -> scope
val get_context : unit -> context

val directory : unit -> Datatype.Filepath.t
(** Current model in ["-wp-out"] directory *)

module type Entries =
sig
  type key
  type data
  val name : string
  val compare : key -> key -> int
  val pretty : Format.formatter -> key -> unit
end

module type Registry =
sig

  module E : Entries
  type key = E.key
  type data = E.data

  val id : basename:string -> key -> string
  val mem : key -> bool
  val get : key -> data option
  val find : key -> data
  val clear : unit -> unit
  val remove : key -> unit
  val define : key -> data -> unit
  (** no redefinition unless forced ; circularity protected *)

  val update : key -> data -> unit
  (** set current value, with no protection *)

  val memoize : (key -> data) -> key -> data
  (** with circularity protection *)

  val compile : (key -> data) -> key -> unit
  (** with circularity protection *)

  val callback : (key -> data -> unit) -> unit

  val iter : (key -> data -> unit) -> unit
  val iter_sorted : (key -> data -> unit) -> unit
end

module Index(E : Entries) : Registry with module E = E
(** projectified, depend on the model, not serialized *)

module Static(E : Entries) : Registry with module E = E
(** projectified, independent from the model, not serialized *)

module type Key =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module type Data =
sig
  type key
  type data
  val name : string
  val compile : key -> data
end

module type IData =
sig
  type key
  type data
  val name : string
  val basename : key -> string
  val compile : key -> string -> data
end

module type Generator =
sig
  type key
  type data
  val mem : key -> bool
  val get : key -> data
  val set : key -> data -> unit
  val find : key -> data
  val remove : key -> unit
  val clear : unit -> unit
end

(** projectified, depend on the model, not serialized *)
module Generator(K : Key)(D : Data with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, independent from the model, not serialized *)
module StaticGenerator(K : Key)(D : Data with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, depend on the model, not serialized *)
module GeneratorID(K : Key)(D : IData with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, independent from the model, not serialized *)
module StaticGeneratorID(K : Key)(D : IData with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data
