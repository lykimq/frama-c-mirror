(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(*                                                                          *)
(****************************************************************************)

(** Smart constructors for some CIL data types *)
open Cil_types

(** Create a typ record, [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_typ : ?tattr:attributes -> typ_node -> typ

(** Create a typ record [TVoid], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tvoid : ?tattr:attributes -> unit  -> typ

(** Create a typ record [TInt ik], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tint : ?tattr:attributes -> ikind -> typ

(** Create a typ record [TFloat fk], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tfloat : ?tattr:attributes -> fkind -> typ

(** Create a typ record [TPtr t], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tptr : ?tattr:attributes -> typ   -> typ

(** Create a typ record [TArray (t, len)], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tarray : ?tattr:attributes -> typ   -> exp option -> typ

(** Create a typ record [TFun (rt, args, is_va)], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tfun : ?tattr:attributes -> typ   ->
  (string * typ * attributes) list option -> bool -> typ

(** Create a typ record [TNamed ti], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tnamed : ?tattr:attributes -> typeinfo  -> typ

(** Create a typ record [TComp ci], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tcomp : ?tattr:attributes -> compinfo  -> typ

(** Create a typ record [TEnum ei], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tenum : ?tattr:attributes -> enuminfo  -> typ

(** Create a typ record [TBuiltin_va_list], [tattr] defaults to empty list.
    @since Frama-C+dev
*)
val mk_tbuiltin : ?tattr:attributes -> unit      -> typ

(** void *)
val voidType: typ

(** bool
    @since 30.0-Zinc *)
val boolType: typ

(** int
    @since 30.0-Zinc *)
val intType: typ

(** unsigned
    @since 30.0-Zinc *)
val uintType: typ

(** short
    @since 30.0-Zinc *)
val shortType : typ

(** unsigned short
    @since 30.0-Zinc *)
val ushortType : typ

(** long
    @since 30.0-Zinc *)
val longType: typ

(** long long
    @since 30.0-Zinc *)
val longLongType: typ

(** unsigned long
    @since 30.0-Zinc *)
val ulongType: typ

(** unsigned long long
    @since 30.0-Zinc *)
val ulongLongType: typ

(** char
    @since 30.0-Zinc *)
val charType: typ

(** signed char
    @since 30.0-Zinc *)
val scharType: typ

(** unsigned char
    @since 30.0-Zinc *)
val ucharType: typ

(** char *
    @since 30.0-Zinc *)
val charPtrType: typ

(** signed char *
    @since 30.0-Zinc *)
val scharPtrType: typ

(** unisgned char *
    @since 30.0-Zinc *)
val ucharPtrType: typ

(** char const *
    @since 30.0-Zinc *)
val charConstPtrType: typ

(** void *
    @since 30.0-Zinc *)
val voidPtrType: typ

(** void const *
    @since 30.0-Zinc *)
val voidConstPtrType: typ

(** int *
    @since 30.0-Zinc *)
val intPtrType: typ

(** unsigned int *
    @since 30.0-Zinc *)
val uintPtrType: typ

(** float
    @since 30.0-Zinc *)
val floatType: typ

(** double
    @since 30.0-Zinc *)
val doubleType: typ

(** long double
    @since 30.0-Zinc *)
val longDoubleType: typ

module Vid: sig val next: unit -> int end
module Sid: sig val next: unit -> int end
module Eid: sig val next: unit -> int end

(** set the vid to a fresh number. *)
val set_vid: varinfo -> unit

(** returns a copy of the varinfo with a fresh vid.
    If the varinfo has an associated logic var, a copy of the logic var
    is made as well.
*)
val copy_with_new_vid: varinfo -> varinfo

(** [change_varinfo_name vi name] changes the name of [vi] to [name]. Takes
    care of renaming the associated logic_var if any.
    @since Oxygen-20120901
*)
val change_varinfo_name: varinfo -> string -> unit

val new_raw_id: unit -> int
(** Generate a new ID. This will be different than any variable ID
    that is generated by {!Cil.makeLocalVar} and friends.
    Must not be used for setting vid: use {!set_vid} instead. *)

(** Creates a (potentially recursive) composite type. The arguments are:
    (1) a boolean indicating whether it is a struct or a union, (2) the name
    (always non-empty), (3) a function that when given a representation of the
    structure type constructs the type of the fields recursive type (the first
    argument is only useful when some fields need to refer to the type of the
    structure itself), and (4) an optional list of attributes to be associated
    with the composite type, "None" means that the struct is incomplete.
    @since 23.0-Vanadium the 4th parameter is a function that returns an option.
*)
val mkCompInfo: bool ->      (* whether it is a struct or a union *)
  string -> (* name of the composite type; cannot be empty *)
  ?norig:string -> (* original name of the composite type, empty when anonymous *)
  (compinfo ->
   (string * typ * int option * attributes * location) list option) ->
  (* a function that when given a forward
     representation of the structure type constructs the type of
     the fields. The function can ignore this argument if not
     constructing a recursive type.  *)
  attributes -> compinfo

(** Makes a shallow copy of a {!Cil_types.compinfo} changing the name. It also
    copies the fields, and makes sure that the copied field points back to the
    copied compinfo.
    If [fresh] is [true] (the default), it will also give a fresh id to the
    copy.
*)
val copyCompInfo: ?fresh:bool -> compinfo -> string -> compinfo


(** Create a fresh logical variable giving its name, type and origin.
    @since Fluorine-20130401
*)
val make_logic_var_kind : string -> logic_var_kind -> logic_type -> logic_var

(** Create a new global logic variable
    @since Fluorine-20130401 *)
val make_logic_var_global: string -> logic_type -> logic_var

(** Create a new formal logic variable
    @since Fluorine-20130401 *)
val make_logic_var_formal: string -> logic_type -> logic_var

(** Create a new quantified logic variable
    @since Fluorine-20130401 *)
val make_logic_var_quant: string -> logic_type -> logic_var

(** Create a new local logic variable
    @since Fluorine-20130401 *)
val make_logic_var_local: string -> logic_type -> logic_var

(** Create a fresh logical (global) variable giving its name and type. *)
val make_logic_info : string -> logic_info

(** Create a new local logic variable given its name.
    @since Fluorine-20130401
*)
val make_logic_info_local : string -> logic_info

(** Create a logic type info given its name.
    @since 30.0-Zinc
*)
val make_logic_type : string -> logic_type_info
