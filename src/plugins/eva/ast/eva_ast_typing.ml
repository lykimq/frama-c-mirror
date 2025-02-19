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

open Eva_ast_types

let type_of_const : constant -> typ = function
  | CTopInt ik -> Cil_const.mk_tint ik
  | CInt64 (_, ik, _) -> Cil_const.mk_tint ik
  | CChr _ -> Cil_const.intType
  | CString (String (_, Base.CSString _)) -> Machine.string_literal_type ()
  | CString (String (_, Base.CSWstring _)) -> Cil_const.mk_tptr (Machine.wchar_type ())
  | CString (_) -> assert false (* it must be a String base*)
  | CReal (_, fk, _) -> Cil_const.mk_tfloat fk
  | CEnum (_ei, e) -> e.typ

let rec type_of_offset (basetyp : typ) : offset -> typ = function
  | NoOffset -> basetyp
  | Index (_, o) ->
    type_of_offset (Cil.typeOf_array_elem basetyp) o
  | Field (fi, o) ->
    let base_attrs = (Cil.unrollType basetyp).tattr in
    let base_attrs = Ast_attributes.filter_qualifiers base_attrs in
    let base_attrs =
      if Ast_attributes.(contains frama_c_mutable fi.fattr) then
        Ast_attributes.drop "const" base_attrs
      else
        base_attrs
    in
    type_of_offset (Cil.typeAddAttributes base_attrs fi.ftype) o

let type_of_lhost : lhost -> typ = function
  | Var vi -> vi.vtype
  | Mem addr -> Cil.typeOf_pointed addr.typ

let type_of_lval_node (host, offset : lval_node) : typ =
  let basetyp = type_of_lhost host in
  type_of_offset basetyp offset

let type_of_exp_node : exp_node -> typ = function
  | Const c -> type_of_const c
  | Lval lv -> Cil.type_remove_qualifier_attributes lv.typ
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf lv -> Cil_const.mk_tptr lv.typ
  | StartOf lv ->
    match Cil.unrollType lv.typ with
    | { tnode = TArray (t, _); tattr } -> Cil_const.mk_tptr ~tattr t
    | _ ->  assert false
