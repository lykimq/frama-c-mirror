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

module Rewrite =
struct
  type visitor = {
    exp : exp -> exp;
    lval : lval -> lval;
    varinfo : varinfo -> varinfo;
    offset : offset -> offset;
  }

  type rewriter = {
    rewrite_exp : visitor:visitor -> exp -> exp;
    rewrite_lval : visitor:visitor -> lval -> lval;
    rewrite_varinfo : visitor:visitor -> varinfo -> varinfo;
    rewrite_offset : visitor:visitor -> offset -> offset;
  }

  let bind (r : rewriter) =
    let rec visitor = {
      exp = (fun e -> r.rewrite_exp ~visitor e);
      lval = (fun lv -> r.rewrite_lval ~visitor lv);
      varinfo = (fun vi -> r.rewrite_varinfo ~visitor vi);
      offset = (fun o -> r.rewrite_offset ~visitor o);
    }
    in
    visitor

  let rewrite_exp ~visitor exp =
    let replace_if condition node =
      if condition then Eva_ast_builder.mk_exp node else exp
    in
    match exp.node with
    | Lval lv ->
      let lv' = visitor.lval lv in
      replace_if (lv != lv') (Lval lv')
    | AddrOf lv ->
      let lv' = visitor.lval lv in
      replace_if (lv != lv') (AddrOf lv')
    | StartOf lv ->
      let lv' = visitor.lval lv in
      replace_if (lv != lv') (StartOf lv')
    | UnOp (op, e, t) ->
      let e' = visitor.exp e in
      replace_if (e' != e) (UnOp (op, e', t))
    | BinOp (op, e1, e2, t) ->
      let e1' = visitor.exp e1
      and e2' = visitor.exp e2 in
      replace_if (e1' != e1 || e2' != e2) (BinOp (op, e1', e2', t))
    | CastE (t, e) ->
      let e' = visitor.exp e in
      replace_if (e' != e) (CastE (t, e'))
    | Const _ ->
      exp

  let rewrite_lval ~visitor lval =
    let lhost, offset = lval.node in
    let lhost' =
      match lhost with
      | Var vi ->
        let vi' = visitor.varinfo vi in
        if vi' != vi then Var vi' else lhost
      | Mem e ->
        let e' = visitor.exp e in
        if e' != e then Mem e' else lhost
    and offset' = visitor.offset offset in
    if lhost' != lhost || offset' != offset
    then Eva_ast_builder.mk_lval (lhost', offset')
    else lval

  let rewrite_offset ~visitor offset =
    match offset with
    | NoOffset -> offset
    | Field (fi, o) ->
      let o' = visitor.offset o in
      if o' != o then Field (fi, o') else offset
    | Index (e, o) ->
      let e' = visitor.exp e
      and o' = visitor.offset o in
      if e != e' || o' != o then Index (e', o') else offset

  let default = {
    rewrite_exp;
    rewrite_lval;
    rewrite_varinfo = (fun ~visitor:_ vi -> vi);
    rewrite_offset;
  }

  let visit_exp rewriter = let visitor = bind rewriter in visitor.exp
  let visit_lval rewriter = let visitor = bind rewriter in visitor.lval
end


module Fold =
struct
  type 'a visitor = {
    neutral : 'a;
    combine : 'a -> 'a -> 'a;
    exp : exp -> 'a;
    lval : lval -> 'a;
    varinfo : varinfo -> 'a;
    offset : offset -> 'a;
  }

  type 'a folder = {
    fold_exp : visitor:'a visitor -> exp -> 'a;
    fold_lval : visitor:'a visitor -> lval -> 'a;
    fold_varinfo : visitor:'a visitor -> varinfo -> 'a;
    fold_offset : visitor:'a visitor -> offset -> 'a;
  }

  let bind ~(neutral : 'a) ~(combine : 'a -> 'a -> 'a) (r : 'a folder) =
    let rec visitor = {
      neutral; combine;
      exp = (fun e -> r.fold_exp ~visitor e);
      lval = (fun lv -> r.fold_lval ~visitor lv);
      varinfo = (fun vi -> r.fold_varinfo ~visitor vi);
      offset = (fun o -> r.fold_offset ~visitor o);
    }
    in
    visitor

  let fold_exp ~visitor exp =
    match exp.node with
    | Lval lv | AddrOf lv | StartOf lv ->
      visitor.lval lv
    | UnOp (_, e, _) | CastE (_, e) ->
      visitor.exp e
    | BinOp (_op, e1, e2, _t) ->
      visitor.combine (visitor.exp e1) (visitor.exp e2)
    | Const _ ->
      visitor.neutral

  let fold_lval ~visitor lval =
    let lhost, offset = lval.node in
    let x =
      match lhost with
      | Var vi -> visitor.varinfo vi
      | Mem e -> visitor.exp e
    and y = visitor.offset offset in
    visitor.combine x y

  let fold_offset ~visitor offset =
    match offset with
    | NoOffset -> visitor.neutral
    | Field (_, o) -> visitor.offset o
    | Index (e, o) ->
      let x = visitor.exp e
      and y = visitor.offset o in
      visitor.combine x y

  let default = {
    fold_exp;
    fold_lval;
    fold_varinfo = (fun ~visitor _vi -> visitor.neutral);
    fold_offset;
  }

  let visit_exp ~neutral ~combine folder =
    let visitor = bind ~neutral ~combine folder in visitor.exp
  let visit_lval ~neutral ~combine folder =
    let visitor = bind ~neutral ~combine folder in visitor.lval
end
