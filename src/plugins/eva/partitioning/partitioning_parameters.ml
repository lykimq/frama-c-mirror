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

open Parameters
open Eva_annotations
open Cil_types

let is_return s = match s.skind with Return _ -> true | _ -> false
let is_loop s =   match s.skind with Loop _ -> true | _ -> false

let warn ?(current = true) =
  Kernel.warning ~wkey:Kernel.wkey_annot_error ~once:true ~current

module Make (Kf : sig val kf: kernel_function end) =
struct
  let kf = Kf.kf

  let widening_delay = WideningDelay.get ()
  let widening_period = WideningPeriod.get ()

  let interpreter_mode = InterpreterMode.get ()

  let slevel stmt =
    if is_return stmt || interpreter_mode then
      max_int
    else match Per_stmt_slevel.local kf with
      | Per_stmt_slevel.Global i -> i
      | Per_stmt_slevel.PerStmt f -> f stmt

  let merge_after_loop = SlevelMergeAfterLoop.mem kf

  let merge stmt =
    is_loop stmt && merge_after_loop
    ||
    match Per_stmt_slevel.merge kf with
    | Per_stmt_slevel.NoMerge -> false
    | Per_stmt_slevel.Merge f -> f stmt

  let min_loop_unroll = MinLoopUnroll.get ()
  let auto_loop_unroll = AutoLoopUnroll.get ()
  let default_loop_unroll = DefaultLoopUnroll.get ()

  let warn_no_loop_unroll stmt =
    let is_attribute a = Ast_attributes.contains a stmt.sattr in
    match List.filter is_attribute ["for" ; "while" ; "dowhile"] with
    | [] -> ()
    | loop_kind :: _ ->
      let wkey =
        if loop_kind = "for"
        then Self.wkey_missing_loop_unroll_for
        else Self.wkey_missing_loop_unroll
      in
      Self.warning
        ~wkey ~source:(fst (Cil_datatype.Stmt.loc stmt)) ~once:true
        "%s loop without unroll annotation" loop_kind

  let unroll stmt =
    let automatic_unrolling i =
      if i > min_loop_unroll
      then Partition.AutoUnroll (stmt, min_loop_unroll, i)
      else Partition.IntLimit min_loop_unroll
    in
    let default = automatic_unrolling auto_loop_unroll in
    match get_unroll_annot stmt with
    | [] -> warn_no_loop_unroll stmt; default
    | [UnrollFull] -> Partition.IntLimit default_loop_unroll
    | [UnrollAuto i] -> automatic_unrolling i
    | [UnrollAmount t] -> begin
        (* Inlines the value of const variables in [t]. *)
        let global_init vi =
          try (Globals.Vars.find vi).init with Not_found -> None
        in
        let t =
          Cil.visitCilTerm (new Logic_utils.simplify_const_lval global_init) t
        in
        try
          match Logic_utils.constFoldTermToInt t with
          | Some n -> Partition.IntLimit (Integer.to_int_exn n)
          | None   -> Partition.ExpLimit (Logic_to_c.term_to_exp t)
        with Z.Overflow | Logic_to_c.No_conversion ->
          warn "invalid loop unrolling parameter; ignoring";
          default
      end
    | _ :: _ :: _ ->
      warn "more than one loop unroll annotation; ignoring";
      default

  let history_size = HistoryPartitioning.get ()

  let split_limit = SplitLimit.get ()

  let universal_splits =
    let add name l =
      try
        let vi = Globals.Vars.find_from_astinfo name Global in
        let term = Eva_annotations.Expression (Cil.evar vi)
        and kind = Partition.Dynamic in
        let monitor = Partition.new_monitor ~limit:split_limit ~term ~kind in
        Partition.Split monitor :: l
      with Not_found ->
        warn ~current:false "cannot find the global variable %s for value \
                             partitioning; ignoring" name;
        l
    in
    ValuePartitioning.fold add []

  let flow_actions stmt =
    let map_annot acc t =
      try
        let action =
          match t with
          | FlowSplit (term, kind) ->
            let split_monitor =
              Partition.new_monitor ~limit:split_limit ~kind ~term
            in
            Partition.Split split_monitor
          | FlowMerge t ->
            Partition.Merge t
        in
        action :: acc
      with
        Logic_to_c.No_conversion ->
        warn "split/merge expressions must be valid expressions; ignoring";
        acc (* Impossible to convert term to lval *)
    in
    List.fold_left map_annot [] (get_flow_annot stmt)

  let call_return_policy =
    Partition.{
      callee_splits = Parameters.InterproceduralSplits.get ();
      callee_history = Parameters.InterproceduralHistory.get ();
      caller_history = true;
      history_size = history_size;
    }
end
