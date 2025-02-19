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

include Plugin.Register
    (struct
      let name = "Eva"
      let shortname = "eva"
      let help =
        "automatically computes variation domains for the variables of the program"
    end)

let () =
  add_plugin_output_aliases ~visible:false ~deprecated:true [ "value" ; "val" ]

(* Do not add dependencies to Kernel parameters here, but at the top of
   Parameters. *)
let kernel_dependencies =
  [ Ast.self;
    Alarms.self;
    Annotations.code_annot_state; ]

let proxy = State_builder.Proxy.(create "eva" Forward kernel_dependencies)
let state = State_builder.Proxy.get proxy

(* Current state of the analysis *)
type computation_state = NotComputed | Computing | Computed | Aborted

module ComputationState =
struct
  let to_string = function
    | NotComputed -> "NotComputed"
    | Computing -> "Computing"
    | Computed -> "Computed"
    | Aborted -> "Aborted"

  module Prototype =
  struct
    include Datatype.Serializable_undefined
    type t = computation_state
    let name = "Eva.Analysis.ComputationState"
    let pretty fmt s = Format.pp_print_string fmt (to_string s)
    let reprs = [ NotComputed ; Computing ; Computed ; Aborted ]
    let dependencies = [ state ]
    let default () = NotComputed
  end

  module Datatype' = Datatype.Make (Prototype)
  include (State_builder.Ref (Datatype') (Prototype))
end

exception Abort

let is_computed () =
  match ComputationState.get () with
  | Computed | Aborted -> true
  | NotComputed | Computing -> false

(* Debug categories. *)
let dkey_initial_state =
  register_category "initial-state"
    ~help:"at the start of the analysis, \
           print the initial value of global variables"

let dkey_final_states =
  register_category "final-states"
    ~help:"at the end of the analysis, print final values inferred \
           at the return point of each analyzed function "

let dkey_summary =
  register_category "summary"
    ~help:"print a summary of the analysis at the end, including coverage \
           and alarm numbers"

let dkey_pointer_comparison =
  register_category "pointer-comparison"
    ~help:"messages about the evaluation of pointer comparisons"

let dkey_cvalue_domain =
  register_category "d-cvalue"
    ~help:"print states of the cvalue domain on some user directives"

let dkey_iterator =
  register_category "iterator"
    ~help:"debug messages about the fixpoint engine on the control-flow graph \
           of functions"

let dkey_widening =
  register_category "widening"
    ~help:"print a message at each point where the analysis applies a widening"

let dkey_partition =
  register_category "partition"
    ~help:"messages about states partitioning"

let () =
  let activate dkey = add_debug_keys dkey in
  List.iter activate
    [dkey_initial_state; dkey_final_states; dkey_summary; dkey_cvalue_domain;
     dkey_partition]

(* Warning categories. *)
let wkey_alarm = register_warn_category "alarm"
let wkey_locals_escaping = register_warn_category "locals-escaping"
let wkey_garbled_mix_write = register_warn_category "garbled-mix:write"
let () = set_warn_status wkey_garbled_mix_write Log.Wfeedback
let wkey_garbled_mix_assigns = register_warn_category "garbled-mix:assigns"
let () = set_warn_status wkey_garbled_mix_assigns Log.Wfeedback
let wkey_garbled_mix_summary = register_warn_category "garbled-mix:summary"
let () = set_warn_status wkey_garbled_mix_summary Log.Wfeedback
let wkey_builtins_missing_spec = register_warn_category "builtins:missing-spec"
let wkey_builtins_override = register_warn_category "builtins:override"
let wkey_libc_unsupported_spec = register_warn_category "libc:unsupported-spec"
let wkey_loop_unroll_auto = register_warn_category "loop-unroll:auto"
let () = set_warn_status wkey_loop_unroll_auto Log.Wfeedback
let wkey_loop_unroll_partial = register_warn_category "loop-unroll:partial"
let () = set_warn_status wkey_loop_unroll_partial Log.Wfeedback
let wkey_missing_loop_unroll = register_warn_category "loop-unroll:missing"
let () = set_warn_status wkey_missing_loop_unroll Log.Winactive
let wkey_missing_loop_unroll_for = register_warn_category "loop-unroll:missing:for"
let () = set_warn_status wkey_missing_loop_unroll_for Log.Winactive
let wkey_signed_overflow = register_warn_category "signed-overflow"
let wkey_invalid_assigns = register_warn_category "assigns:invalid-location"
let () = set_warn_status wkey_invalid_assigns Log.Wfeedback
let wkey_missing_assigns = register_warn_category "assigns:missing"
let () = set_warn_status wkey_missing_assigns Log.Werror
let wkey_missing_assigns_result = register_warn_category "assigns:missing-result"
let wkey_experimental = register_warn_category "experimental"
let wkey_unknown_size = register_warn_category "unknown-size"
let wkey_ensures_false = register_warn_category "ensures-false"
let wkey_watchpoint = register_warn_category "watchpoint"
let () = set_warn_status wkey_watchpoint Log.Wfeedback
let wkey_recursion = register_warn_category "recursion"
let () = set_warn_status wkey_recursion Log.Wfeedback
