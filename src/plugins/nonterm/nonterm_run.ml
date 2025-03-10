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
open Cil_datatype

module Self = Plugin.Register
    (struct
      let name = "nonterm"
      let shortname = "nonterm"
      let help =
        "Warns when definitively non-terminating functions/loops are \
         detected (e.g. reachable functions with unreachable returns)."
    end)

module Enabled =
  Self.WithOutput
    (struct
      let option_name = "-nonterm"
      let help = "when on (off by default), \
                  warns about non-terminating functions/loops"
      let output_by_default = false
    end)

let () = Parameter_customize.argument_may_be_fundecl ()
module Ignore =
  Self.Filled_string_set
    (struct
      let option_name = "-nonterm-ignore"
      let arg_name = "f1,..,fn"
      let help = "ignore functions f1,..,fn and direct calls to them. \
                  Functions prefixed with '-' are removed from the ignore \
                  list. Calls via function pointers are never ignored. \
                  By default, the following functions are ignored: \
                  abort, exit"
      let default = Datatype.String.Set.of_list ["abort"; "exit"]
    end)

module DeadCode =
  Self.False
    (struct
      let option_name = "-nonterm-dead-code"
      let help = "warns about syntactically unreachable code. \
                  Note that this may emit a substantial amount of warnings."
    end)

let pretty_stmt_kind fmt stmt =
  match stmt.skind with
  | Break _ ->
    let implicit =
      try
        let kf = Kernel_function.find_englobing_kf stmt in
        let loop = Kernel_function.find_enclosing_loop kf stmt in
        (* heuristic: if both statements have the same location, then
           the break was implicitly generated *)
        Location.equal (Stmt.loc stmt) (Stmt.loc loop)
      with Not_found -> false
    in
    Format.fprintf fmt "%sbreak" (if implicit then "implicit " else "")
  | Return _ ->
    (* heuristic: if the return statement has no predecessors, then
       it is implicitly generated (and dead code) *)
    let implicit = stmt.preds = [] in
    Format.fprintf fmt "%sreturn" (if implicit then "implicit " else "")
  | Loop _ -> Format.fprintf fmt "loop"
  | Switch _ -> Format.fprintf fmt "switch"
  | Instr (Call _) -> Format.fprintf fmt "function call"
  | Instr (Local_init(_,ConsInit _,_)) ->
    Format.fprintf fmt "function call (initializer)"
  | _ -> Format.fprintf fmt "statement"

let pp_numbered_stacks fmt callstacks =
  if List.length callstacks < 2 then
    Format.fprintf fmt "stack: %a"
      (Pretty_utils.pp_list ~sep:": " Eva.Callstack.pretty) callstacks
  else
    (* number callstacks *)
    let numbered_callstacks =
      List.mapi (fun i cs -> (i+1, cs)) callstacks
    in
    Format.fprintf fmt "%a"
      (Pretty_utils.pp_list ~sep:"@\n"
         (Pretty_utils.pp_pair ~pre:"stack " ~sep:": "
            Format.pp_print_int Eva.Callstack.pretty))
      numbered_callstacks

let wkey_stmt = Self.register_warn_category "stmt"

let warn_nonterminating_statement stmt callstacks =
  Self.warning ~wkey:wkey_stmt ~source:(fst (Stmt.loc stmt))
    "non-terminating %a@\n%a"
    pretty_stmt_kind stmt pp_numbered_stacks callstacks

let wkey_dead = Self.register_warn_category "dead-code"

let warn_dead_code stmt =
  Self.warning ~wkey:wkey_dead ~source:(fst (Stmt.loc stmt))
    "%a is syntactically unreachable" pretty_stmt_kind stmt

class dead_cc_collector kf = object
  inherit Visitor.frama_c_inplace

  val reachable =
    let first = Kernel_function.find_first_stmt kf in
    let initial_reachable = Stmt.Hptset.add first (Stmts_graph.reachable_stmts kf first) in
    ref initial_reachable

  val dead_ccs = ref []
  val cur_cc = ref []

  method get =
    (* the last cc may not have been finalized *)
    if !cur_cc <> [] then begin
      dead_ccs := !cur_cc :: !dead_ccs;
      cur_cc := []
    end;
    !dead_ccs

  method! vstmt stmt =
    let new_succs cc s = List.filter (fun s' -> not (List.mem s' cc)) s.succs in
    if not (Stmt.Hptset.mem stmt !reachable) then begin
      (* add [stmt] and its successors to a connected component; if there is
         already one with [stmt], remain there, otherwise create a new one *)
      begin
        if !cur_cc = [] then begin
          let cc = stmt :: new_succs [] stmt in
          cur_cc := cc
        end else
        if List.mem stmt !cur_cc then begin
          (* part of same cc: update cc in previous list *)
          cur_cc := !cur_cc @ new_succs !cur_cc stmt;
        end else (* new cc *) begin
          dead_ccs := !cur_cc :: !dead_ccs;
          cur_cc := stmt :: new_succs [] stmt;
        end
      end;
      reachable := Stmt.Hptset.add stmt !reachable
    end;
    Cil.DoChildren
end

let wkey_unreachable = Self.register_warn_category "unreachable"

let warn_unreachable_statement stmt =
  Self.warning ~wkey:wkey_unreachable ~source:(fst (Stmt.loc stmt))
    "unreachable %a" pretty_stmt_kind stmt

class unreachable_stmt_visitor kf to_ignore = object
  inherit Visitor.frama_c_inplace

  val semantically_unreachable : stmt list ref = ref []

  method get : stmt list = !semantically_unreachable

  val syntactically_reachable =
    let first = Kernel_function.find_first_stmt kf in
    let initial_reachable = Stmt.Hptset.add first (Stmts_graph.reachable_stmts kf first) in
    initial_reachable

  val semantically_considered = ref to_ignore

  method! vstmt stmt =
    if Stmt.Hptset.mem stmt syntactically_reachable &&
       not (Eva.Results.is_reachable stmt) &&
       not (Stmt.Hptset.mem stmt !semantically_considered)
    then begin
      (* add node and its reachable successors to the considered statements *)
      let cc = Stmt.Hptset.add stmt (Stmts_graph.reachable_stmts kf stmt) in
      semantically_considered :=
        Stmt.Hptset.union !semantically_considered cc;
      semantically_unreachable := stmt :: !semantically_unreachable
    end;
    Cil.DoChildren
end

(* Unreachable returns only need to be checked if:
   1. SyntacticallyUnreachable is disabled (otherwise it already checks them);
   2. No warnings were emitted for the function (otherwise it may be redundant). *)
let check_unreachable_returns kf =
  try
    let ret_stmt = Kernel_function.find_return kf in
    if not (Eva.Results.is_reachable ret_stmt) then
      warn_unreachable_statement ret_stmt
  with
  | Kernel_function.No_Statement -> (* should never happen *)
    Self.error "function %a has no return statement, skipping"
      Kernel_function.pretty kf

(* Checks [kf] for unreachable statements (ignoring those in [to_ignore])
   and emits warnings. [warned_kfs] indicates functions which already had
   warnings emitted, to minimize the amount of redundant ones. *)
let check_unreachable_statements kf ~to_ignore ~dead_code ~warned_kfs =
  match Eva.Analysis.status kf with
  | Unreachable | Analyzed NoResults -> ()
  | SpecUsed | Builtin _ ->
    (* TODO: consider as non-terminating if spec has
       \terminates(false) or \ensures(false) *)
    Self.debug "not analyzing function %a@ \
                (using specification instead of definition),@ \
                considered as always terminating"
      Kernel_function.pretty kf
  | Analyzed _ ->
    try
      let vis = new unreachable_stmt_visitor kf to_ignore in
      ignore (Visitor.visitFramacKf (vis :> Visitor.frama_c_visitor) kf);
      if dead_code then begin
        (* compute syntactically unreachable statements *)
        let vis = new dead_cc_collector kf in
        ignore (Visitor.visitFramacKf (vis :> Visitor.frama_c_visitor) kf);
        let cc_heads = List.map List.hd vis#get in
        Stmt.Hptset.iter (fun h -> warn_dead_code h) (Stmt.Hptset.of_list cc_heads)
      end
      else if not (Kernel_function.Set.mem kf warned_kfs) then
        check_unreachable_returns kf
    with
    | Kernel_function.No_Statement -> (* should never happen *)
      Self.error "function %a has no return statement, skipping"
        Kernel_function.pretty kf

(* To avoid redundant warnings, calls to possibly non-terminating functions
   are ignored if:
   1. the function is in the list of functions to be ignored;
   2. or Eva results are available for the function.
   In case 2, the call is ignored because non-terminating statements inside
   it will already be reported. *)
let ignore_kf name =
  try
    let kf = Globals.Functions.find_by_name name in
    Ignore.mem name || Eva.Results.are_available kf
  with Not_found -> false

(* simple statement collector: accumulates a list of all
   statements, except calls to functions in [to_ignore]. *)
class stmt_collector = object
  inherit Visitor.frama_c_inplace
  val instr_stmts = ref []
  method! vstmt stmt =
    begin
      match stmt.skind with
      | (Instr (Call (_, {enode = Lval (Var vi, _)}, _, _))
        | Instr (Local_init (_, ConsInit(vi,_,_), _))) when
          (ignore_kf vi.vname) -> ()
      | _ -> instr_stmts := stmt :: !instr_stmts
    end;
    Cil.DoChildren
  method get_instr_stmts = List.rev !instr_stmts
end

(* collects the list of non-terminating instructions *)
let collect_nonterminating_statements fd nonterm_stacks =
  let vis = new stmt_collector in
  ignore (Visitor.visitFramacFunction (vis :> Visitor.frama_c_visitor) fd);
  let new_nonterm_stmts = ref Stmt.Hptset.empty in
  let add_stack stmt cs =
    new_nonterm_stmts := Stmt.Hptset.add stmt !new_nonterm_stmts;
    let prev_stack_list =
      try
        Hashtbl.find nonterm_stacks stmt
      with Not_found -> []
    in
    Hashtbl.replace nonterm_stacks stmt (cs :: prev_stack_list)
  in
  List.iter (fun stmt ->
      match stmt.skind with
      | Block _ -> (* do not compute; already done for the block stmts *) ()
      | _ ->
        let source = fst (Stmt.loc stmt) in
        Self.debug ~source "processing stmt:@ %a" Printer.pp_stmt stmt;
        let process_callstack cs =
          if Eva.Results.(after stmt |> in_callstack cs |> is_empty) then
            add_stack stmt cs
          else if match stmt.skind with Loop _ -> true | _ -> false then begin
            (* special treatment for loops: even if their after state
                is reachable, we must check that at least one outgoing
                edge is reachable *)
            let out_edges = Stmts_graph.get_all_stmt_out_edges stmt in
            let all_out_edges_unreachable =
              List.for_all (fun (_, out_stmt) ->
                  Eva.Results.(before out_stmt |> in_callstack cs |> is_empty)
                ) out_edges
            in
            if all_out_edges_unreachable then add_stack stmt cs
          end
        in
        List.iter process_callstack Eva.Results.(before stmt |> callstacks)
    ) vis#get_instr_stmts;
  !new_nonterm_stmts

let run () =
  if not (Ast.is_computed ()) then
    Self.abort "nonterm requires a computed AST";
  if not (Eva.Analysis.is_computed ()) then
    Self.abort "nonterm requires a computed value analysis";
  Self.debug "Starting analysis...";
  let file = Ast.get () in
  let globals = file.globals in
  let nonterm_stacks = Hashtbl.create 13 in
  List.iter (fun glob ->
      match glob with
      | GFun (fd, _loc) ->
        let fname = fd.svar.vorig_name in
        if Ignore.mem fname then
          Self.debug "ignoring function: %s" fname
        else begin
          Self.debug "considering function: %s" fname;
          let new_nonterm_stmts =
            collect_nonterminating_statements fd nonterm_stacks
          in
          let warned_kfs =
            Stmt.Hptset.fold (fun stmt acc ->
                let cs = Hashtbl.find nonterm_stacks stmt in
                let cs = List.sort Eva.Callstack.compare_lex cs in
                warn_nonterminating_statement stmt cs;
                Kernel_function.Set.add (Kernel_function.find_englobing_kf stmt) acc
              ) new_nonterm_stmts Kernel_function.Set.empty
          in
          let kf = Globals.Functions.get fd.svar in
          check_unreachable_statements kf ~to_ignore:new_nonterm_stmts ~warned_kfs
            ~dead_code:(DeadCode.get());
        end
      | _ -> ()
    ) globals;
  Self.feedback ~level:2 "Analysis done."
;;

let run_once, _ = State_builder.apply_once "Nonterm.run" [Eva.Analysis.self] run

let main () =
  if Enabled.get () then run_once ()

let () =
  Boot.Main.extend main
