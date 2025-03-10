open Logic_ptree
open Cil_types
open Logic_typing

let type_foo typing_context _loc l =
  let preds =
    List.map
      (typing_context.type_predicate
         typing_context typing_context.pre_state)
      l
  in
  Ext_preds preds

module Count = State_builder.Counter(struct let name = "Count" end)

module Bar_table =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Datatype.List(Cil_datatype.Predicate))
    (struct
      let name = "Bar_table"
      let dependencies = [ Count.self ]
      let size = 3
    end)

let type_bar typing_context _loc l =
  let i = Count.next() in
  let p =
    List.map
      (typing_context.type_predicate
         typing_context
         (typing_context.post_state [Normal])) l
  in
  Bar_table.add i p;
  Ext_id i

let print_bar prt fmt ext =
  match ext with
  | Ext_id idx ->
    let l = Bar_table.find idx in
    Pretty_utils.pp_list
      ~pre:"@[<hov 2>" ~sep:",@ " ~suf:"@]" prt#predicate fmt l
  | Ext_preds _ | Ext_terms _ | Ext_annot _->
    Kernel.fatal "bar extension should have ids as arguments"

let visit_bar vis ext =
  match ext with
  | Ext_id idx ->
    let l = Bar_table.find idx in
    let l' = Extlib.map_no_copy (Cil.visitCilPredicate vis) l in
    if Visitor_behavior.is_copy vis#behavior then begin
      let idx' = Count.next () in
      Queue.add (fun () -> Bar_table.add idx' l') vis#get_filling_actions;
      Cil.ChangeTo(Ext_id idx')
    end else begin
      Bar_table.replace idx l';
      Cil.SkipChildren
    end
  | Ext_terms _ | Ext_preds _ | Ext_annot _ ->
    Kernel.fatal "bar extension should have ids as arguments"

let type_baz typing_context _loc l =
  let t =
    List.map
      (typing_context.type_term typing_context typing_context.pre_state) l
  in
  Ext_terms t

module Count_bla = State_builder.Counter(struct let name = "Count_bla" end)

module Bla_table =
  State_builder.Hashtbl(Datatype.Int.Hashtbl)(Cil_datatype.Predicate)
    (struct
      let name = "Bla_table"
      let dependencies = [ Ast.self; Count_bla.self ]
      let size = 3
    end)

let add_builtin () =
  let trace =
    { bl_name = "\\trace";
      bl_labels = []; bl_params = []; bl_type = None;
      bl_profile = [ "x", Linteger ] }
  in
  Logic_builtin.add trace

let () = add_builtin ()

let type_bla typing_context _loc l =
  let type_predicate ctxt env p =
    match p.lexpr_node with
    | PLapp("\\trace", [], [pred]) ->
      let pred = typing_context.type_predicate typing_context env pred in
      let li = List.hd (Logic_env.find_all_logic_functions "\\trace") in
      let i = Count.next () in
      let ti = Logic_const.tinteger ~loc:pred.pred_loc i in
      Bla_table.add i pred;
      Logic_const.papp ~loc:p.lexpr_loc (li,[],[ti])
    | _ -> typing_context.type_predicate ctxt env p
  in
  let ctxt = { typing_context with type_predicate } in
  let l =
    List.map (type_predicate ctxt ctxt.pre_state) l
  in
  Ext_preds l

let type_empty _ loc = function
  | [] -> Ext_terms [];
  | _ -> Kernel.abort ~loc "empty_extension should not have arguments"

let () =
  Acsl_extension.register_behavior ~plugin:"test"
    "foo" type_foo false ;
  Acsl_extension.register_code_annot_next_loop ~plugin:"test"
    "lfoo" type_foo false ;
  Acsl_extension.register_code_annot ~plugin:"test"
    "ca_foo" type_foo false ;
  Acsl_extension.register_code_annot_next_stmt ~plugin:"test"
    "ns_foo" type_foo false ;
  Acsl_extension.register_global ~plugin:"test"
    "global_foo" type_foo false ;
  Acsl_extension.register_behavior ~plugin:"test"
    "bar" type_bar ~printer:print_bar ~visitor:visit_bar false ;
  Acsl_extension.register_behavior ~plugin:"test"
    "bla" type_bla false ;
  Acsl_extension.register_code_annot_next_both ~plugin:"test"
    "baz" type_baz false;
  Acsl_extension.register_code_annot ~plugin:"test"
    "empty_extension" type_empty false

let run () =
  Ast.compute ();
  let debug = Kernel.debug_atleast 1 in
  let my_file = Extlib.temp_file_cleanup_at_exit ~debug "Extend" ".i" in
  let out = open_out my_file in
  let fmt = Format.formatter_of_out_channel out in
  File.pretty_ast ~fmt ();
  let prj = Project.create "reparsing" in
  Project.on prj add_builtin ();
  Project.on prj Kernel.Files.add (Datatype.Filepath.of_string my_file);
  Kernel.feedback "Reparsing file";
  (* Avoid having a temporary name in the oracle. *)
  Kernel.Verbose.set 0;
  Project.on prj Ast.compute ();
  File.pretty_ast ~prj ()

let () = Boot.Main.extend run
