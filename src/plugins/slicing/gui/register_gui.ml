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

(* Update the 'Slicing' column of the gui filetree. *)
let update_column  = ref (fun _ -> ())

(* Are results shown? *)
module Enabled = struct
  include State_builder.Ref
      (Datatype.Bool)
      (struct
        let name = "Slicing_gui.State"
        let dependencies = [Api.self]
        let default () = false
      end)
end

(* for slicing callback *)
let mk_selection fselect = fselect Api.Select.empty_selects

(* for slicing callback *)
let mk_selection_cad fselect =
  mk_selection fselect (Api.Mark.make ~ctrl:true ~addr:true ~data:true)

(* for slicing callback *)
let mk_selection_all fselect =
  mk_selection fselect ~spare:false

(* for slicing callback *)
let mk_slice selection =
  Enabled.set true;
  Api.Project.reset_slicing ();
  Api.Request.add_persistent_selection selection ;
  Api.Request.apply_all_internal ();
  if SlicingParameters.Mode.Callers.get () then
    Api.Slice.remove_uncalled ();
  let sliced_project_name =
    SlicingParameters.ProjectName.get () ^ SlicingParameters.ExportedProjectPostfix.get ()
  in
  Api.Project.extract sliced_project_name

(* To add a sensitive/insensitive menu item to a [factory] *)
let add_item (factory:GMenu.menu GMenu.factory) ~callback name arg_opt =
  match arg_opt with
  | None ->
    (* add the menu item, but it isn't sensitive *)
    let item = factory#add_item name ~callback:(fun () -> ()) in
    item#misc#set_sensitive false
  | Some arg ->
    (* add the menu item with its callback *)
    ignore (factory#add_item name ~callback:(fun () -> callback arg))

(* To inform the user about a status. *)
let gui_annot_info (main_ui:Design.main_window_extension_points) ?(level=2) txt=
  if (SlicingParameters.verbose_atleast level) then begin
    main_ui#pretty_information "%t.@." txt
  end

(* To inform the user about an error. *)
let gui_mk_slice (main_ui:Design.main_window_extension_points) selection ~info =
  gui_annot_info main_ui info;
  let new_project = mk_slice selection in (* ... slicing computation *)
  gui_annot_info main_ui
    (fun fmt -> Format.fprintf fmt "Slice exported to project: %s"
        (Project.get_unique_name new_project));
  main_ui#rehighlight ()

let msg_help_enable_gui = "Enables/Disables the Slicing GUI."
let msg_help_libraries =
  "Allows/Disallows the use of the -slicing-level option for calls to \
   undefined functions."

let check_value_computed (main_ui:Design.main_window_extension_points) =
  if Eva.Analysis.is_computed () then true
  else
    let answer = GToolbox.question_box
        ~title:("Eva Needed")
        ~buttons:[ "Run"; "Cancel" ]
        ("Eva has to be run first.\nThis can take some time and may \
          require some special settings.\n"
         ^"Do you want to run Eva with its current settings now?")
    in
    if answer = 1 then
      match main_ui#full_protect ~cancelable:true Eva.Analysis.compute with
      | Some _ ->
        main_ui#redisplay (); (* New alarms *)
        true
      | None -> false
    else false

(* To do an action and inform the user. *)
let gui_apply_action (main_ui:Design.main_window_extension_points) f x ~info =
  f x ;
  gui_annot_info main_ui info

let slicing_selector (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) ~button localizable =
  if (not (Eva.Analysis.is_computed ()))
  then
    ignore
      (popup_factory#add_item "Enable _slicing"
         ~callback:
           (fun () ->
              let enable () =
                Enabled.set true;
                !update_column `Visibility
              in
              if (not (Eva.Analysis.is_computed ())) then begin
                if check_value_computed main_ui then enable ()
              end
              else enable ()
           ))
  else
  if button = 1 then
    begin let level = 1 in
      let slicing_view () =
        gui_annot_info main_ui ~level
          (fun fmt -> Format.fprintf fmt "Highlighting.")
      in
      SlicingState.may slicing_view;
      if SlicingParameters.verbose_atleast level then begin
        let slicing_mark () =
          let slicing_mark kf get_mark =
            (* use -slicing-debug -verbose to get slicing mark information *)
            let add_mark_info txt = gui_annot_info ~level main_ui
                (fun fmt -> Format.fprintf fmt "Tag: %s" (txt ()))
            in
            let slices = Api.Slice.get_all kf in
            match slices with
            | [] -> (* No slice for this kf *)
              add_mark_info (fun () ->
                  if Api.Project.is_called kf
                  then (* but the source function is called *)
                    (Format.asprintf "<src>%a"
                       Api.Mark.pretty (Api.Mark.get_from_src_func kf))
                  else
                    "<   ><   >")
            | slices ->
              if Api.Project.is_called kf
              then begin (* The source function is also called *)
                assert (not (kf == fst (Globals.entry_point ()))) ;
                add_mark_info (fun () ->
                    Format.asprintf "<src>%a"
                      Api.Mark.pretty (Api.Mark.get_from_src_func kf))
              end ;
              let mark_slice slice =
                add_mark_info (fun () -> Format.asprintf "%a" Api.Mark.pretty (get_mark slice))
              in List.iter mark_slice slices
          in match localizable with
          | Printer_tag.PTermLval(Some kf,(Kstmt ki),_,_)
          | PLval (Some kf,(Kstmt ki),_)
          | PStmt (kf,ki) ->
            slicing_mark kf
              (fun slice -> Api.Slice.get_mark_from_stmt slice ki)
          | PVDecl (Some kf,_,vi) ->
            slicing_mark kf
              (fun slice -> Api.Slice.get_mark_from_local_var slice vi)
          | _ -> ()
        in
        SlicingState.may slicing_mark
      end
    end
  else if button = 3 then begin
    let submenu = popup_factory#add_submenu "Slicing" in
    let slicing_factory =
      new Design.protected_menu_factory (main_ui:>Gtk_helper.host) submenu
    in
    (* definitions for slicing plug-in *)
    let add_slicing_item name ~callback v =
      let callback v =
        callback v;
        !update_column `Contents
      in
      add_item slicing_factory name ~callback v
    in
    let mk_slice = gui_mk_slice main_ui in
    let add_slice_menu kf_opt kf_ki_lv_opt =
      (let callback kf =
         mk_slice
           ~info:(fun fmt ->
               Format.fprintf fmt
                 "Request for slicing effects of function %a"
                 Kernel_function.pretty kf)
           (mk_selection_all Api.Select.select_func_calls_to kf)
       in
       add_slicing_item "Slice calls to" kf_opt ~callback);

      (let callback kf =
         mk_slice
           ~info:(fun fmt ->
               Format.fprintf fmt
                 "Request for slicing entrance into function %a"
                 Kernel_function.pretty kf)
           (mk_selection_all Api.Select.select_func_calls_into kf)
       in
       add_slicing_item "Slice calls into" kf_opt ~callback);

      (let callback kf =
         mk_slice
           ~info:(fun fmt ->
               Format.fprintf fmt
                 "Request for returned value of function %a"
                 Kernel_function.pretty kf)
           (mk_selection_all Api.Select.select_func_return kf)
       in
       add_slicing_item "Slice result"
         (Extlib.opt_filter
            (fun kf ->
               let is_not_void_kf x =
                 match x.Cil_types.vtype.tnode with
                 | TFun ({ tnode = TVoid },_,_) -> false
                 | _ -> true
               in is_not_void_kf (Kernel_function.get_vi kf))
            kf_opt)
         ~callback);

      (let callback (kf, ki, _) =
         mk_slice
           ~info:(fun fmt ->
               Format.fprintf fmt
                 "Request for slicing effects of statement %d"
                 ki.sid)
           (mk_selection_all Api.Select.select_stmt ki kf)
       in
       add_slicing_item "Slice stmt" kf_ki_lv_opt ~callback);

      let get_lv lvopt text =
        match lvopt with
        | None ->
          Gtk_helper.input_string
            ~parent:main_ui#main_window ~title:"Enter an lvalue" text
        | Some lv ->
          (* For probably dubious reasons, the functions in Api.Select
             require strings instead of directly a lvalue. Thus, we convert
             our shiny lvalue to string, so that it may be parsed back... *)
          Some (Pretty_utils.to_string Printer.pp_lval lv)
      in
      (let callback (kf, ki, lvopt) =
         let do_with_txt txt =
           try
             let lval_str =
               Datatype.String.Set.add txt Datatype.String.Set.empty
             in
             mk_slice
               ~info:(fun fmt ->
                   Format.fprintf fmt
                     "Request for slicing lvalue %s before statement %d"
                     txt
                     ki.sid)
               (mk_selection_cad Api.Select.select_stmt_lval
                  lval_str ~before:true ki ~eval:ki kf)
           with e ->
             main_ui#error "Invalid expression: %s" (Printexc.to_string e)
         in
         let txt = get_lv lvopt
             "Input a lvalue to slice on its value before the current statement."
         in
         Option.iter do_with_txt txt
       in
       add_slicing_item "Slice lval" kf_ki_lv_opt ~callback);

      (let callback (kf, ki, lvopt) =
         let do_with_txt txt =
           try
             let lval_str =
               Datatype.String.Set.add txt Datatype.String.Set.empty
             in
             mk_slice
               ~info:(fun fmt ->
                   Format.fprintf fmt
                     "Request for slicing read accesses to lvalue %s"
                     txt)
               (mk_selection_cad
                  Api.Select.select_func_lval_rw
                  ~rd:lval_str
                  ~wr:Datatype.String.Set.empty
                  ~eval:ki kf)
           with e ->
             main_ui#error "Invalid expression: %s" (Printexc.to_string e)
         in
         let txt = get_lv lvopt
             "Input a lvalue to slice on its read accesses."
         in
         Option.iter do_with_txt txt
       in
       add_slicing_item "Slice rd" kf_ki_lv_opt ~callback);

      (let callback (kf, ki, lvopt) =
         let do_with_txt txt =
           try
             let lval_str =
               Datatype.String.Set.add txt Datatype.String.Set.empty
             in
             mk_slice
               ~info:(fun fmt ->
                   Format.fprintf fmt
                     "Request for slicing written accesses to lvalue %s"
                     txt)
               (mk_selection_cad
                  Api.Select.select_func_lval_rw
                  ~rd:Datatype.String.Set.empty
                  ~wr:lval_str
                  ~eval:ki kf)
           with e ->
             main_ui#error "Invalid expression: %s" (Printexc.to_string e)
         in
         let txt = get_lv lvopt
             "Input a lvalue to slice on its write accesses."
         in
         Option.iter do_with_txt txt
       in
       add_slicing_item "Slice wr" kf_ki_lv_opt ~callback);

      let callback (kf, ki, _) =
        mk_slice
          ~info:(fun fmt ->
              Format.fprintf fmt
                "Request for slicing accessibility to statement %d"
                ki.sid)
          (mk_selection_all Api.Select.select_stmt_ctrl ki kf)
      in
      add_slicing_item "Slice ctrl" kf_ki_lv_opt ~callback
    in
    let some_kf_from_vi vi =
      try let kf = Globals.Functions.get vi in
        if Eva.Results.is_called kf then Some kf else None
      with Not_found -> None in
    let some_kf_from_lv  lv =
      match lv with
      | Var vi,_ -> some_kf_from_vi vi
      | _ -> None
    in
    let some_kf_ki_lv kf stmt lvopt =
      if Eva.Results.is_called kf && Eva.Results.is_reachable stmt
      then Some (kf, stmt, lvopt) else None
    in
    begin  (* add menu for slicing and scope plug-in *)
      match localizable with
      | Printer_tag.PLval (Some kf,(Kstmt stmt),lv)->
        add_slice_menu
          (some_kf_from_lv lv) (some_kf_ki_lv kf stmt (Some lv))
(*
        | Pretty_source.PTermLval(Some kf,_,Kstmt ki,_)
            (* as for 'statement' localizable. We currently ignore the
               term-lval *)
*)
      | PStmt (kf, stmt) ->
        add_slice_menu None (some_kf_ki_lv kf stmt None)
      | PVDecl (kfopt,ki,vi) -> begin
          add_slice_menu (some_kf_from_vi vi) None;
          match kfopt, ki with
          | Some kf, Kstmt stmt ->
            add_slice_menu None (some_kf_ki_lv kf stmt None)
          | _ -> ()
        end
      | _  ->
        add_slice_menu None None
    end;
    ignore (slicing_factory#add_separator ());
  end

let slicing_highlighter(buffer:Design.reactive_buffer) localizable ~start ~stop=
  if Enabled.get () then begin
    (* Definition for highlight 'Slicing' *)
    let highlight () =
      let buffer = buffer#buffer in
      let ki = Pretty_source.ki_of_localizable localizable in
      if Eva.Results.is_reachable_kinstr ki then
        let unused_code_area =
          Gtk_helper.make_tag buffer
            ~name:"slicing_unused" [`STRIKETHROUGH true ]
        in
        let spare_code_area =
          Gtk_helper.make_tag buffer ~name:"slicing_spare" [`UNDERLINE `LOW] in
        let necessary_code_area =
          Gtk_helper.make_tag buffer
            ~name:"slicing_necessary" [`BACKGROUND "green"]
        in
        let apply_on_one_project_and_merge_slices kf pb pe mark_of_slice =
          let apply_mark mark =
            if SlicingParameters.debug_atleast 1 then
              SlicingParameters.debug "Got mark: %a"
                Api.Mark.pretty mark;
            if Api.Mark.is_bottom mark then
              Gtk_helper.apply_tag buffer unused_code_area pb pe;
            if Api.Mark.is_spare mark then
              Gtk_helper.apply_tag buffer spare_code_area pb pe;
            if (Api.Mark.is_ctrl mark
                || Api.Mark.is_data mark
                || Api.Mark.is_addr mark)
            then
              Gtk_helper.apply_tag buffer necessary_code_area pb pe
          in
          let slices = Api.Slice.get_all kf in
          begin
            match slices with
            | [] ->
              (* No slice for this kf *)
              if Api.Project.is_called kf
              then begin
                SlicingParameters.debug "Got source code@." ;
                apply_mark (Api.Mark.get_from_src_func kf)
              end
              else
                Gtk_helper.apply_tag buffer unused_code_area pb pe
            | slices ->
              if Api.Project.is_called kf
              then begin
                assert (not (kf == fst (Globals.entry_point ()))) ;
                SlicingParameters.debug "Got source code" ;
                apply_mark (Api.Mark.get_from_src_func kf)
              end ;
              if SlicingParameters.debug_atleast 1 then begin
                let l = List.length slices in
                if l >=2 then
                  SlicingParameters.debug "Got %d slices" (List.length slices)
              end;
              let mark_slice slice =
                let mark = mark_of_slice slice in
                apply_mark mark
              in List.iter mark_slice slices
          end
        in
        let tag_stmt kf stmt pb pe =
          assert (Eva.Results.is_reachable stmt) ;
          apply_on_one_project_and_merge_slices
            kf
            pb
            pe
            (fun slice -> Api.Slice.get_mark_from_stmt slice stmt)
        in
        let tag_vdecl kf vi pb pe =
          if not vi.vglob then
            apply_on_one_project_and_merge_slices
              kf
              pb
              pe
              (fun slice -> Api.Slice.get_mark_from_local_var slice vi)
        in
        match localizable with
        | Printer_tag.PStmt (kf,stmt) -> tag_stmt kf stmt start stop
        | PVDecl (Some kf,_,vi) -> tag_vdecl kf vi start stop
        | PStmtStart _
        | PVDecl (None,_,_)
        | PLval _
        | PTermLval _
        | PGlobal _
        | PIP _
        | PExp _
        | PType _ -> ()
    in
    (* 2. Highlights the 'Slicing' *)
    SlicingState.may highlight
  end

(* Not used *)
(* let none_text = "<i>None</i>" *)

let pretty_setting_option fmt =
  Format.fprintf fmt "@[Setting option %s@ %s@ for the current project@]"

let gui_set_slicing_debug (main_ui:Design.main_window_extension_points) v =
  let old = SlicingParameters.Verbose.get () in
  if v <> old then (* Otherwise set is done at every refreshing *)
    gui_apply_action main_ui SlicingParameters.Verbose.set v
      ~info:(fun fmt ->
          pretty_setting_option fmt "-slicing-verbose" (string_of_int v))

let gui_set_slicing_level (main_ui:Design.main_window_extension_points) v =
  let old = SlicingParameters.Mode.Calls.get () in
  if v != old then (* Otherwise set is done at every refreshing *)
    gui_apply_action main_ui SlicingParameters.Mode.Calls.set v
      ~info:(fun fmt ->
          pretty_setting_option fmt "-slicing-level" (string_of_int v))

let gui_set_slicing_undef_functions (main_ui:Design.main_window_extension_points) v =
  let old = SlicingParameters.Mode.SliceUndef.get () in
  if v != old then (* Otherwise set is done at every refreshing *)
    gui_apply_action main_ui SlicingParameters.Mode.SliceUndef.set v
      ~info:(fun fmt ->
          pretty_setting_option fmt
            (if v then "-slice-undef-functions" else "-no-slice-undef-functions")
            "")

let slicing_panel (main_ui:Design.main_window_extension_points) =
  let w = GPack.vbox  () in
  let table = GPack.table ~columns:2 ~rows:2 ~homogeneous:true ~packing:w#pack () in

  let hbox2 = GPack.hbox ~packing:(table#attach ~left:1 ~top:0) () in

  (* [enabled_button] to give slicing menu available *)
  let do_refresh to_enable =
    if to_enable then ignore (check_value_computed main_ui);
    !update_column `Visibility;
    main_ui#rehighlight ();
  in
  let enabled_button =
    let b = GButton.check_button
        ~label:"Enable"
        ~active:(Enabled.get ())
        ~packing:(table#attach ~left:0 ~top:0) () in
    main_ui#help_message b "%s" msg_help_enable_gui ;
    ignore (b#connect#toggled
              ~callback:(fun () ->
                  Enabled.set b#active;
                  do_refresh b#active));
    b
  in
  let verbose_refresh = Gtk_helper.on_int ~lower:0 ~upper:3
      hbox2
      "Verbosity"
      ~sensitive:Enabled.get
      SlicingParameters.Verbose.get
      (gui_set_slicing_debug main_ui)
  in
  let hbox3 = GPack.hbox ~packing:(table#attach ~left:1 ~top:1) () in
  (* [slice_undef_button] related to -slice-undef option *)
  let slice_undef_button =
    let b = GButton.check_button
        ~label:"Libraries"
        ~active:(Enabled.get ())
        ~packing:(table#attach ~left:0 ~top:1) () in
    main_ui#help_message b "%s" msg_help_libraries ;
    ignore (b#connect#toggled
              ~callback:(fun () ->
                  gui_set_slicing_undef_functions main_ui b#active));
    b
  in
  let level_refresh = Gtk_helper.on_int ~lower:0 ~upper:3
      hbox3
      "Level"
      ~sensitive:Enabled.get
      SlicingParameters.Mode.Calls.get
      (gui_set_slicing_level main_ui)
  in
  let refresh () =
    let value_is_computed = Eva.Analysis.is_computed () in
    let enabled = Enabled.get () in
    enabled_button#misc#set_sensitive value_is_computed ;
    slice_undef_button#misc#set_sensitive enabled ;
    verbose_refresh ();
    level_refresh ();
    if Enabled.get () <> enabled_button#active then (
      enabled_button#set_active (Enabled.get ());
      !update_column `Contents;
    );
    slice_undef_button#set_active (SlicingParameters.Mode.SliceUndef.get());
  in
  refresh () ;
  "Slicing",w#coerce,Some refresh

let file_tree_decorate (file_tree:Filetree.t) =
  update_column :=
    file_tree#append_pixbuf_column
      ~title:"Slicing"
      (fun globs ->
         SlicingState.may_map
           ~none:[`STOCK_ID ""]
           (fun () ->
              if List.exists
                  (fun glob -> match glob with
                     | GFun ({svar = vi},_ ) ->
                       begin
                         try
                           let kf = Globals.Functions.get vi
                           in (Api.Project.is_called kf)
                              || ( [] != (Api.Slice.get_all kf))
                         with Not_found -> false
                       end
                     |  _ -> false)
                  globs
              then [`STOCK_ID "gtk-apply"]
              else [`STOCK_ID ""]))
      (fun () -> Enabled.get ());
  !update_column `Visibility

let main (main_ui:Design.main_window_extension_points) =
  main_ui#register_source_selector slicing_selector;
  main_ui#register_source_highlighter slicing_highlighter;
  main_ui#register_panel slicing_panel;
  file_tree_decorate main_ui#file_tree

let () =
  Design.register_extension main

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
