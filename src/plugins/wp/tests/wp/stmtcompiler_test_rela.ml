(* test API of StmtCompiler for relational property verification*)

open Wp
open Factory
open Memory

let run () =
  let setup : Factory.setup = { mheap = Hoare;
                                mvar = Var;
                                cint = Cint.Natural;
                                cfloat = Cfloat.Real}
  in
  let driver = Driver.load_driver () in
  let model = Factory.instance setup driver in
  let module C = (val (Factory.compiler setup.mheap setup.mvar)) in
  let module Compiler = StmtSemantics.Make(C) in
  let module Cfg = CfgCompiler in

  let provers =
    List.fold_right
      (fun pname prvs -> match VCS.parse_prover pname with
         | None -> prvs
         | Some VCS.Tactical -> prvs
         | Some prv -> (VCS.Batch, prv) :: prvs)
      ["Alt-Ergo"] []
  in

  let spawn goal =
    let result _ prv res =
      Format.printf "[%a] %t@.@\n"
        VCS.pp_prover prv (VCS.pp_result_qualif prv res)
    in
    let server = ProverTask.server () in
    Prover.spawn goal ~delayed:true ~result provers;
    Task.launch server
  in

  let prove kf sequent =
    let goal = Wpo.GOAL.make sequent in
    let vc_annot = Wpo.VC_Annot.{
        axioms = None;
        goal;
        tags = []; warn = [];
        deps = Property.Set.empty;
        path = Cil_datatype.Stmt.Set.empty;
        source = None;
      }
    in
    let funct = Kernel_function.get_definition kf in
    let stmt = List.hd (funct.sbody.bstmts) in
    let pred = Cil_types.{
        pred_name = [];
        pred_loc = funct.svar.vdecl;
        pred_content = Cil_types.Ptrue;
      }
    in
    let pred = Logic_const.toplevel_predicate pred in
    let annot = Logic_const.new_code_annotation (AAssert ([],pred)) in
    let po = Wpo.{
        po_gid = "";
        po_sid = "";
        po_name = "";
        po_idx = Function(kf, None);
        po_model = model;
        po_pid = WpPropId.mk_assert_id kf stmt annot;
        po_formula = vc_annot;
      }
    in
    let inter_po = ref po in
    Property_status.iter (fun x ->
        match Wpo.goals_of_property x with
        | h :: _ ->
          inter_po := Wpo.{
              po_gid = "";
              po_sid = "";
              po_name = "";
              po_idx = Function(kf, None);
              po_model = model;
              po_pid = h.po_pid;
              po_formula = vc_annot;
            }
        | _ -> ()
      );
    spawn !inter_po;
  in

  let reads_formal f sigma =
    let v= C.M.load sigma
        (Ctypes.C_int (Wp.Ctypes.c_int Cil_types.IInt)) (C.M.cvar f)
    in
    let t =
      C.C.cval v
    in t
  in

  let run_test kf =
    let fct = Kernel_function.get_definition kf in
    WpContext.on_context (model,WpContext.Kf kf)
      begin fun () ->
        let block = Interpreted_automata.Compute.get_automaton ~annotations:true kf in
        let formal = List.hd (fct.sformals) in

        (*First call*)
        let seq1 = {Memory.pre = Cfg.node (); post = Cfg.node ()} in
        let env1 = Compiler.empty_env kf  in
        let env1 = Compiler.(env1 @* [Clabels.here,seq1.pre;Clabels.next,seq1.post]) in
        let path1 = Compiler.automaton env1 block in
        let cfg1 = path1.Compiler.paths_cfg in
        let node1 = Cfg.T.init' seq1.pre (reads_formal formal) in
        let (_,sigma1,sequence1) =
          Cfg.compile seq1.pre
            (Cfg.Node.Set.singleton seq1.post) (Cfg.T.reads node1) cfg1 in
        let node1 = Cfg.T.relocate sigma1 node1 in
        let term_1 = Cfg.T.get node1 in

        (*Seconde call*)
        let seq2 = {Memory.pre = Cfg.node (); post = Cfg.node ()} in
        let env2 = Compiler.empty_env kf  in
        let env2 = Compiler.(env2 @* [Clabels.here,seq2.pre;Clabels.next,seq2.post]) in
        let path2 = Compiler.automaton env2 block in
        let cfg2 = path2.Compiler.paths_cfg in
        let node2 = Cfg.T.init' seq2.pre (reads_formal formal) in
        let (_,sigma2,sequence2) =
          Cfg.compile
            seq2.pre (Cfg.Node.Set.singleton seq2.post)
            (Cfg.T.reads node2) cfg2 in
        let node2 = Cfg.T.relocate sigma2 node2 in
        let term_2 = Cfg.T.get node2 in

        let return1 =
          Compiler.result env1
        in
        let return1 = Lang.F.e_var return1 in
        let return2 =
          Compiler.result env2
        in
        let return2 = Lang.F.e_var return2 in
        let pred =
          Lang.F.p_imply (Lang.F.p_lt term_1 term_2) (Lang.F.p_lt return1 return2)
        in
        Format.printf "------The pred %a @." Lang.F.pp_pred pred;
        let sequent =
          (Conditions.concat [sequence1;sequence2]),pred
        in
        Format.printf "#######################################################################@.";
        Format.printf "Sequent: @[%a@]" !Conditions.pretty sequent;
        Format.printf "#######################################################################@.";
        prove kf sequent;
      end ()
  in

  let ordered_kf =
    List.sort (fun kf1 kf2 ->
        String.compare
          (Kernel_function.get_name kf1)
          (Kernel_function.get_name kf2))
      (Globals.Functions.fold (fun kf acc -> kf::acc) []) in

  List.iter (fun kf ->
      if Kernel_function.is_definition kf then
        Lang.local run_test kf)
    ordered_kf


let () = Boot.Main.extend run
