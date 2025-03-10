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

type state =
  | Empty
  | Proof of ProofEngine.tree
  | Forking of ProofEngine.tree * ProofEngine.fork * Task.pool
  | Composer of ProofEngine.tree * GuiTactic.composer * Ptip.target
  | Browser of ProofEngine.tree * GuiTactic.browser * Ptip.target

let on_proof_context proof job data =
  let ctxt = ProofEngine.tree_context proof in
  WpContext.on_context ctxt job data

(* -------------------------------------------------------------------------- *)
(* --- Autofocus Management                                               --- *)
(* -------------------------------------------------------------------------- *)

type mode = [ `Refresh | `Autofocus | `ViewModel | `ViewAll | `ViewRaw ]

module Config = Gtk_helper.Configuration

class ['a] menu ~(data : ('a * string * string) list) ~key ~default =
  let options = List.map (fun (v,d,_) -> v,d) data in
  let values = List.map (fun (v,_,k) -> v,k) data in
  object(self)
    inherit ['a] Widget.menu ~default ~options ()
    initializer
      Wutil.later (fun () -> Config.config_values ~key ~default ~values self)
  end


class autofocus =
  object inherit [mode] menu
      ~key:"GuiGoal.autofocus"
      ~default:`Autofocus
      ~data:[
        `Refresh , "Refresh" , "REFRESH" ;
        `Autofocus , "Autofocus" , "AUTOFOCUS" ;
        `ViewAll , "Full Context" , "VIEW_ALL" ;
        `ViewModel , "Unmangled Memory" , "VIEW_MODEL" ;
        `ViewRaw , "Raw Obligation" , "VIEW_RAW" ;
      ]
  end

class iformat =
  object inherit [Plang.iformat] menu
      ~key:"GuiGoal.iformat"
      ~default:`Dec
      ~data:[
        `Dec , "Decimal" , "DEC" ;
        `Hex , "Hexa" , "HEX" ;
        `Bin , "Binary" , "BIN" ;
      ]
  end

class rformat =
  object inherit [Plang.rformat] menu
      ~key:"GuiGoal.rformat"
      ~default:`Ratio
      ~data:[
        `Ratio , "Real" , "REAL" ;
        `Float , "Float (32 bits)" , "F32" ;
        `Double , "Float (64 bits)" , "F64" ;
      ]
  end

(* -------------------------------------------------------------------------- *)
(* --- Goal Panel                                                         --- *)
(* -------------------------------------------------------------------------- *)

class pane (gprovers : GuiConfig.provers) =
  let icon = new Widget.image GuiProver.no_status in
  let status = new Widget.label () in
  let text = new Wtext.text () in
  let scripter = new GuiProof.printer text in
  let printer = new GuiSequent.focused text in
  let composer = new GuiComposer.composer printer in
  let browser = new GuiComposer.browser printer in
  let layout = new Wutil.layout in
  let palette = new Wpalette.panel () in
  let help = new Widget.button
    ~label:"Tactics" ~border:false ~tooltip:"List Available Tactics" () in
  let delete = new Widget.button
    ~icon:`DELETE ~tooltip:"Delete current proof" () in
  let cancel = new Widget.button
    ~icon:`UNDO ~tooltip:"Undo Proof Steps" () in
  let forward = new Widget.button
    ~icon:`MEDIA_FORWARD ~tooltip:"Go ahead among pending goals" () in
  let next = new Widget.button
    ~icon:`MEDIA_NEXT ~tooltip:"Goto next pending goal" () in
  let prev = new Widget.button
    ~icon:`MEDIA_PREVIOUS ~tooltip:"Goto previous pending goal" () in
  let play_script = new Widget.button
    ~icon:`REVERT_TO_SAVED ~tooltip:"Replay Session Script" () in
  let save_script = new Widget.button
    ~icon:`SAVE ~tooltip:"Save Script" () in
  let autofocus = new autofocus in
  let iformat = new iformat in
  let rformat = new rformat in
  let autosearch = new GuiTactic.autosearch () in
  let strategies = new GuiTactic.strategies () in
  object(self)

    val mutable state : state = Empty
    val mutable provers : GuiProver.prover list = []
    val mutable tactics : GuiTactic.tactic list = []

    initializer
      begin
        let toolbar =
          Wbox.(toolbar
                  [ w prev ; w next ; w cancel ; w forward ;
                    w autofocus ; w iformat ; w rformat ;
                    w play_script ; w save_script ;
                    w ~padding:6 icon ; h ~padding:6 status ]
                  [ w help ; w delete ]) in
        let content = Wbox.split ~dir:`HORIZONTAL
            text#widget (Wbox.scroll palette#widget) in
        Wutil.later (fun () ->
            Config.config_float ~key:"GuiGoal.palette" ~default:0.8 content
          );
        layout#populate (Wbox.panel ~top:toolbar content#widget) ;
        let why3_provers =
          List.map
            (fun dp -> new GuiProver.prover ~console:text ~prover:(VCS.Why3 dp))
            (Why3.Whyconf.Sprover.elements gprovers#get) in
        provers <- why3_provers ;
        List.iter (fun p -> palette#add_tool p#tool) provers ;
        palette#add_tool autosearch#tool ;
        palette#add_tool strategies#tool ;
        Strategy.iter autosearch#register ;
        ProofStrategy.iter strategies#register ;
        Tactical.iter
          (fun tac ->
             let gtac = new GuiTactic.tactic tac printer#pp_selection in
             tactics <- gtac :: tactics ;
             palette#add_tool gtac#tool) ;
        tactics <- List.rev tactics ;
        self#register_provers gprovers#get;
        printer#on_selection (fun () -> self#update) ;
        scripter#on_click self#goto ;
        scripter#on_backtrack self#backtrack ;
        gprovers#connect self#register_provers ;
        delete#connect (fun () -> self#interrupt ProofEngine.cancel_current_node) ;
        cancel#connect (fun () -> self#interrupt ProofEngine.cancel_parent_tactic) ;
        forward#connect (fun () -> self#forward) ;
        next#connect (fun () -> self#navigate succ) ;
        prev#connect (fun () -> self#navigate pred) ;
        save_script#connect (fun () -> self#save_script) ;
        play_script#connect (fun () -> self#play_script) ;
        autofocus#connect self#autofocus ;
        iformat#connect self#iformat ;
        rformat#connect self#rformat ;
        composer#connect (fun () -> self#update) ;
        browser#connect (fun () -> self#update) ;
        help#connect (fun () -> self#open_help) ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- Behavior                                                       --- *)
    (* ---------------------------------------------------------------------- *)

    val mutable helpmode = false
    method private open_help =
      helpmode <- true ;
      self#update

    method private quit_help =
      helpmode <- false ;
      self#update

    method private compose cc =
      match state with
      | Proof proof ->
        composer#clear ;
        let tgt = printer#unselect in
        state <- Composer(proof,cc,tgt) ;
        self#update
      | _ -> ()

    method private browse cc =
      match state with
      | Proof proof ->
        browser#clear ;
        let tgt = printer#unselect in
        state <- Browser(proof,cc,tgt) ;
        self#update
      | _ -> ()

    method private interrupt cancel =
      match state with
      | Empty -> ()
      | Proof proof | Composer(proof,_,_) | Browser(proof,_,_) ->
        cancel proof ;
        printer#reset ;
        self#update
      | Forking (proof,_,pool) ->
        cancel proof ;
        Task.iter Task.cancel pool ;
        state <- Proof proof ;
        ProofEngine.forward proof ;
        printer#reset ;
        self#update

    method private forward =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p -> ProofEngine.forward p ; self#update

    method private goto s =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p -> ProofEngine.goto p s ; self#update

    method private navigate f =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof p ->
        match ProofEngine.current p with
        | `Leaf (k,_) -> ProofEngine.goto p (`Leaf(f k)) ; self#update
        | `Main | `Internal _ -> ()

    method private iformat f = printer#set_iformat f ; self#update
    method private rformat f = printer#set_rformat f ; self#update

    method private autofocus = function
      | `Autofocus ->
        printer#set_focus_mode true ;
        printer#set_state_mode true ;
        self#update
      | `ViewRaw ->
        printer#set_focus_mode false ;
        printer#set_state_mode false ;
        self#update
      | `ViewModel ->
        printer#set_focus_mode true ;
        printer#set_state_mode false ;
        self#update
      | `ViewAll ->
        printer#set_focus_mode false ;
        printer#set_state_mode true ;
        self#update
      | `Refresh ->
        helpmode <- false ;
        printer#reset ;
        let mode =
          match printer#get_focus_mode , printer#get_state_mode with
          | true , true -> `Autofocus
          | false , false -> `ViewRaw
          | true , false -> `ViewModel
          | false , true -> `ViewAll
        in
        autofocus#set mode ; self#update

    method private provers =
      (List.map (fun dp -> VCS.Why3 dp)
         (Why3.Whyconf.Sprover.elements gprovers#get))

    method private play_script =
      match state with
      | Proof p ->
        ProofEngine.clear_tree p ;
        ProverScript.spawn
          ~provers:self#provers
          ~result:
            (fun wpo prv res ->
               text#printf "[%a] %a : %a@."
                 VCS.pp_prover prv Wpo.pp_title wpo VCS.pp_result res)
          ~success:
            (fun _ _ ->
               ProofEngine.forward p ;
               self#update)
          (ProofEngine.main p) ;
        let server = ProverTask.server () in
        Task.launch server
      | Empty | Forking _ | Composer _ | Browser _ -> ()

    method private save_script =
      match state with
      | Proof p ->
        let main = ProofEngine.main p in
        let json = ProofScript.encode (ProofEngine.script p) in
        ProofSession.save ~stdout:false main json ;
        ProofEngine.set_saved p true ;
        self#update
          (*
          text#clear ;
          text#printf "@{<bf>Session:@} '%a'@." ProofSession.pretty main ;
          text#printf "@[<hov 2>@{<bf>Script:@}@ %a@]@." Json.pp json ;
          self#update_statusbar ;
          *)
      | Empty | Forking _ | Composer _ | Browser _ -> ()

    (* ---------------------------------------------------------------------- *)
    (* --- Prover Controllers                                             --- *)
    (* ---------------------------------------------------------------------- *)

    method private register_provers dps =
      begin
        (* register missing provers *)
        let dps = Why3.Whyconf.Sprover.elements dps in
        let prvs = List.map (fun p -> VCS.Why3 p) dps in
        (* set visible provers *)
        List.iter
          (fun prover ->
             let prv = prover#prover in
             match prover#prover with
             | VCS.Why3 _ -> prover#set_visible (List.mem prv prvs)
             | _ -> ()
          ) provers ;
        (* add missing provers *)
        List.iter
          (fun prv ->
             if List.for_all (fun p -> p#prover <> prv) provers then
               begin
                 let prover = new GuiProver.prover ~console:text ~prover:prv in
                 begin match state with
                   | Proof p -> prover#update (ProofEngine.main p)
                   | Empty | Forking _ | Composer _ | Browser _ -> prover#clear
                 end ;
                 provers <- provers @ [ prover ] ;
                 palette#add_tool prover#tool ;
               end
          ) prvs ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- External API                                                   --- *)
    (* ---------------------------------------------------------------------- *)

    method select = function
      | None ->
        state <- Empty ; self#update
      | Some w ->
        let pw = ProofEngine.proof ~main:w in
        let changed = match state with
          | Empty -> true
          | Proof p -> p != pw
          | Forking _ | Composer _ | Browser _ -> false
        in
        if changed then
          begin
            printer#reset ;
            self#update_provers None ;
            self#update_tactics None ;
            state <- Proof pw ;
            ProofEngine.forward pw ;
            self#update ;
          end

    (* ---------------------------------------------------------------------- *)
    (* --- Repaint                                                        --- *)
    (* ---------------------------------------------------------------------- *)

    method coerce = layout#coerce

    method private update_provers = function
      | None ->
        List.iter (fun prover -> prover#clear) provers
      | Some wpo ->
        List.iter (fun prover -> prover#update wpo) provers

    method private update_tactics = function
      | None ->
        printer#highlight Tactical.Empty ;
        autosearch#connect None ;
        strategies#connect None ;
        List.iter (fun tactic -> tactic#clear) tactics
      | Some(tree,sequent,sel) ->
        on_proof_context tree
          begin fun () ->
            (* configure strategies *)
            let node = ProofEngine.head tree in
            let wpo = ProofEngine.head_goal tree in
            let hints = ProofStrategy.hints ?node wpo in
            autosearch#connect (Some (self#autosearch sequent));
            strategies#connect ~hints (Some self#strategies);
            (* configure tactics *)
            List.iter (fun (tactic : GuiTactic.tactic) ->
                let process = self#apply in
                let composer = self#compose in
                let browser = self#browse in
                tactic#select ~process ~composer ~browser ~tree sel
              ) tactics ;
            (* target selection feedback *)
            printer#highlight
              (if List.exists (fun tactics -> tactics#targeted) tactics
               then sel else Tactical.Empty)
          end ()

    method private update_scriptbar =
      match state with
      | Empty | Forking _ ->
        begin
          save_script#set_enabled false ;
          play_script#set_enabled false ;
        end
      | Proof proof | Composer(proof,_,_) | Browser(proof,_,_) ->
        begin
          let main = ProofEngine.main proof in
          let play = ProofSession.exists main in
          let save = not (ProofEngine.saved proof) in
          play_script#set_enabled play ;
          save_script#set_enabled save ;
        end

    method private update_pending kind proof n =
      match ProofEngine.current proof with
      | `Main | `Internal _ ->
        next#set_enabled false ;
        prev#set_enabled false ;
        if n = 1 then
          Pretty_utils.ksfprintf status#set_text "One %s Goal" kind
        else
          Pretty_utils.ksfprintf status#set_text "%d %s Goals" n kind
      | `Leaf(k,_) ->
        prev#set_enabled (0 < k) ;
        next#set_enabled (k+1 < n) ;
        if k = 0 && n = 1 then
          Pretty_utils.ksfprintf status#set_text
            "Last %s Goal" kind
        else
          Pretty_utils.ksfprintf status#set_text
            "%s Goal #%d /%d" kind (succ k) n

    method private update_statusbar =
      match state with
      | Empty ->
        begin
          icon#set_icon GuiProver.no_status ;
          next#set_enabled false ;
          prev#set_enabled false ;
          cancel#set_enabled false ;
          delete#set_enabled false ;
          forward#set_enabled false ;
          status#set_text "No Status" ;
          help#set_enabled false ;
        end
      | Proof proof | Forking(proof,_,_)
      | Composer(proof,_,_) | Browser(proof,_,_) ->
        begin
          let nofork = match state with Forking _ -> false | _ -> true in
          delete#set_enabled nofork ;
          help#set_enabled
            (match state with Proof _ -> not helpmode | _ -> false) ;
          match ProofEngine.status proof with
          | `Unproved ->
            icon#set_icon GuiProver.ko_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            cancel#set_enabled false ;
            forward#set_enabled false ;
            status#set_text "Non Proved Property" ;
          | `Invalid | `StillResist 0 ->
            icon#set_icon GuiProver.wg_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            cancel#set_enabled false ;
            forward#set_enabled false ;
            status#set_text "Invalid Smoke-test" ;
          | `Passed ->
            icon#set_icon GuiProver.smoke_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            cancel#set_enabled false ;
            forward#set_enabled false ;
            status#set_text "Passed Smoke Test" ;
          | `Proved ->
            icon#set_icon GuiProver.ok_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            cancel#set_enabled false ;
            forward#set_enabled false ;
            status#set_text "Proved Goal" ;
          | `Pending 0 ->
            icon#set_icon GuiProver.ok_status ;
            next#set_enabled false ;
            prev#set_enabled false ;
            forward#set_enabled false ;
            cancel#set_enabled nofork ;
            status#set_text "Proof Terminated" ;
          | `Pending n ->
            icon#set_icon GuiProver.ko_status ;
            forward#set_enabled nofork ;
            cancel#set_enabled nofork ;
            self#update_pending "Pending" proof n ;
          | `StillResist n ->
            icon#set_icon GuiProver.smoke_status ;
            forward#set_enabled nofork ;
            cancel#set_enabled nofork ;
            self#update_pending "Smoking" proof n ;
        end

    method private update_tacticbar =
      match state with
      | Empty | Forking _ ->
        self#update_provers None ;
        self#update_tactics None ;
      | Proof proof ->
        begin
          self#update_provers (Some (ProofEngine.head_goal proof)) ;
          let sequent = printer#sequent in
          let select = printer#selection in
          self#update_tactics (Some(proof,sequent,select)) ;
        end
      | Composer _ | Browser _ -> ()

    method private update_proofview =
      match state with
      | Empty -> text#clear
      | Proof _ when helpmode ->
        begin
          text#clear ;
          let callback () = self#quit_help in
          text#printf "@\n@{<bf>Available Tactics:@} %t@\n@\n"
            (printer#button ~title:"Close" ~callback) ;
          text#hrule ;
          let pp_item pp fmt tac =
            Format.fprintf fmt "[ @{<bf>%a@} ] @{<it>%s@}@\n"
              pp tac#title tac#descr in
          Pretty_utils.pp_items
            ~title:(fun tac -> tac#title)
            ~iter:Tactical.iter
            ~pp_item text#fmt ;
          text#hrule ;
        end
      | Proof proof ->
        on_proof_context proof
          begin fun () ->
            text#clear ;
            let main = ProofEngine.main proof in
            if ProofSession.exists main then
              text#printf
                (if ProofEngine.saved proof
                 then "%a (@{<green>saved@})@."
                 else "%a (@{<orange>modified@})@.")
                ProofSession.pp_script_for main
            else
              text#printf "%a (@{<blue>not created@})@."
                ProofSession.pp_script_for main ;
            text#hrule ;
            scripter#tree proof ;
            text#hrule ;
            text#printf "@\n%a@." printer#pp_goal (ProofEngine.head_goal proof) ;
            text#printf "@{<bf>Goal id:@}  %s@." main.po_gid ;
            text#printf "@{<bf>Short id:@} %s@." main.po_sid ;
            text#hrule ;
            scripter#status proof ;
          end ()
      | Composer(proof,cc,tgt) ->
        on_proof_context proof
          begin fun () ->
            text#clear ;
            let quit () =
              state <- Proof proof ;
              printer#restore ~focus:`Select tgt ;
              self#update in
            text#printf "%t@." (composer#print cc ~quit) ;
            text#hrule ;
            text#printf "@\n%a@." printer#pp_goal (ProofEngine.head_goal proof) ;
          end ()
      | Browser(proof,cc,tgt) ->
        on_proof_context proof
          begin fun () ->
            text#clear ;
            let quit () =
              state <- Proof proof ;
              printer#restore ~focus:`Select tgt ;
              self#update in
            text#printf "%t@." (browser#print cc ~quit) ;
            text#hrule ;
            text#printf "@\n%a@." printer#pp_goal (ProofEngine.head_goal proof) ;
          end ()
      | Forking _ -> ()

    method update =
      begin
        self#update_statusbar ;
        self#update_proofview ;
        self#update_scriptbar ;
        self#update_tacticbar ;
      end

    (* ---------------------------------------------------------------------- *)
    (* --- Splitter                                                       --- *)
    (* ---------------------------------------------------------------------- *)

    method private commit () =
      match state with
      | Empty | Proof _ | Composer _ | Browser _ -> ()
      | Forking(proof,fork,pool) ->
        let n = Task.size pool in
        if n = 0 then
          begin
            ignore (ProofEngine.commit fork) ;
            ProofEngine.validate proof ;
            ProofEngine.forward proof ;
            state <- Proof proof ;
            printer#reset ;
            self#update ;
          end
        else
          Wutil.later self#commit

    method private schedule pool provers goal =
      Prover.spawn goal
        ~delayed:true
        ~result:
          begin fun wpo prv res ->
            text#printf "[%a] %a : %a@."
              VCS.pp_prover prv Wpo.pp_title wpo VCS.pp_result res
          end
        ~success:(fun _ _ -> Wutil.later self#commit)
        ~pool (List.map (fun dp -> VCS.Batch , dp) provers)

    method private fork proof fork =
      Wutil.later
        begin fun () ->
          let provers = self#provers in
          let pool = Task.pool () in
          ProofEngine.iter (self#schedule pool provers) fork ;
          let server = ProverTask.server () in
          state <- Forking(proof,fork,pool) ;
          Task.launch server ;
          printer#reset ;
          text#clear ;
          text#printf "Tactic %a@." ProofEngine.pretty fork ;
          text#printf "%d sub-goals generated.@." (Task.size pool) ;
          text#printf "Computing...@." ;
          self#update ;
        end

    method private apply tactic selection process =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
        Wutil.later
          begin fun () ->
            try
              let tactic = ProofScript.jtactic tactic selection in
              let anchor = ProofEngine.anchor proof () in
              self#fork proof (ProofEngine.fork proof ~anchor tactic process)
            with Exit | Not_found | Invalid_argument _ ->
              text#printf "Application of tactic '%s' failed." tactic#title
          end

    method private search proof = function
      | None -> text#printf "No tactic found.@\n"
      | Some fork -> self#fork proof fork

    method private autosearch sequent ~depth ~width auto =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
        Wutil.later
          begin fun () ->
            if depth <= 1 then
              let fork = ProverSearch.search proof ~sequent auto in
              self#search proof fork
            else
              begin
                let provers = List.map (fun e -> e#prover) provers in
                ProverScript.search
                  ~depth ~width ~auto
                  ~provers
                  ~result:
                    (fun wpo prv res ->
                       text#printf "[%a] %a : %a@."
                         VCS.pp_prover prv
                         Wpo.pp_title wpo
                         VCS.pp_result res)
                  ~success:
                    (fun _ _ ->
                       ProofEngine.forward proof ;
                       self#update ;
                       text#printf "Strategies Applied." )
                  proof (ProofEngine.anchor proof ()) ;
                let server = ProverTask.server () in
                Task.launch server
              end
          end

    method private strategies ~depth strategy =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
        Wutil.later
          begin fun () ->
            ProverScript.explore
              ~depth ?strategy
              ~result:
                (fun wpo prv res ->
                   text#printf "[%a] %a : %a@."
                     VCS.pp_prover prv
                     Wpo.pp_title wpo
                     VCS.pp_result res)
              ~success:
                (fun _ _ ->
                   ProofEngine.forward proof ;
                   self#update ;
                   text#printf "Strategie(s) Explored." )
              proof (ProofEngine.anchor proof ()) ;
            let server = ProverTask.server () in
            Task.launch server
          end

    method private backtrack node =
      match state with
      | Empty | Forking _ | Composer _ | Browser _ -> ()
      | Proof proof ->
        begin
          ProofEngine.goto proof (`Node node) ;
          let fork =
            ProverSearch.backtrack proof ~anchor:node ~loop:true () in
          self#search proof fork
        end

  end
