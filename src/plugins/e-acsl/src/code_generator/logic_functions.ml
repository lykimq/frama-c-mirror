(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2025                                               *)
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
open Analyses_types
open Analyses_datatype
module Error = Translation_error

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

let predicate_to_exp_ref
  : (adata:Assert.t ->
     kernel_function ->
     Env.t ->
     predicate ->
     exp * Assert.t * Env.t) ref
  =
  ref (fun ~adata:_ _kf _env _p ->
      Extlib.mk_labeled_fun "predicate_to_exp_ref")

let term_to_exp_ref
  : (adata:Assert.t ->
     kernel_function ->
     Env.t ->
     term ->
     exp * Assert.t * Env.t) ref
  =
  ref (fun ~adata:_ _kf _env _t -> Extlib.mk_labeled_fun "term_to_exp_ref")

(*****************************************************************************)
(************************** Auxiliary  functions* ****************************)
(*****************************************************************************)

(* @return true iff the result of the function is provided by reference as the
   first extra argument at each call *)
let result_as_extra_argument typ =
  let is_composite typ =
    match Cil.unrollTypeNode typ with
    | TComp _ | TPtr _ | TArray _ -> true
    | TInt _ | TVoid  | TFloat _ | TFun _ | TNamed _ | TEnum _
    | TBuiltin_va_list -> false
  in
  Gmp_types.is_t typ || is_composite typ

(*****************************************************************************)
(****************** Generation of function bodies ****************************)
(*****************************************************************************)

(* Generate the block of code containing the statement assigning [e] to [ret_vi]
   (the result). *)
let generate_return_block ~loc env ret_vi e = match e.enode with
  | Lval (Var _, NoOffset) ->
    (* the returned value is a variable: Cil invariant preserved;
       no need of [ret_vi] *)
    let return_retres = Cil.mkStmt ~valid_sid:true (Return (Some e, loc)) in
    let b, env =
      Env.pop_and_get env return_retres ~global_clear:false Env.After
    in
    b.blocals <- b.blocals;
    b.bscoping <- true;
    b, env
  | _ ->
    (* the returned value is _not_ a variable: restore the invariant *)
    let init = AssignInit (SingleInit e) in
    let set =
      Cil.mkStmtOneInstr ~valid_sid:true (Local_init (ret_vi, init, loc))
    in
    let return =
      Cil.mkStmt ~valid_sid:true (Return (Some (Cil.evar ~loc ret_vi), loc))
    in
    let b, env = Env.pop_and_get env set ~global_clear:false Env.Middle in
    ret_vi.vdefined <- true;
    b.blocals <- ret_vi :: b.blocals;
    b.bstmts <- b.bstmts @ [ return ];
    b.bscoping <- true;
    b, env

(* Generate the function's body for predicates. *)
let pred_to_block ~loc kf env ret_vi p =
  let e, _, env = !predicate_to_exp_ref ~adata:Assert.no_data kf env p in
  (* for predicate, since the result is either 0 or 1, return it directly (it
     cannot be provided as extra argument *)
  generate_return_block ~loc env ret_vi e

(* Generate the function's body for terms. *)
let term_to_block ~loc kf env ret_ty ret_vi t =
  let e, _, env = !term_to_exp_ref ~adata:Assert.no_data kf env t in
  if Cil.isVoidType ret_ty then
    (* if the function's result is a GMP, it is the first parameter of the
       function (by reference). *)
    let set =
      let lv_star_ret = Cil.mkMem ~addr:(Cil.evar ~loc ret_vi) ~off:NoOffset in
      let star_ret = Smart_exp.lval ~loc lv_star_ret in
      Gmp.init_set ~loc lv_star_ret star_ret e
    in
    let return_void = Cil.mkStmt ~valid_sid:true (Return (None, loc)) in
    let b, env = Env.pop_and_get env set ~global_clear:false Env.Middle in
    b.bstmts <- b.bstmts @ [ return_void ];
    b.bscoping <- true;
    b, env
  else
    generate_return_block ~loc env ret_vi e

let generate_body ~loc kf env ret_ty ret_vi = function
  | LBterm t -> term_to_block ~loc kf env ret_ty ret_vi t
  | LBpred p -> pred_to_block ~loc kf env ret_vi p
  | LBnone |LBreads _ | LBinductive _ -> assert false

(* Generate a kernel function from a given logic info [li] *)
let generate_kf ~loc fname env params_ty ret_ty params_ival li =
  (* build the formal parameters *)
  let params, params_ty_vi =
    List.fold_right2
      (fun lvi pty (params, params_ty) ->
         let ty = match pty with
           | Gmpz ->
             (* GMP's integer are arrays: consider them as pointers in function's
                parameters *)
             Gmp_types.Z.t_as_ptr ()
           | C_integer _ when Options.Gmp_only.get () -> Gmp_types.Z.t_as_ptr ()
           | C_integer ik -> Cil_const.mk_tint ik
           | C_float _ when Options.Gmp_only.get () -> Gmp_types.Q.t_as_ptr ()
           | C_float fk -> Cil_const.mk_tfloat fk
           (* for the time being, no reals but rationals instead *)
           | Rational -> Gmp_types.Q.t ()
           | Real -> Error.not_yet "real number"
           | Nan -> Typing.typ_of_lty lvi.lv_type
         in
         (* build the formals: cannot use [Cil.makeFormal] since the function
            does not exist yet *)
         let vi = Cil.makeVarinfo false true lvi.lv_name ty in
         vi :: params, (lvi.lv_name, ty, []) :: params_ty)
      li.l_profile
      params_ty
      ([], [])
  in
  (* build the varinfo storing the result *)
  let res_as_extra_arg = result_as_extra_argument ret_ty in
  let is_gmp = Gmp_types.is_t ret_ty in
  let ret_vi, ret_ty, params_with_ret, params_ty_with_ret =
    let vname = "__retres" in
    if res_as_extra_arg then
      let ret_ty_ptr = Cil_const.mk_tptr ret_ty (* call by reference *) in
      let vname = vname ^ "_arg" in
      let vi = Cil.makeVarinfo false true vname ret_ty_ptr in
      vi, Cil_const.voidType, vi :: params, (vname, ret_ty_ptr, []) :: params_ty_vi
    else
      Cil.makeVarinfo false false vname ret_ty, ret_ty, params, params_ty_vi
  in
  (* build the function's varinfo *)
  let vi =
    Cil.makeGlobalVar
      fname
      (Cil_const.mk_tfun
         ~tattr:li.l_var_info.lv_attr
         ret_ty
         (Some params_ty_with_ret)
         false)
  in
  vi.vdefined <- true;
  (* create the fundec *)
  let fundec =
    { svar = vi;
      sformals = params_with_ret;
      slocals = []; (* filled later to break mutual dependencies between
                       creating this list and creating the kf *)
      smaxid = 0;
      sbody = Cil.mkBlock []; (* filled later; same as above *)
      smaxstmtid = None;
      sallstmts = [];
      sspec = Cil.empty_funspec () }
  in
  Cil.setMaxId fundec;
  let spec = Cil.empty_funspec () in
  (* register the definition *)
  Globals.Functions.replace_by_definition spec fundec loc;
  (* create the kernel function itself *)
  let kf = Globals.Functions.get fundec.svar in
  Annotations.register_funspec ~emitter:Options.emitter kf;
  (* closure generating the function's body.
     Delay its generation after filling the memoization table (for termination
     of recursive function calls) *)
  let gen_body () =
    let env = (* cannot use bindings from other functions (the use site) *)
      Env.Logic_binding.clear env
    in
    let env = Env.push env in
    (* fill the typing environment with the function's parameters
       before generating the code (code generation invokes typing) *)
    let env = Env.Logic_env.push_new env params_ival in
    let env =
      List.fold_left2 (Env.Logic_binding.add_binding) env li.l_profile params
    in
    let assigns_from =
      try Some (Assigns.get_assigns_from ~loc env li.l_profile li.l_var_info)
      with Assigns.NoAssigns -> None
    in
    let assigned_var =
      Logic_const.new_identified_term
        (if res_as_extra_arg then
           Assigns.get_assigned_var ~loc ~is_gmp ret_vi
         else
           Logic_const.tresult fundec.svar.vtype)
    in
    begin
      match assigns_from with
      | None -> ()
      | Some assigns_from ->
        let assigns_from =
          List.map
            (fun e ->
               Logic_const.new_identified_term
                 (Logic_utils.expr_to_term e))
            assigns_from
        in
        Annotations.add_assigns
          ~keep_empty:false
          Options.emitter
          ~behavior:Cil.default_behavior_name
          kf
          (Writes [ assigned_var , From assigns_from]);
    end;
    let b, env = generate_body ~loc kf env ret_ty ret_vi li.l_body in
    fundec.sbody <- b;
    (* add the generated variables in the necessary lists *)
    (* TODO: factorized the code below that add the generated vars with method
       [add_generated_variables_in_function] in the main visitor *)
    let vars =
      let l = Env.get_generated_variables env in
      if ret_vi.vdefined then
        (ret_vi, Env.LFunction kf) :: l
      else
        l
    in
    let locals, blocks =
      List.fold_left
        (fun (local_vars, block_vars as acc) (v, scope) -> match scope with
           | Env.LFunction kf' when Kernel_function.equal kf kf' ->
             v :: local_vars, block_vars
           | Env.LLocal_block kf' when Kernel_function.equal kf kf' ->
             v :: local_vars, block_vars
           | _ -> acc)
        (fundec.slocals, fundec.sbody.blocals)
        vars
    in
    fundec.slocals <- locals;
    fundec.sbody.blocals <- blocks;
    List.iter
      (fun lvi ->
         ignore (Env.Logic_binding.remove env lvi))
      li.l_profile
  in
  vi, kf, gen_body

(***************************** Memoization ********************************)

(* This module memoizes for each translated logic_info the corresponding
   generated kernel functions. Each generated kernel function is associated
   with its signature (profile + return type), because sometimes multiple
   translated versions of a single logic_info are required, depending on the
   calling context. *)
module Gen_functions : sig

  type fgen = (kernel_function, exn) result

  val clear : unit -> unit

  val fold_sorted :
    (Profile.t * typ -> fgen -> 'a -> 'a) -> 'a -> 'a

  val memo :
    (typ ->
     Profile.t ->
     logic_info ->
     varinfo * kernel_function * (unit -> unit)) ->
    Profile.t * typ ->
    logic_info ->
    varinfo * (unit -> unit)

  val replace : logic_info -> Profile.t * typ -> fgen -> unit

  val kernel_functions_of_logic_info : logic_info -> varinfo list

end = struct

  module Memo_tbl = Logic_info.Hashtbl

  (* The signature of generated functions depends on the profile of the logic
     function as well as its return type. In certain contexts the return type
     might be GMP; in these cases an additional result parameter is required. *)
  module Profile_and_return_type =
    Datatype.Pair_with_collections
      (Profile)
      (Typ) (* return type *)

  module Signatures = Profile_and_return_type.Hashtbl

  type fgen = (kernel_function, exn) result

  (* For each logic_info, memoize for each encountered profile and return type
     the generated C function *)
  let memo_tbl : fgen Signatures.t Memo_tbl.t = Memo_tbl.create 7

  let clear () = Memo_tbl.clear memo_tbl

  let fold_sorted f =
    Memo_tbl.fold_sorted
      (fun _ -> Signatures.fold_sorted f)
      memo_tbl

  let memo f (profile, ret_ty) li =
    let gen tbl =
      let vi, kf, gen_body = f ret_ty profile li in
      Signatures.add tbl (profile, ret_ty) (Ok kf);
      vi, gen_body
    in
    (* memoize the function's varinfo *)
    try
      let h = Memo_tbl.find memo_tbl li in
      try
        let kf = Signatures.find h (profile, ret_ty) in
        let kf = match kf with
          | Ok kf -> kf
          | Error exn -> raise exn
        in
        Kernel_function.get_vi kf,
        (fun () -> ()) (* body generation already planified *)
      with Not_found -> gen h
    with Not_found ->
      let h = Signatures.create 7 in
      Memo_tbl.add memo_tbl li h;
      gen h

  let replace li (profile, ret_ty) fgen =
    let h = Memo_tbl.find memo_tbl li in
    Signatures.replace h (profile, ret_ty) fgen

  let kernel_functions_of_logic_info li =
    try
      let params = Memo_tbl.find memo_tbl li in
      let add_fundecl _ kf acc =
        match kf with
        | Ok kf -> Kernel_function.get_vi kf :: acc
        | Error _ -> acc (* Exception has already been signaled *)
      in
      Signatures.fold_sorted add_fundecl params []
    with Not_found -> []
end


let reset () = Gen_functions.clear ()

let add_generated_functions_to_file file =
  let rec decls_of_li ?(generated = false) ?(loc = Location.unknown) li =
    let dependencies =
      List.concat_map (decls_of_li ~generated:true ~loc)
        (Logic_normalizer.Logic_infos.generated_of li)
    in
    let add_generated_annot =
      if generated
      then fun decls -> GAnnot(Dfun_or_pred(li, loc), loc) :: decls
      else fun decls -> decls
    in
    let kfs = Gen_functions.kernel_functions_of_logic_info li in
    let decls =
      List.map (fun kf -> GFunDecl (Cil.empty_funspec (), kf, loc)) kfs
    in
    dependencies @ add_generated_annot @@ decls
  in
  let generated_decls_of_global = function
    | GAnnot(Dfun_or_pred(li, loc), _) -> decls_of_li ~loc li
    | _ -> []
  in
  (* add declarations of generated functions just below the logic
     function/predicate they belong to;
     the (potentially mutually recursive) definitions follow later *)
  let all_decls =
    List.concat_map (fun g -> g :: generated_decls_of_global g) file.globals
  in
  let add_fundef _ kf acc =
    let get_fundef kf =
      try Kernel_function.get_definition kf
      with Kernel_function.No_Definition -> assert false
    in
    match kf with
    | Ok kf ->
      Globals.Functions.register kf;
      GFun (get_fundef kf, Location.unknown) :: acc
    | Error _ -> acc
  in
  (* append the generated function definitions at the end; as the declarations
     are all already above they can be mutually recursive. *)
  let new_globals =
    all_decls @ List.rev @@ Gen_functions.fold_sorted add_fundef []
  in
  file.globals <- new_globals

let ret_ty_of_tapp ~env = function
  | Some tapp ->
    let logic_env = Env.Logic_env.get env in
    Typing.get_typ ~logic_env tapp
  | None  -> Cil_const.intType

(* Generate (and memoize) the function body and create the calls to the
   generated functions. *)
let function_to_exp ~loc ?tapp fname env kf li params_ty profile args =
  let ret_ty = ret_ty_of_tapp ~env tapp in
  (* memoize the function's varinfo *)
  let fvi, gen_body =
    Gen_functions.memo (generate_kf fname ~loc env params_ty) (profile, ret_ty) li
  in
  (* the generation of the function body must be performed after memoizing the
     kernel function in order to handle recursive calls in finite time :-) *)
  gen_body ();
  (* create the function call for the tapp *)
  let mkcall vi =
    let mk_args types args =
      match types.tnode (* generated by E-ACSL: no need to unroll *) with
      | TFun(_, Some params, _) ->
        (* additional casts are necessary whenever the argument is GMP and the
           parameter is a (small) integralType: after handling the context in
           [Translate] through [add_cast], the GMP has been translated into a
           [long] (that is what provided the GMP API). This [long] must now be
           translated to the parameter's type. It cannot be done before since
           the exact type of the parameter is only computed when the function is
           generated *)
        List.map2
          (fun (_, newt, _) e -> Cil.mkCast ~force:false ~newt e)
          params
          args
      | _ -> assert false
    in
    if result_as_extra_argument ret_ty then
      let args = mk_args fvi.vtype (Cil.mkAddrOf ~loc (Cil.var vi) :: args) in
      Call(None, Cil.evar fvi, args, loc)
    else
      let args = mk_args fvi.vtype args in
      Call(Some (Cil.var vi), Cil.evar fvi, args, loc)
  in
  (* generate the varinfo storing the result of the call *)
  Env.new_var
    ~loc
    ~name:li.l_var_info.lv_name
    env
    kf
    tapp
    ret_ty
    (fun vi _ -> [ Cil.mkStmtOneInstr ~valid_sid:true (mkcall vi) ])

let raise_errors = function
  | LBnone ->
    Error.not_yet
      "logic functions or predicates with no definition nor reads clause"
  | LBreads _ ->
    Error.not_yet "logic functions or predicates performing read accesses"
  | LBinductive _ -> Error.not_yet "inductive logic functions"
  | LBterm _
  | LBpred _ -> ()

let app_to_exp ~adata ~loc ?tapp kf env ?eargs li targs =
  let fname = li.l_var_info.lv_name in
  (* build the varinfo (as an expression) which stores the result of the
     function call. *)
  let _, e, adata, env =
    if Builtins.mem li.l_var_info.lv_name then
      (* E-ACSL built-in function call *)
      let args, adata, env =
        match eargs with
        | None ->
          List.fold_right
            (fun targ (l, adata, env) ->
               let e, adata, env = !term_to_exp_ref ~adata kf env targ in
               e :: l, adata, env)
            targs
            ([], adata, env)
        | Some eargs ->
          if List.compare_lengths targs eargs <> 0 then
            Options.fatal
              "[Tapp] unexpected number of arguments when calling %s"
              fname;
          eargs, adata, env
      in
      let vi, e, env =
        Env.new_var
          ~loc
          ~name:(fname ^ "_app")
          env
          kf
          tapp
          (Misc.cty (Option.get li.l_type))
          (fun vi _ ->
             [ Smart_stmt.rtl_call ~loc
                 ~result:(Cil.var vi)
                 ~prefix:""
                 fname
                 args ])
      in
      vi, e, adata, env
    else
      begin
        raise_errors li.l_body;
        (* build the arguments and compute the integer_ty of the parameters *)
        let params_ty, params_ival, args, adata, env =
          let eargs, adata, env =
            match eargs with
            | None ->
              List.fold_right
                (fun targ (eargs, adata, env) ->
                   let e, adata, env = !term_to_exp_ref ~adata kf env targ in
                   e :: eargs, adata, env)
                targs
                ([], adata, env)
            | Some eargs ->
              if List.compare_lengths targs eargs <> 0 then
                Options.fatal
                  "[Tapp] unexpected number of arguments when calling %s"
                  fname;
              eargs, adata, env
          in
          try
            List.fold_right2
              (fun targ earg (params_ty, params_ival, args, adata, env) ->
                 let logic_env = Env.Logic_env.get env in
                 let param_ty = Typing.get_number_ty ~logic_env targ in
                 let param_ival = Interval.get ~logic_env targ in
                 let e, env =
                   try
                     let ty = Typing.typ_of_number_ty param_ty in
                     Typed_number.add_cast
                       ~loc
                       ~name:(Varname.of_exp earg)
                       env
                       kf
                       (Some ty)
                       Analyses_types.C_number
                       (Some targ)
                       earg
                   with Typing.Not_a_number ->
                     earg, env
                 in
                 param_ty :: params_ty,
                 param_ival :: params_ival,
                 e :: args,
                 adata,
                 env)
              targs eargs
              ([], [], [], adata ,env)
          with Invalid_argument _ ->
            Options.fatal
              "[Tapp] unexpected number of arguments when calling %s"
              fname
        in
        let gen_fname =
          Varname.get ~scope:Global (Functions.RTL.mk_gen_name fname)
        in
        let profile = Profile.make li.l_profile params_ival in
        let vi, e, env =
          try
            function_to_exp ~loc ?tapp gen_fname env kf li params_ty profile args
          with exn ->
            (* Those accesses always succeed *)
            let ret_ty = ret_ty_of_tapp ~env tapp in
            Gen_functions.replace li (profile, ret_ty) (Error exn);
            raise exn
        in
        vi, e, adata, env
      end
  in
  e, adata, env

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
