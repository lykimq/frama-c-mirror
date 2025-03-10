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

(* Detailed description of transformations implemented in this file is
   presented in Sections 2 and 3 of the RV'17 paper "Runtime Detection of
   Temporal Memory Errors" by K. Vorobyov, N. Kosmatov, J Signoles and
   A. Jakobsson. *)

module RTL = Functions.RTL
module Libc = Functions.Libc
module Error = Translation_error
open Cil_types
open Cil_datatype

let generate = ref false
let enable param = generate := param
let is_enabled () = !generate

(* ************************************************************************** *)
(* Types {{{ *)
(* ************************************************************************** *)

(* Type of identifier tracked by a LHS referent number *)
type flow =
  | Direct (* take origin number of RHS *)
  | Indirect (* take referent number of RHS *)
  | Copy (* Copy shadow from RHS to LHS *)
(* }}} *)

(*  ************************************************************************* *)
(* Generate analysis function calls {{{ *)
(* ************************************************************************** *)

module Mk: sig
  (* Generate either
     - [store_nblock(lhs, rhs)], or
     - [store_nreferent(lhs, rhs)]
       function call based on the value of [flow] *)
  val store_reference: loc:location -> flow -> lval -> exp -> stmt

  (* Generate a [save_*_parameter] call *)
  val save_param: loc:location -> flow -> exp -> int -> stmt

  (* Generate [pull_parameter] call *)
  val pull_param: loc:location -> varinfo -> int -> stmt

  (* Generate [(save|pull)_return(lhs, param_no)] call *)
  val handle_return_referent: save:bool -> loc:location -> exp -> stmt

  (* Generate [reset_return()] call *)
  val reset_return_referent: loc:location -> stmt

  (* Generate [memcpy(lhs, rhs, size)] function call assuming that [lhs = rhs]
     represents an assignment of struct to a struct, that is, both sides are
     left values and we need to use addressof for both sides *)
  val temporal_memcpy_struct: loc:location -> lval -> exp -> stmt
end = struct

  let store_reference ~loc flow lhs rhs =
    let prefix = RTL.temporal_prefix in
    let fname = match flow with
      | Direct -> "store_nblock"
      | Indirect -> "store_nreferent"
      | Copy -> Options.fatal "Copy flow type in store_reference"
    in
    Smart_stmt.rtl_call ~loc ~prefix fname [ Cil.mkAddrOf ~loc lhs; rhs ]

  let save_param ~loc flow lhs pos =
    let infix = match flow with
      | Direct -> "nblock"
      | Indirect -> "nreferent"
      | Copy -> "copy"
    in
    let prefix = RTL.temporal_prefix in
    let fname = "save_" ^ infix ^ "_parameter" in
    Smart_stmt.rtl_call ~loc ~prefix fname [ lhs ; Cil.integer ~loc pos ]

  let pull_param ~loc vi pos =
    let prefix = RTL.temporal_prefix in
    let fname = "pull_parameter" in
    let exp = Cil.mkAddrOfVi vi in
    let sz = Cil.kinteger ~loc IULong (Cil.bytesSizeOf vi.vtype) in
    Smart_stmt.rtl_call ~loc ~prefix fname [ exp ; Cil.integer ~loc pos ; sz ]

  let handle_return_referent ~save ~loc lhs =
    let prefix = RTL.temporal_prefix in
    let fname = match save with
      | true -> "save_return"
      | false -> "pull_return"
    in
    (* TODO: Returning structs is unsupported so far *)
    (match Cil.(typeOf lhs).tnode with
     | TPtr _ -> ()
     | _ -> Error.not_yet "Struct in return");
    Smart_stmt.rtl_call ~loc ~prefix fname [ lhs ]

  let reset_return_referent ~loc =
    let prefix = RTL.temporal_prefix in
    Smart_stmt.rtl_call ~loc ~prefix "reset_return" []

  let temporal_memcpy_struct ~loc lhs rhs =
    let prefix = RTL.temporal_prefix in
    let fname  = "memcpy" in
    let size = Cil.sizeOf ~loc (Cil.typeOfLval lhs) in
    Smart_stmt.rtl_call ~loc ~prefix fname [ Cil.mkAddrOf ~loc lhs; rhs; size ]
end
(* }}} *)

(* ************************************************************************** *)
(* Handle assignments {{{ *)
(* ************************************************************************** *)

(* Given an lvalue [lhs] representing LHS of an assignment, and an expression
   [rhs] representing its RHS compute triple (l,r,f), such that:
   - lval [l] and exp [r] are addresses of a pointer and a memory block, and
   - flow [f] indicates how to update the meta-data of [l] using information
     stored by [r]. The values of [f] indicate the following
     + Direct - referent number of [l] is assigned the referent number of [r]
     + Indirect - referent number of [l] is assigned the origin number of [r]
     + Copy - metadata of [r] is copied to metadata of [l] *)
let assign ?(ltype) lhs rhs loc =
  (* Do not use [Extlib.opt_conv] here, application of the [None] part should
     not be evaluated at this point, as otherwise it will lead to an exception
     via [Cil.typeOfLval] later *)
  let ltype = match ltype with
    | Some l -> l
    | None -> Cil.typeOfLval lhs
  in
  match Cil.unrollTypeNode ltype with
  | TPtr _ ->
    let base = Misc.ptr_base ~loc:rhs.eloc rhs in
    let rhs, flow =
      (match (Cil.stripCasts base).enode with
       | AddrOf _
       | StartOf _ -> rhs, Direct
       (* Unary operator describes !, ~ or -: treat it same as Const since
          it implies integer or logical operations. This case is rare but
          happens: for instance in Gap SPEC CPU benchmark the returned pointer
          is assigned -1 (for whatever bizarre reason) *)
       | Const _ | UnOp _ -> base, Direct
       (* Special case for literal strings which E-ACSL rewrites into
          global variables: take the origin number of a string *)
       | Lval(Var vi, _) when RTL.is_generated_name vi.vname ->
         base, Direct
       (* Lvalue of a pointer type can be a cast of an integral type, for
          instance for the case when address is taken by value (shown via the
          following example).
            uintptr_t addr = ...;
            char *p = (char* )addr;
          If this is the case then the analysis takes the value of a variable.
       *)
       | Lval lv ->
         if Cil.isPointerType (Cil.unrollType (Cil.typeOfLval lv)) then
           Cil.mkAddrOf ~loc lv, Indirect
         else
           rhs, Direct
       (* Binary operation which yields an integer (or FP) type.
          Since LHS is of pointer type we assume that the whole integer
          expression computes to an address for which there is no
          outer container, so the only thing to do is to take origin number *)
       | BinOp(op, _, _, _) ->
         (* At this point [ptr_index] should have split pointer arithmetic into
            base pointer and index so there should be no pointer arithmetic
            operations there. The following bit is to make sure of it. *)
         (match op with
          | MinusPI | PlusPI -> assert false
          | _ -> ());
         base, Direct
       | _ -> assert false)
    in Some (lhs, rhs, flow)
  | TNamed _ -> assert false
  | TInt _ | TFloat _ | TEnum _ -> None
  | TComp _ ->
    let rhs = match rhs.enode with
      | AddrOf _ -> rhs
      | Lval lv -> Cil.mkAddrOf ~loc lv
      | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _
      | UnOp _ | BinOp _ | CastE _ | StartOf _ ->
        Options.abort "unsupported RHS %a" Printer.pp_exp rhs
    in Some (lhs, rhs, Copy)
  (* va_list is a builtin type, we assume it has no pointers here and treat
     it as a "big" integer rather than a struct *)
  | TBuiltin_va_list -> None
  | TArray _ -> Some (lhs, rhs, Direct)
  (* void type should not happen as we are dealing with assignments *)
  | TVoid -> Options.fatal "Void type in assignment"
  | TFun _ -> Options.fatal "TFun type in assignment"

(* Generate a statement tracking temporal metadata associated with assignment
   [lhs] = [rhs], where lhs is a left value and [rhs] is an expression. *)
let mk_stmt_from_assign loc lhs rhs =
  let fn (lhs, rhs, flow) = match flow with
    | Direct | Indirect -> Mk.store_reference ~loc flow lhs rhs
    | Copy -> Mk.temporal_memcpy_struct ~loc lhs rhs
  in
  Option.map fn (assign lhs rhs loc)
(* }}} *)

(* ************************************************************************** *)
(* Handle Set instructions {{{ *)
(* ************************************************************************** *)

(* Top-level handler for Set instructions *)
let set_instr ?(post=false) loc lhs rhs env kf =
  if Memory_tracking.must_monitor_lval ~kf lhs then
    Option.fold
      ~some:(fun stmt -> Env.add_stmt ~post env stmt)
      ~none:env
      (mk_stmt_from_assign loc lhs rhs)
  else
    env
(* }}} *)

(* ************************************************************************** *)
(* Handle Call instructions {{{ *)
(* ************************************************************************** *)

module Function_call: sig
  (* Top-level handler for Call instructions *)
  val instr:
    lval option -> exp -> exp list -> location -> Env.t -> kernel_function ->
    Env.t
end = struct

  (* Track function arguments: export referents of arguments to a global
     structure so they can be retrieved once that function is called *)
  let save_params loc args env kf =
    let (env, _) = List.fold_left
        (fun (env, index) param ->
           let lv = Mem(param), NoOffset in
           let ltype = Cil.typeOf param in
           let vals = assign ~ltype lv param loc in
           Option.fold
             ~some:(fun (_, rhs, flow) ->
                 let env =
                   if Memory_tracking.must_monitor_exp ~kf param then
                     let stmt = Mk.save_param ~loc flow rhs index in
                     Env.add_stmt ~post:false env stmt
                   else env
                 in
                 (env, index+1))
             ~none:(env, index+1)
             vals)
        (env, 0)
        args
    in env

  (* Update local environment with a statement tracking temporal metadata
     associated with assignment [ret] = [func(args)]. *)
  let call_with_ret ?(alloc=false) loc ret env =
    let rhs = Smart_exp.lval ~loc ret in
    let vals = assign ret rhs loc in
    (* Track referent numbers of assignments via function calls.
       Library functions (i.e., with no source code available) that return
       values are considered to be functions that allocate memory. They are
       considered so because they need to be handled exactly as memory
       allocating functions, that is, the referent of the returned pointer is
       assigned the origin number associated with the return value. For
       instance, for some [p = call();] [store_nblock( *p,..)] is appended.
       Note that for this we need [Direct] flow and also dereference the
       pointer to get its number. This is done in the following statement
       (where variable [alloc] indicates whether a function is a
       memory-allocating function or not).

       Alternatively, if a function does not allocate memory and its body has
       been instrumented, then information about referent numbers should be
       stored in the internal data structure and it is retrieved using
       [pull_return] added via a call to [Mk.handle_return_referent] *)
    Option.fold
      ~some:(fun (lhs, rhs, flow) ->
          let flow, rhs = match flow with
            | Indirect when alloc -> Direct, (Smart_exp.deref ~loc rhs)
            | _ -> flow, rhs
          in
          let stmt =
            if alloc then
              Mk.store_reference ~loc flow lhs rhs
            else
              Mk.handle_return_referent ~save:false ~loc (Cil.mkAddrOf ~loc lhs)
          in
          Env.add_stmt ~post:true env stmt)
      ~none:env
      vals

  (* Update local environment with a statement tracking temporal metadata
     associated with memcpy/memset call *)
  let call_memxxx loc args fexp env =
    if Libc.is_memcpy fexp || Libc.is_memset fexp then
      let prefix = RTL.temporal_prefix in
      let name = match fexp.enode with
        | Lval(Var vi, _) -> vi.vname
        | _ -> Options.fatal "[Temporal.call_memxxx] not a left-value"
      in
      let stmt =
        Smart_stmt.rtl_call ~loc ~prefix name args
      in
      Env.add_stmt ~post:false env stmt
    else
      env

  let instr ret fexp args loc env kf =
    (* Add function calls to reset_parameters and reset_return before each
       function call regardless. They are not really required, as if the
       instrumentation is correct then the right parameters will be saved
       and the right parameter will be pulled at runtime. In practice, however,
       it makes sense to make this somewhat-debug-level-call. In production mode
       the implementation of the function should be empty and compiler should
       be able to optimize that code out. *)
    let stmt =
      let prefix = RTL.temporal_prefix in
      let name = "reset_parameters" in
      Smart_stmt.rtl_call ~loc ~prefix name []
    in
    let env = Env.add_stmt ~post:false env stmt in
    let stmt = Mk.reset_return_referent ~loc in
    let env = Env.add_stmt ~post:false env stmt in
    (* Push parameters with either a call to a function pointer or a function
        definition otherwise there is no point. *)
    let has_def = Functions.has_fundef fexp in
    let env =
      if Cil.isFunctionType (Cil.typeOf fexp) || has_def then
        save_params loc args env kf
      else
        env
    in
    (* Handle special cases of memcpy/memset *)
    let env = call_memxxx loc args fexp env in
    (* Memory allocating functions have no definitions so below expression
       should capture them *)
    let alloc = not has_def in
    Option.fold
      ~some:(fun lhs ->
          if Memory_tracking.must_monitor_lval ~kf lhs then
            call_with_ret ~alloc loc lhs env
          else env)
      ~none:env
      ret
end
(* }}} *)

(* ************************************************************************** *)
(* Handle Local_init instructions {{{ *)
(* ************************************************************************** *)
module Local_init: sig
  (* Top-level handler for Local_init instructions *)
  val instr:
    varinfo -> local_init -> location -> Env.t -> kernel_function -> Env.t
end = struct

  let rec handle_init offset loc vi init env kf = match init with
    | SingleInit exp ->
      set_instr ~post:true loc (Var vi, offset) exp env kf
    | CompoundInit(_, inits) ->
      List.fold_left
        (fun acc (off, init) ->
           let off = Cil.addOffset off offset in
           handle_init off loc vi init acc kf)
        env
        inits

  let instr vi li loc env kf =
    if Memory_tracking.must_monitor_vi ~kf vi then
      match li with
      | AssignInit init ->
        handle_init NoOffset loc vi init env kf
      | ConsInit(fexp, args, _) ->
        let ret = Some (Cil.var vi) in
        let fexp = Cil.evar ~loc fexp in
        Function_call.instr ret fexp args loc env kf
    else
      env
end
(* }}} *)

(* ************************************************************************** *)
(* Track function arguments {{{ *)
(* ************************************************************************** *)

(* Update local environment with a statement tracking temporal metadata
   associated with adding a function argument to a stack frame *)
let track_argument ?(typ) param index env =
  let typ = Option.value ~default:param.vtype typ in
  match Cil.unrollTypeNode typ with
  | TPtr _
  | TComp _ ->
    let stmt = Mk.pull_param ~loc:Location.unknown param index in
    Env.add_stmt ~post:false env stmt
  | TInt _ | TFloat _ | TEnum _ | TBuiltin_va_list -> env
  | TNamed _ -> assert false
  | TVoid |TArray _ | TFun _ ->
    Options.fatal "Failed to handle function parameter"
(* }}} *)

(* ************************************************************************** *)
(* Handle return statements {{{ *)
(* ************************************************************************** *)

(* Update local environment [env] with statements tracking return value
   of a function. *)
let handle_return_stmt loc ret env =
  match ret.enode with
  | Lval lv ->
    if Cil.isPointerType (Cil.typeOfLval lv) then
      let exp = Cil.mkAddrOf ~loc lv in
      let stmt = Mk.handle_return_referent ~loc ~save:true exp in
      Env.add_stmt ~post:false env stmt
    else
      env
  | _ -> Options.fatal "Something other than Lval in return"

let handle_return_stmt loc ret env kf =
  if Memory_tracking.must_monitor_exp ~kf ret then
    handle_return_stmt loc ret env
  else
    env
(* }}} *)

(* ************************************************************************** *)
(* Handle instructions {{{ *)
(* ************************************************************************** *)

(* Update local environment [env] with statements tracking
   instruction [instr] *)
let handle_instruction instr env kf =
  match instr with
  | Set(lv, exp, loc) ->
    set_instr loc lv exp env kf
  | Call(ret, fexp, args, loc) ->
    Function_call.instr ret fexp args loc env kf
  | Local_init(vi, li, loc) ->
    Local_init.instr vi li loc env kf
  | Asm _ ->
    Options.warning ~once:true ~current:true
      "@[Analysis is potentially incorrect in presence of assembly code.@]";
    env
  | Skip _ | Code_annot _ -> env
(* }}} *)

(* ************************************************************************** *)
(* Initialization of globals {{{ *)
(* ************************************************************************** *)

(* Provided that [vi] is a global variable initialized by the initializer [init]
   at offset [off] return [Some stmt], where [stmt] is a statement
   tracking that initialization. If [init] does not need to be tracked than
   the return value is [None] *)
let mk_global_init ~loc vi off init =
  let exp = match init with
    | SingleInit e -> e
    (* Compound initializers should have been thrown away at this point *)
    | _ -> Options.fatal "Unexpected ComppoundInit in global initializer"
  in
  (* Initializer expression can be a literal string, so look up the
     corresponding variable which that literal string has been converted to *)
  let exp =
    try let rec get_string e = match e.enode with
        | Const(CStr str) -> str
        | CastE(_, exp) -> get_string exp
        | _ -> raise Not_found
      in
      let str = get_string exp in
      Cil.evar ~loc (Literal_strings.find str)
    with
    (* Not a literal string: just use the expression at hand *)
      Not_found -> exp
  in
  (* The input [vi] is from the old project, so get the corresponding variable
     from the new one, otherwise AST integrity is violated *)
  let lv = Var vi, off in
  mk_stmt_from_assign loc lv exp
(* }}} *)

(* ************************************************************************** *)
(* Public API {{{ *)
(* ************************************************************************** *)

let handle_function_parameters kf env =
  if is_enabled () then
    let env, _ = List.fold_left
        (fun (env, index) param ->
           let env =
             if Memory_tracking.must_monitor_vi ~kf param
             then track_argument param index env
             else env
           in
           env, index + 1)
        (env, 0)
        (Kernel_function.get_formals kf)
    in env
  else
    env

let handle_stmt stmt env kf =
  if is_enabled () then begin
    match stmt.skind with
    | Instr instr -> handle_instruction instr env kf
    | Return(ret, loc) ->
      Option.fold ~some:(fun ret -> handle_return_stmt loc ret env kf) ~none:env ret
    | Goto _ | Break _ | Continue _ | If _ | Switch _ | Loop _ | Block _
    | UnspecifiedSequence _ | Throw _ | TryCatch _ | TryFinally _
    | TryExcept _ -> env
  end else
    env

let generate_global_init vi off init =
  if is_enabled () then mk_global_init ~loc:vi.vdecl vi off init
  else None
(* }}} *)

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
