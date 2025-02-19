(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(*                                                                          *)
(****************************************************************************)

(* Modified by TrustInSoft *)

(* Type check and elaborate ABS to CIL *)

(* The references to ISO means ANSI/ISO 9899-1999 *)
module H = Hashtbl
module IH = Datatype.Int.Hashtbl

open Pretty_utils
open Cabs
open Cabshelper
open Cil
let valid_sid = false
(* All statements generated here must have an invalid sid. Use this variable
   for the valid_sid label of Cil.mkStmt*. *)
open Cil_types
open Cil_datatype
open Cil_const

(* Maps the start and end positions of a function declaration or definition
   (including its possible contract) to its name. *)
module FuncLocs = struct
  include
    State_builder.List_ref
      (Datatype.Triple
         (Cil_datatype.Position)(Cil_datatype.Position)(Datatype.String)
      )
      (struct
        let name = "FuncLocs"
        let dependencies = [ Kernel.Files.self ]
      end)

  let add_loc ?spec loc1 loc2 funcname =
    let startpos =
      match spec with
      | None -> fst loc1
      | Some (_, spec_loc) -> fst spec_loc
    in
    let endpos = snd loc2 in
    add (startpos, endpos, funcname)
end

let func_locs () = FuncLocs.get ()

(* Attributes which are entirely unsupported by Frama-C and must cause a
   parsing error, since their behavior requires non-standard parsing *)
let unsupported_attributes = ["vector_size"]

(* Attributes which must be erased to avoid issues (e.g., GCC 'malloc'
   attributes can refer to erased functions and make the code un-reparsable *)
let erased_attributes = ["malloc"]
let () = Ast_attributes.register_list AttrIgnored erased_attributes

let stripUnderscore s =
  if String.length s = 1 then begin
    if s = "_" then
      Kernel.error ~once:true ~current:true "Invalid attribute name %s" s;
    s
  end else begin
    let res = Extlib.strip_underscore s in
    if res = "" then
      Kernel.error ~once:true ~current:true "Invalid attribute name %s" s;
    if List.mem res unsupported_attributes then
      Kernel.error ~current:true "unsupported attribute: %s" s
    else begin
      if not (Ast_attributes.is_known res) then
        Kernel.warning
          ~once:true ~current:true ~wkey:Kernel.wkey_unknown_attribute
          "Unknown attribute: %s" s
    end;
    res
  end

let frama_c_keep_block = "FRAMA_C_KEEP_BLOCK"
let () = Cil_printer.register_shallow_attribute frama_c_keep_block
let () = Ast_attributes.register AttrStmt frama_c_keep_block

let fc_stdlib = "fc_stdlib"
let fc_stdlib_generated = "fc_stdlib_generated"
let () = Cil_printer.register_shallow_attribute fc_stdlib
let () = Cil_printer.register_shallow_attribute fc_stdlib_generated
(* fc_stdlib attribute already registered in cil.ml *)
let () = Ast_attributes.register (AttrName false) fc_stdlib_generated

let fc_local_static = "fc_local_static"
let () = Cil_printer.register_shallow_attribute fc_local_static
let () = Ast_attributes.register (AttrName false) fc_local_static

let frama_c_destructor = "fc_destructor"
let () = Cil_printer.register_shallow_attribute frama_c_destructor
let () = Ast_attributes.register (AttrName false) frama_c_destructor

let () =
  (* packed and aligned are treated separately, we ignore them
     during standard processing.
  *)
  Ast_attributes.register AttrIgnored "packed";
  Ast_attributes.register AttrIgnored "aligned";
  Ast_attributes.register (AttrFunType false) "warn_unused_result";
  Ast_attributes.register (AttrName false) "FC_OLDSTYLEPROTO";
  Ast_attributes.register (AttrName false) "static";
  Ast_attributes.register (AttrName false) "missingproto";
  Ast_attributes.register AttrIgnored "dummy";
  Ast_attributes.register AttrIgnored "signal"; (* AVR-specific attribute *)
  Ast_attributes.register AttrIgnored "leaf";
  Ast_attributes.register AttrIgnored "nonnull";
  Ast_attributes.register AttrIgnored "deprecated";
  Ast_attributes.register AttrIgnored "access";
  Ast_attributes.register AttrIgnored "returns_twice";
  Ast_attributes.register AttrIgnored "pure";
  Ast_attributes.register AttrIgnored "cleanup";
  Ast_attributes.register AttrIgnored "warning";
  ()

(** A hook into the code that creates temporary local vars.  By default this
    is the identity function, but you can overwrite it if you need to change the
    types of cabs2cil-introduced temp variables. *)
let typeForInsertedVar: (Cil_types.typ -> Cil_types.typ) ref = ref (fun t -> t)

let cabs_exp loc node = { expr_loc = loc; expr_node = node }

let abort_context ?loc msg =
  let loc = match loc with
    | None -> Current_loc.get()
    | Some loc -> loc
  in
  let append fmt =
    Format.pp_print_newline fmt ();
    Errorloc.pp_context_from_file fmt loc
  in
  Kernel.abort ~current:true ~append msg

module IgnorePureExpHook =
  Hook.Build (struct type t = string * Cil_types.exp end)

let register_ignore_pure_exp_hook f =
  IgnorePureExpHook.extend (fun (x,z) -> f x z)

module ImplicitPrototypeHook =
  Hook.Build (struct type t = varinfo end)

let register_implicit_prototype_hook f = ImplicitPrototypeHook.extend f

module DifferentDeclHook =
  Hook.Build(struct type t = varinfo * varinfo end)

let register_different_decl_hook f =
  DifferentDeclHook.extend (fun (x,y) -> f x y)

module NewGlobalHook = Hook.Build(struct type t = varinfo * bool end)
let register_new_global_hook f = NewGlobalHook.extend (fun (x, y) -> f x y)

module LocalFuncHook = Hook.Build(struct type t = varinfo end)

let register_local_func_hook = LocalFuncHook.extend

module IgnoreSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * Cil_types.exp end)

let register_ignore_side_effect_hook f =
  IgnoreSideEffectHook.extend (fun (y,z) -> f y z)

module ConditionalSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * Cabs.expression end)

module ForLoopHook =
  Hook.Build(struct
    type t =
      Cabs.for_clause * Cabs.expression * Cabs.expression * Cabs.statement
  end)

let register_for_loop_all_hook f =
  ForLoopHook.extend (fun (x,y,z,t) -> f x y z t)

let register_for_loop_init_hook f =
  ForLoopHook.extend (fun (x,_,_,_) -> f x)

let register_for_loop_test_hook f =
  ForLoopHook.extend (fun (_,x,_,_) -> f x)

let register_for_loop_incr_hook f =
  ForLoopHook.extend (fun (_,_,x,_) -> f x)

let register_for_loop_body_hook f =
  ForLoopHook.extend (fun (_,_,_,x) -> f x)

let register_conditional_side_effect_hook f =
  ConditionalSideEffectHook.extend (fun (y,z) -> f y z)

(* These symbols are supposed to be macros. It is not possible to
   take their address or to redeclare them outside of the proper header
   in stdlib. See CERT MSC38-C rule.
*)
let no_suppress_function_macro =
  [ "assert"; "setjmp"; "va_arg"; "va_copy"; "va_end"; "va_start" ]

let no_redefine_macro =
  "errno" :: "math_errhandling" :: no_suppress_function_macro

let is_stdlib_function_macro n = List.mem n no_suppress_function_macro

let is_stdlib_macro n = List.mem n no_redefine_macro

let is_bitwise_bop = function
  | Cabs.BAND | Cabs.BOR | Cabs.XOR -> true
  | _ -> false

let is_relational_bop = function
  | EQ | NE | LT | GT | LE | GE -> true
  | _ -> false

let rec stripParen e =
  match e with
  | { expr_node = Cabs.PAREN e } -> stripParen e
  | e -> e

let is_for_builtin builtin info =
  match info with
  | SINGLE_INIT { expr_node = VARIABLE name } ->
    String.equal ("__fc_" ^ builtin) name
  | _ -> false

let rec is_dangerous_offset = function
    NoOffset -> false
  | Field (fi, o) ->
    Cil.typeHasAttribute "volatile" (Cil.unrollType fi.ftype) ||
    is_dangerous_offset o
  | Index _ -> true

let rec is_dangerous e = match e.enode with
  | Lval lv | AddrOf lv | StartOf lv -> is_dangerous_lval lv
  | UnOp (_,e,_) | CastE(_,e) -> is_dangerous e
  | BinOp(_,e1,e2,_) -> is_dangerous e1 || is_dangerous e2
  | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
    false
and is_dangerous_lval = function
  | Var v,_ when
      (not v.vglob && not v.vformal && not v.vtemp)
      || Ast_attributes.contains "volatile" v.vattr
      || Cil.typeHasAttribute "volatile" (Cil.unrollType v.vtype)
    -> true
  (* Local might be uninitialized, which will trigger UB,
     but we assume that the variables we generate are correctly initialized.
  *)
  | Var _, o -> is_dangerous_offset o
  | Mem _,_ -> true

class check_no_locals = object
  inherit nopCilVisitor
  method! vlval (h,_) =
    (match h with
     | Var v ->
       if not v.vglob then
         Kernel.error ~once:true ~current:true
           "Forbidden access to local variable %a in static initializer"
           Cil_printer.pp_varinfo v
     | _ -> ());
    DoChildren
end

let rec check_no_locals_in_initializer i =
  match i with
  | SingleInit e ->
    ignore (visitCilExpr (new check_no_locals) e)
  | CompoundInit (ct, initl) ->
    foldLeftCompound ~implicit:false
      ~doinit:(fun _off' i' _ () ->
          check_no_locals_in_initializer i')
      ~ct:ct
      ~initl:initl
      ~acc:()


(* ---------- source error message handling ------------- *)
let cabslu s =
  {Cil_datatype.Position.unknown with
   Filepath.pos_path = Datatype.Filepath.of_string ("Cabs2cil_start" ^ s)},
  {Cil_datatype.Position.unknown with
   Filepath.pos_path = Datatype.Filepath.of_string ("Cabs2cil_end" ^ s)}


(** Keep a list of the variable ID for the variables that were created to
 * hold the result of function calls *)
let callTempVars: unit IH.t = IH.create 13

(* Check that s starts with the prefix p *)
let prefix p s =
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p


(***** PROCESS PRAGMAS **********)

(* fc_stdlib pragma. Delimits blocks of globals that are declared in
   a given std lib header. By default, they will not be pretty-printed by
   frama-c -print, which will emit #include "header.h" instead
*)
let current_stdheader = ref []

let pop_stdheader () =
  match !current_stdheader with
  | s::l ->
    Kernel.debug ~dkey:Kernel.dkey_typing_pragma "Popping %s %s" fc_stdlib s;
    current_stdheader := l
  | [] ->
    Kernel.warning ~current:true
      "#pragma %s pop does not match a push" fc_stdlib

let push_stdheader s =
  Kernel.debug ~dkey:Kernel.dkey_typing_pragma "Pushing %s %s@." fc_stdlib s;
  current_stdheader := s::!current_stdheader

(* Returns the topmost (latest) header that is not internal to Frama-C,
   unless it is the only one.
   This prevents the pretty-printing function from including Frama-C
   internal files, unless they were directly specified by the user. *)
let get_current_stdheader () =
  let rec aux = function
    | [] -> ""
    | [ s ] -> s
    | s :: l when String.starts_with ~prefix:"__fc_" s -> aux l
    | s :: _ -> s
  in
  aux !current_stdheader

(* there are several pragmas that we process directly here and remove
   from the globals list, by returning None. We bind their respective
   processing functions with the operator below.
*)
let (>>?) opt f =
  match opt with
  | Some (name, args) -> f name args
  | _ -> opt

let process_stdlib_pragma name args =
  if name = fc_stdlib then begin
    match args with
    | [ ACons ("pop",_) ] -> pop_stdheader (); None
    | [ ACons ("push",_); AStr s ] ->
      let base_name = (System_config.Share.libc:>string) in
      let relative_name = Filepath.relativize ~base_name s in
      push_stdheader relative_name;
      None
    | _ -> Some (name, args)
  end else Some (name, args)

let fc_stdlib_attribute attrs =
  let s = get_current_stdheader () in
  if s = "" then attrs
  else begin
    let payload, attrs =
      if Ast_attributes.contains fc_stdlib attrs then begin
        AStr s :: Ast_attributes.find_params fc_stdlib attrs,
        Ast_attributes.drop fc_stdlib attrs
      end else [AStr s], attrs
    in
    Ast_attributes.add (fc_stdlib, payload) attrs
  end
(* ICC align/noalign pragmas (not supported by GCC/MSVC with this syntax).
   Implemented by translating them to 'aligned' attributes. Currently,
   only default and noalign are supported, not explicit alignment values.
   Cf. www.slac.stanford.edu/grp/cd/soft/rmx/manuals/IC_386.PDF *)
let current_pragma_align = ref (None : bool option)
let pragma_align_by_struct = H.create 17

let process_align_pragma name args =
  let aux pname v =
    (if Machine.(msvcMode () || gccMode ())
     then Kernel.warning ?wkey:None else Kernel.debug ~level:1 ?dkey:None)
      ~current:true "Parsing ICC '%s' pragma." pname;
    match args with
    | [] -> current_pragma_align := Some v
    | l ->
      List.iter
        (function
          | AStr s | ACons (s, _) -> H.replace pragma_align_by_struct s v
          | _ -> Kernel.warning ~current:true
                   "Unsupported '%s' pragma not honored by Frama-C." pname
        ) l
  in
  match name with
  | "align" -> aux "align" true
  | "noalign" -> aux "noalign" false
  | _ -> ()

let align_pragma_for_struct sname =
  try Some (H.find pragma_align_by_struct sname)
  with Not_found -> !current_pragma_align

(* The syntax and semantics for the pack pragmas are GCC's, which emulates most
   of MSVC's behaviors. Some of it has been tested using MSVC 2010.
   Note that #pragma pack directives are emulated by translating them into
   GCC-style attributes, which in turn are not supported by MSVC.
   Therefore some combinations of attributes may be impossible to produce in
   MSVC, which means that Frama-C on an MSVC machdep may accept more programs
   that MSVC would. *)

(* The pack pragma stack *)
let packing_pragma_stack = Stack.create ()

(* The current pack pragma *)
let current_packing_pragma = ref None
let pretty_current_packing_pragma fmt =
  let align =
    Option.value ~default:(Integer.of_int (Machine.alignof_aligned ()))
      !current_packing_pragma
  in
  Integer.pretty fmt align

(* Checks if [n] is a valid alignment for #pragma pack, and emits a warning
   if it is not the case. Returns the value to be set as current packing pragma.
   From the MSDN reference
   (msdn.microsoft.com/en-us/library/2e70t5y1(v=vs.100).aspx):
   Valid values are 1, 2, 4, 8, and 16.

   NOTE: GCC seems to consider '#pragma pack(0)' as equivalent to '#pragma pack()',
   but this is not specified in their documentation. To avoid rejecting programs
   with such pragmas, we emulate GCC's current behavior but emit a warning.
   This is the only case when this function returns [None]. *)
let get_valid_pragma_pack_alignment n =
  if Integer.is_zero n && Machine.gccMode () then begin
    Kernel.warning ~current:true "GCC accepts pack(0) but does not specify its \
                                  behavior; considering it equivalent to pack()";
    true, None
  end
  else begin
    let valid = Integer.(equal n one || equal n two || equal n four ||
                         equal n eight || equal n sixteen)
    in
    if not valid then
      Kernel.warning ~current:true "ignoring invalid packing alignment (%a)"
        Integer.pretty n;
    valid, Some n
  end

let process_pack_pragma name args =
  begin match name with
    | "pack" -> begin
        match args with
        | [ACons ("",[])] (*  #pragma pack() *) ->
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "packing pragma: restoring alignment to default (%d)"
            (Machine.alignof_aligned ());
          current_packing_pragma := None; None
        | [AInt n] (* #pragma pack(n) *) ->
          let is_valid, new_pragma = get_valid_pragma_pack_alignment n in
          if is_valid then begin
            Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
              "packing pragma: setting alignment to %a" Integer.pretty n;
            current_packing_pragma := new_pragma; None
          end else
            Some (name, args)
        | [ACons ("push",[])] (* #pragma pack(push) *) ->
          Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
            "packing pragma: pushing alignment %t" pretty_current_packing_pragma;
          Stack.push !current_packing_pragma packing_pragma_stack; None
        | [ACons ("push",[]); AInt n] (* #pragma pack(push,n) *) ->
          let is_valid, new_pragma = get_valid_pragma_pack_alignment n in
          if is_valid then begin
            Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
              "packing pragma: pushing alignment %t, setting alignment to %a"
              pretty_current_packing_pragma Integer.pretty n;
            Stack.push !current_packing_pragma packing_pragma_stack;
            current_packing_pragma:= new_pragma; None
          end else
            Some (name, args)
        | ACons ("push",[]) :: args (* unknown push directive *) ->
          Kernel.warning ~current:true
            "Unsupported argument for pragma pack push directive: `%a'."
            Format.(
              pp_print_list
                ~pp_sep:(fun fmt ()->pp_print_string fmt ", ")
                Cil_printer.pp_attrparam)
            args;
          (* We don't change the current packing directive, but
             nevertheless push it on the stack, to avoid a mismatched
             pop somewhere later. *)
          Stack.push !current_packing_pragma packing_pragma_stack;
          None
        | [ACons ("pop",[])] (* #pragma pack(pop) *) ->
          begin try
              current_packing_pragma := Stack.pop packing_pragma_stack;
              Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
                "packing pragma: popped alignment %t" pretty_current_packing_pragma;
              None
            with Stack.Empty ->
              (* GCC/Clang/MSVC seem to ignore the directive when a pop() is
                 called with an empty stack, so we emulate their behavior. *)
              Kernel.warning ~current:true
                "ignoring #pragma pack(pop) with empty stack";
              None
          end
        | [ACons ("show",[])] (* #pragma pack(show) *) ->
          Some (name, args)
        | _ ->
          Kernel.warning ~current:true
            "Unsupported packing pragma not honored by Frama-C: #pragma pack(%a)"
            (Pretty_utils.pp_list ~sep:", " ~empty:"<empty>"
               Cil_printer.pp_attrparam) args;
          Some (name, args)
      end
    | _ -> Some (name, args)
  end

let force_packed_attribute a =
  if Ast_attributes.contains "packed" a then a
  else Ast_attributes.add ("packed",[]) a

let is_power_of_two i = i > 0 && i land (i-1) = 0

(* Computes the numeric value corresponding to an 'aligned' attribute:
   – if 'aligned' (without integer), then use the maximum machine alignment;
   – else, try to const-fold the expression to an integer value.
   Returns [Some n] in case of success, [None] otherwise.
   Note that numeric values that are not powers of two are invalid and
   also return [None]. *)
let eval_aligned_attrparams aps =
  match aps with
  | [] -> Some (Integer.of_int (Machine.alignof_aligned ()))
  | [ap] ->
    begin
      match Cil.intOfAttrparam ap with
      | None -> None
      | Some n -> if is_power_of_two n then Some (Integer.of_int n) else None
    end
  | _ -> (* 'aligned(m,n,...)' is not a valid syntax *) None

let warn_invalid_align_attribute aps =
  Kernel.warning ~current:true ~once:true
    "ignoring invalid aligned attribute: %a"
    Cil_printer.pp_attribute ("aligned", aps)

(* If there is more than one 'aligned' attribute, GCC's behavior is to
   consider the maximum among them. This function computes this value
   and also emits warnings for invalid attributes. *)
let combine_aligned_attributes attrs =
  match Ast_attributes.filter "aligned" attrs with
  | [] -> None
  | aligned_attrs ->
    List.fold_left (fun acc attr ->
        match attr with
        | ("aligned", aps) ->
          begin
            let align = eval_aligned_attrparams aps in
            if align = None then begin
              warn_invalid_align_attribute aps;
              acc
            end else
              match acc, align with
              | None, a | a, None -> a
              | Some old_n, Some new_n -> Some (Integer.max old_n new_n)
          end
        | _ -> assert false (* attributes were previously filtered by name *)

      ) None aligned_attrs

let warn_incompatible_pragmas_attributes apragma has_attrs =
  if apragma <> None then
    Kernel.warning ~current:true ~once:true
      "ignoring 'align' pragma due to presence of 'pack' pragma.@ \
       No compiler was supposed to accept both syntaxes.";
  if Machine.msvcMode () && has_attrs then
    (* MSVC does not allow attributes *)
    Kernel.warning ~current:true ~once:true
      "field attributes should not be present in MSVC-compatible sources"

(* checks [attrs] for invalid aligned() attributes *)
let check_aligned attrs =
  List.fold_right (fun attr acc ->
      match attr with
      | ("aligned", aps) ->
        if eval_aligned_attrparams aps = None then
          (warn_invalid_align_attribute aps; acc)
        else attr :: acc
      | _ -> attr :: acc
    ) attrs []

(* Takes into account the possible effect of '#pragma pack' directives on
   component [ci], and checks the alignment of aligned() attributes.
   This function is complemented by
   [process_pragmas_pack_align_field_attributes]. *)
let process_pragmas_pack_align_comp_attributes loc ci cattrs =
  let source = snd loc in
  match !current_packing_pragma, align_pragma_for_struct ci.corig_name with
  | None, None -> check_aligned cattrs
  | Some n, apragma ->
    warn_incompatible_pragmas_attributes apragma (cattrs <> []);
    let with_aligned_attributes =
      match combine_aligned_attributes cattrs with
      | None ->
        (* No valid aligned attributes in this field.
           – if the composite type has a packed attribute, then add the
             alignment given by the pack pragma;
           – otherwise, no alignment attribute is necessary.
           Drop existing "aligned" attributes, if there are invalid ones. *)
        if Ast_attributes.contains "packed" cattrs
        then Ast_attributes.drop "aligned" cattrs
        else begin
          Kernel.feedback ~source ~dkey:Kernel.dkey_typing_pragma
            "adding aligned(%a) attribute to comp '%s' due to packing pragma"
            Integer.pretty n ci.cname;
          Ast_attributes.replace_params "aligned" [AInt n] cattrs
        end
      | Some local ->
        (* The largest aligned wins with GCC. Don't know
           with other compilers. *)
        let align = Integer.max n local in
        Kernel.feedback ~source ~dkey:Kernel.dkey_typing_pragma
          "setting aligned(%a) attribute to comp '%s' due to packing pragma"
          Integer.pretty align ci.cname;
        Ast_attributes.replace_params "aligned" [AInt align] cattrs
    in
    force_packed_attribute with_aligned_attributes
  | None, Some true ->
    Ast_attributes.drop "aligned" cattrs
  | None, Some false ->
    force_packed_attribute
      (Ast_attributes.replace_params "aligned" [AInt Integer.one] cattrs)

(* Takes into account the possible effect of '#pragma pack' directives on
   field [fi], and checks the alignment of aligned() attributes.
   Because we emulate them using GCC attributes, this transformation
   is complex and depends on several factors:
   - if the struct inside the pragma is packed, then ignore alignment constraints
     given by the pragma;
   - otherwise, each struct field should have the alignment given by the pack
     directive, unless that field already has an align attribute, in which case
     the minimum of both is taken into account (note that, in GCC, if a field
     has 2 alignment directives, it is the maximum of those that is taken). *)
let process_pragmas_pack_align_field_attributes fi fattrs cattr =
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = fi.floc in
  match !current_packing_pragma, align_pragma_for_struct fi.forig_name with
  | None, None -> check_aligned fattrs
  | Some n, apragma ->
    warn_incompatible_pragmas_attributes apragma (fattrs <> []);
    let field_align = combine_aligned_attributes fattrs in
    (* If this field has no valid aligned attributes and the composite type
        has a packed attribute, nothing needs to be done: the composite will
        have the "packed" attribute anyway. *)
    if field_align = None && Ast_attributes.contains "packed" cattr then
      Ast_attributes.drop "aligned" fattrs
    else
      (* Otherwise, align on min(n, max(field alignment, type alignment)):
         the field alignment attribute (if there is one) may be smaller than
         its type alignment, so we get the maximum of both. Then, we apply
         the pragma pack: the final alignment will be the minimum between what
         we had and [n]. *)
      let type_align = Integer.of_int (Cil.bytesAlignOf fi.ftype) in
      let existing_align =
        Option.fold field_align ~none:type_align ~some:(Integer.max type_align)
      in
      let new_align = Integer.min n existing_align in
      Kernel.feedback ~dkey:Kernel.dkey_typing_pragma ~current:true
        "%s aligned(%a) attribute to field '%s.%s' due to packing pragma"
        (if Option.is_none field_align then "adding" else "setting")
        Integer.pretty new_align fi.fcomp.cname fi.fname;
      Ast_attributes.replace_params "aligned" [AInt new_align] fattrs
  | None, Some true ->
    Ast_attributes.drop "aligned" fattrs
  | None, Some false ->
    Ast_attributes.replace_params "aligned" [AInt Integer.one] fattrs


(***** COMPUTED GOTO ************)

(* The address of labels are small integers (starting from 0). A computed
 * goto is replaced with a switch on the address of the label. We generate
 * only one such switch and we'll jump to it from all computed gotos. To
 * accomplish this we'll add a local variable to store the target of the
 * goto. *)

(* The local variable in which to put the detonation of the goto and the
 * statement where to jump *)
let gotoTargetData: (varinfo * stmt) option ref = ref None

(* The "addresses" of labels *)
let gotoTargetHash: (string, int) H.t = H.create 13
let gotoTargetNextAddr: int ref = ref 0


(* When we process an argument list, remember the argument index which has a
 * transparent union type, along with the original type. We need this to
 * process function definitions *)
let transparentUnionArgs : (int * typ) list ref = ref []

let debugLoc = false
let convLoc (l : cabsloc) =
  if debugLoc then
    Kernel.debug "convLoc at %a: line %d, btye %d\n"
      Datatype.Filepath.pretty (fst l).Filepath.pos_path
      (fst l).Filepath.pos_lnum (fst l).Filepath.pos_bol;
  l

let isOldStyleVarArgName n =
  if Machine.msvcMode () then n = "va_alist"
  else n = "__builtin_va_alist"

let isOldStyleVarArgTypeName n =
  if Machine.msvcMode () then n = "va_list"  || n = "__ccured_va_list"
  else n = "__builtin_va_alist_t"

(* CERT EXP 46 rule: operands of bitwise operators should not be of type _Bool
   or the result of a comparison.
*)
let check_logical_operand e t =
  let (source,_) = e.expr_loc in
  match Cil.unrollTypeNode t with
  | TInt IBool ->
    Kernel.warning ~wkey:Kernel.wkey_cert_exp_46 ~source
      "operand of bitwise operator has boolean type"
  | _ ->
    let rec aux = function
      | { expr_node = Cabs.PAREN e} -> aux e
      | { expr_node = Cabs.BINARY (bop,_,_); expr_loc = (source, _) }
        when is_relational_bop bop ->
        Kernel.warning ~wkey:Kernel.wkey_cert_exp_46 ~source
          "operand of bitwise operator is a logical relation"
      | _ -> (* EXP 46 does not forbid something like
                (x && y) & z, even though the logical and returns 0 or 1 as
                a relational operator. Maybe this should be clarified. *)
        ()
    in
    aux e

(*** EXPRESSIONS *************)

(* We collect here the program *)
let theFile : global list ref = ref []
let theFileTypes : global list ref = ref []
(* This hashtbl contains the varinfo-indexed globals of theFile.
   They are duplicated here for faster lookup *)
let theFileVars : global Cil_datatype.Varinfo.Hashtbl.t =
  Cil_datatype.Varinfo.Hashtbl.create 13

let findVarInTheFile vi =
  try  List.rev (Cil_datatype.Varinfo.Hashtbl.find_all theFileVars vi)
  with Not_found -> []

let update_fundec_in_theFile vi (f:global -> unit) =
  let rec aux = function
    | [] -> assert false
    | (GFunDecl _ as g) :: _ -> f g
    | _ :: tl -> aux tl
  in
  aux (findVarInTheFile vi)

let update_funspec_in_theFile vi spec =
  let rec aux = function
    | [] -> assert false
    | GFun (f,oldloc) :: _ ->
      Logic_utils.merge_funspec ~oldloc f.sspec spec
    | _ :: tl -> aux tl
  in
  aux (findVarInTheFile vi)

let find_existing_behaviors vi =
  let behaviors spec = List.map (fun x -> x.b_name) spec.spec_behavior in
  let aux acc = function
    | GFun(f,_) -> (behaviors f.sspec) @ acc
    | GFunDecl (spec,_,_)  -> behaviors spec @ acc
    | _ -> acc
  in List.fold_left aux [] (findVarInTheFile vi)

let get_formals vi =
  let rec aux = function
    | [] -> assert false
    | GFun(f,_)::_ -> f.sformals
    | _ :: tl -> aux tl
  in aux (findVarInTheFile vi)

let initGlobals () =
  theFile := [];
  theFileTypes := [];
  Cil_datatype.Varinfo.Hashtbl.clear theFileVars


(* Keep track of some variable ids that must be turned into definitions. We
 * do this when we encounter what appears a definition of a global but
 * without initializer. We leave it a declaration because maybe down the road
 * we see another definition with an initializer. But if we don't see any
 * then we turn the last such declaration into a definition without
 * initializer *)
let mustTurnIntoDef: bool IH.t = IH.create 117

(* Globals that have already been defined. Indexed by the variable name. *)
let alreadyDefined: (string, location) H.t = H.create 117

let isDefined vi = H.mem alreadyDefined vi.vorig_name

(* Globals that were created due to static local variables. We chose their
 * names to be distinct from any global encountered at the time. But we might
 * see a global with conflicting name later in the file. *)
let staticLocals: (string, varinfo) H.t = H.create 13


(* Typedefs. We chose their names to be distinct from any global encountered
 * at the time. But we might see a global with conflicting name later in the
 * file *)
let typedefs: (string, typeinfo) H.t = H.create 13

let fileGlobals () =
  let rec revonto (tail: global list) = function
      [] -> tail

    | GVarDecl (vi, _) :: rest when IH.mem mustTurnIntoDef vi.vid ->
      IH.remove mustTurnIntoDef vi.vid;
      (* Use the location of vi instead of the one carried by GVarDecl.
         Maybe we found in the same file a declaration and then a tentative
         definition. In this case, both are GVarDecl, but the location carried
         by [vi] is the location of the tentative definition, which is more
         useful. *)
      if vi.vstorage = Extern then vi.vstorage <- NoStorage;
      vi.vdefined <- true;
      revonto (GVar (vi, {init = None}, vi.vdecl) :: tail) rest

    | x :: rest -> revonto (x :: tail) rest
  in
  revonto (revonto [] !theFile) !theFileTypes


class checkGlobal = object
  inherit nopCilVisitor


  method! vglob = function
    | GVar _ -> DoChildren
    | _ -> SkipChildren

  method! vexpr exp =
    begin
      match exp.enode with
      | SizeOfE _ ->
        (* sizeOf doesn't depend on the definitions *)
        ()
      | _ ->
        let problematic_var : string option ref = ref None in
        let is_varinfo_cst vi =
          let res = Cil.isConstType vi.vtype && isDefined vi in
          if not res then problematic_var := Some vi.vorig_name;
          res
        in
        if not(isConstant ~is_varinfo_cst exp)
        then
          match !problematic_var with
          | Some name ->
            Kernel.error ~once:true ~current:true
              ("%s is not a compile-time constant") name
          | None ->
            Kernel.error ~once:true ~current:true
              "Initializer element is not a compile-time constant";
    end;
    SkipChildren

end

let cabsPushGlobal (g: global) =
  ignore (visitCilGlobal (new checkGlobal) g);
  pushGlobal g ~types:theFileTypes ~variables:theFile;
  (match g with
   | GVar (vi, _, _) | GVarDecl (vi, _)
   | GFun ({svar = vi}, _) | GFunDecl (_, vi, _) ->
     (* Do 'add' and not 'replace' here, as we may store both
        declarations and definitions for the same varinfo *)
     Cil_datatype.Varinfo.Hashtbl.add theFileVars vi g
   | _ -> ()
  )


(********* ENVIRONMENTS ***************)

(* The environment is kept in two distinct data structures. A hash table maps
 * each original variable name into a varinfo (for variables, or an
 * enumeration tag, or a type). (Note that the varinfo might contain an
 * alpha-converted name different from that of the lookup name.) The Ocaml
 * hash tables can keep multiple mappings for a single key. Each time the
 * last mapping is returned and upon deletion the old mapping is restored. To
 * keep track of local scopes we also maintain a list of scopes (represented
 * as lists).  *)
type envdata =
    EnvVar of varinfo                   (* The name refers to a variable
                                         * (which could also be a function) *)
  | EnvEnum of enumitem                 (* the name refers to an enum item *)
  | EnvTyp of typ                       (* The name is of the form  "struct
                                         * foo", or "union foo" or "enum foo"
                                         * and refers to a type. Note that
                                         * the name of the actual type might
                                         * be different from foo due to alpha
                                         * conversion *)
  | EnvLabel of string                  (* The name refers to a label. This
                                         * is useful for GCC's locally
                                         * declared labels. The lookup name
                                         * for this category is "label foo" *)

let env  = Datatype.String.Hashtbl.create 307
(* ghost environment: keep track of all symbols, in the order
   in which they have been introduced. Superset of env *)
let ghost_env = Datatype.String.Hashtbl.create 307
(* We also keep a global environment. This is always a subset of the env *)
let global_env = Datatype.String.Hashtbl.create 307
(* ghost global environment: superset of global and subset of ghost *)
let ghost_global_env = Datatype.String.Hashtbl.create 307

(* maps a C label to the ghost environment of variables in scope
   at this program point. Used for typing \at() terms and predicates. *)
let label_env = Datatype.String.Hashtbl.create 307

let add_label_env lab =
  let add_if_absent v (d,_) map =
    match d with
    | EnvVar vi when not (Datatype.String.Map.mem v map) ->
      Datatype.String.Map.add v vi map
    | _ -> map
  in
  let open Datatype.String.Hashtbl in
  let lab_env = fold add_if_absent ghost_env Datatype.String.Map.empty in
  add label_env lab lab_env

let remove_label_env lab =
  Datatype.String.Hashtbl.remove label_env lab

(* In the scope we keep the original name, so we can remove them from the
 * hash table easily *)
type undoScope =
    UndoRemoveFromEnv of bool * string (* boolean indicates whether we should
                                          remove from ghost env only, or both.*)
  | UndoAlphaEnv of location Alpha.undoAlphaElement list

let scopes :  undoScope list ref list ref = ref []

(* tries to estimate if the name 's' was declared in the current scope;
   note that this may not work in all cases *)
let declared_in_current_scope ~ghost s =
  match !scopes with
  | [] -> (* global scope *)
    let env = if ghost then ghost_global_env else global_env in
    Datatype.String.Hashtbl.mem env s
  | cur_scope :: _ ->
    let names_declared_in_current_scope =
      List.filter_map
        (function
          | UndoRemoveFromEnv (ghost',s) when ghost || not ghost' -> Some s
          | _ -> None)
        !cur_scope
    in
    List.mem s names_declared_in_current_scope

(* When you add to env, you also add it to the current scope *)
let addLocalToEnv ghost name data =
  let v = data, Current_loc.get() in
  Datatype.String.Hashtbl.add ghost_env name v;
  if not ghost then Datatype.String.Hashtbl.add env name v;
  (* If we are in a scope, then it means we are not at top level. Add the
   * name to the scope *)
  match !scopes with
  | [] -> begin
      match data with
      | EnvVar _ ->
        Kernel.fatal ~current:true
          "addLocalToEnv: not in a scope when adding %s!" name
      | _ ->
        (* Adding a type with local scope *)
        Datatype.String.Hashtbl.add ghost_global_env name v;
        if not ghost then Datatype.String.Hashtbl.add global_env name v
    end
  | s :: _ -> s := UndoRemoveFromEnv (ghost, name) :: !s

let addGlobalToEnv ghost name data =
  let open Datatype.String.Hashtbl in
  let v = data, Current_loc.get () in
  add ghost_env name v;
  if not ghost then add env name v;
  add ghost_global_env name v;
  if not ghost then add global_env name v

(* Create a new name based on a given name. The new name is formed from a
 * prefix (obtained from the given name as the longest prefix that ends with
 * a non-digit), followed by a '_' and then by a positive integer suffix. The
 * first argument is a table mapping name prefixes with the largest suffix
 * used so far for that prefix. The largest suffix is one when only the
 * version without suffix has been used. *)
let alphaTable : location Alpha.alphaTable = H.create 307
(* vars and enum tags. For composite types we have names like "struct
 * foo" or "union bar" *)

let fresh_global lookupname =
  fst (Alpha.newAlphaName ~alphaTable ~undolist:None ~lookupname
         ~data:(Current_loc.get ()))

(* To keep different name scopes different, we add prefixes to names
 * specifying the kind of name: the kind can be one of "" for variables or
 * enum tags, "struct" for structures and unions (they share the name space),
 * "enum" for enumerations, or "type" for types *)
let kindPlusName (kind: string)
    (origname: string) : string =
  (* typedefs live in the same namespace as normal identifiers. *)
  if kind = "" || kind = "type" then origname
  else kind ^ " " ^ origname

let stripKind (kind: string) (kindplusname: string) : string =
  let kind = if kind = "type" then "" else kind in
  let l = 1 + String.length kind in
  if l > 1 then
    String.sub kindplusname l (String.length kindplusname - l)
  else
    kindplusname

let is_same_kind kind info =
  match kind, info with
  | "", EnvEnum _
  | "enum", EnvTyp _
  | "type", EnvTyp _
  | "struct", EnvTyp _
  | "union", EnvTyp _
  | "label", EnvLabel _
  | "", EnvVar _ -> true
  | _, _ -> false

let find_identifier_decl ghost name info =
  match info with
  | UndoRemoveFromEnv (ghost', name') ->
    (ghost || not ghost') && name = name'
  | _ -> false

let newAlphaName
    ghost
    (globalscope: bool) (* The name should have global scope *)
    (kind: string)
    (origname: string) : string * location =
  let lookupname = kindPlusName kind origname in
  (* If we are in a scope then it means that we are alpha-converting a local
   * name. Go and add stuff to reset the state of the alpha table but only to
   * the top-most scope (that of the enclosing function) *)
  let rec findEnclosingFun = function
      [] -> (* At global scope *) None
    | [s] -> Some s
    | _ :: rest -> findEnclosingFun rest
  in
  let undo_scope =
    if not globalscope then findEnclosingFun !scopes else None
  in
  let undolist =
    match undo_scope with None -> None | Some _ -> Some (ref [])
  in
  let data = Current_loc.get () in
  let newname, oldloc =
    Alpha.newAlphaName ~alphaTable ~undolist ~lookupname ~data
  in
  (match undo_scope, undolist with
   | None, None -> ()
   | Some s, Some l -> s := (UndoAlphaEnv !l) :: !s
   | _ -> assert false (* by construction, both options have the same status*));
  if newname <> lookupname then begin
    try
      let info =
        if !scopes = [] then begin
          let env = if ghost then ghost_global_env else global_env in
          fst (Datatype.String.Hashtbl.find env lookupname)
        end else
        if List.exists
            (find_identifier_decl ghost lookupname) !(List.hd !scopes)
        then begin
          let env = if ghost then ghost_env else env in
          fst (Datatype.String.Hashtbl.find env lookupname)
        end
        else raise Not_found
      in
      if kind <> "type" then
        (* in C11, typedefs can be redefined under some conditions (which are
           checked in doTypedef); this test catches other kinds of errors, such
           as redefined enumeration constants *)
        Kernel.error ~current:true
          "redefinition of '%s'%s in the same scope.@ \
           Previous declaration was at %a"
          origname (if is_same_kind kind info then "" else " with different kind")
          Cil_datatype.Location.pretty oldloc
    with
    | Not_found -> () (* no clash of identifiers *)
    | Failure _ ->
      Kernel.fatal ~current:true
        "finding a fresh identifier in local scope with empty scopes stack"
  end;
  stripKind kind newname, oldloc

(*** In order to process GNU_BODY expressions we must record that a given
 *** COMPUTATION is interesting *)
let gnu_body_result : (Cabs.statement * ((exp * typ) option ref)) ref
  = ref ({stmt_ghost = false; stmt_node = Cabs.NOP (None, cabslu "_NOP")}, ref None)

(*** When we do statements we need to know the current return type *)
let dummy_function = emptyFunction "@dummy@"
let currentReturnType : typ ref = ref voidType
let currentFunctionFDEC: fundec ref = ref dummy_function

let lastStructId = ref 0
let anonStructName (k: string) (suggested: string) =
  incr lastStructId;
  "__anon" ^ k ^ (if suggested <> "" then "_"  ^ suggested else "")
  ^ "_" ^ (string_of_int (!lastStructId))


let constrExprId = ref 0


let startFile () =
  let open Datatype.String.Hashtbl in
  clear label_env;
  clear env;
  clear ghost_env;
  H.clear alphaTable;
  lastStructId := 0

(* Lookup a variable name. Return also the location of the definition. Might
 * raise Not_found  *)
let lookupVar ghost name =
  let env = if ghost then ghost_env else env in
  match Datatype.String.Hashtbl.find env name with
  | (EnvVar vi), loc -> vi, loc
  | _ -> raise Not_found

let only_ghost_symbol name =
  try ignore (lookupVar false name); false
  with Not_found ->
  try ignore (lookupVar true name); true
  with Not_found -> false

let lookupGlobalVar ghost name =
  let env = if ghost then ghost_global_env else global_env in
  match Datatype.String.Hashtbl.find env name with
  | (EnvVar vi), loc -> vi, loc
  | _ -> raise Not_found

(* Add a new variable. Do alpha-conversion if necessary *)
let alphaConvertVarAndAddToEnv addtoenv vi =
  (* Announce the name to the alpha conversion table *)
  let newname, oldloc =
    newAlphaName vi.vghost (addtoenv && vi.vglob) "" vi.vname
  in
  (* Make a copy of the vi if the name has changed. Never change the name for
   * global variables *)
  let newvi =
    if vi.vname = newname then
      vi
    else begin
      if vi.vglob then begin
        (* if a purely local variable stole our name, force it to be renamed.*)
        let local =
          List.find_opt
            (fun x -> x.vname = vi.vname) !currentFunctionFDEC.slocals
        in
        match local with
        | Some local -> local.vname <- newname; vi
        | None ->
          (* Perhaps this is because we have seen a static local which happened
           * to get the name that we later want to use for a global. *)
          try
            let static_local_vi = H.find staticLocals vi.vname in
            H.remove staticLocals vi.vname;
            (* Use the new name for the static local *)
            static_local_vi.vname <- newname;
            (* And continue using the last one *)
            vi
          with Not_found -> begin
              (* Or perhaps we have seen a typedef which stole our name. This is
                 possible because typedefs use the same name space *)
              try
                let typedef_ti = H.find typedefs vi.vname in
                H.remove typedefs vi.vname;
                (* Use the new name for the typedef instead *)
                typedef_ti.tname <- newname;
                (* And continue using the last name *)
                vi
              with Not_found ->
                abort_context
                  "It seems that we would need to rename global %s (to %s) \
                   because of previous occurrence at %a"
                  vi.vname newname Cil_printer.pp_location oldloc;
            end
      end else copyVarinfo vi newname
    end
  in
  (* Store all locals in the slocals (in reversed order). *)
  if not vi.vglob && not vi.vformal then
    !currentFunctionFDEC.slocals <- newvi :: !currentFunctionFDEC.slocals;

  (if addtoenv then
     if vi.vglob then
       addGlobalToEnv vi.vghost vi.vname (EnvVar newvi)
     else
       addLocalToEnv vi.vghost vi.vname (EnvVar newvi));
(*
  ignore (E.log "  new=%s\n" newvi.vname);
*)
  (*  ignore (E.log "After adding %s alpha table is: %a\n"
              newvi.vname docAlphaTable alphaTable); *)
  newvi

let constFoldTypeVisitor = object
  inherit nopCilVisitor
  method! vtype { tnode; tattr }: typ visitAction =
    match tnode with
    | TArray(bt, Some len) ->
      let len' = constFold true len in
      ChangeDoChildrenPost (
        mk_tarray ~tattr bt (Some len'),
        (fun x -> x)
      )
    | _ -> DoChildren
end

(* Const-fold any expressions that appear as array lengths in this type *)
let constFoldType (t:typ) : typ =
  visitCilType constFoldTypeVisitor t

let to_integer i =
  match Integer.to_int_opt i with
  | Some i -> i
  | None ->
    Kernel.error ~current:true "integer too large: %a"
      Integer.pretty_hex i;
    -1

let constFoldToInteger e =
  try Option.map Integer.to_int_exn (Cil.constFoldToInt e)
  with Z.Overflow ->
    Kernel.error ~current:true
      "integer constant too large in expression %a"
      Cil_printer.pp_exp e;
    None

let get_temp_name ghost () =
  let undolist = ref [] in
  let data = Current_loc.get() in
  let name = if ghost then "g_tmp" else "tmp" in
  let name, _ =
    Alpha.newAlphaName ~alphaTable ~undolist:(Some undolist) ~lookupname:name ~data
  in
  let undolist = !undolist in
  Alpha.undoAlphaChanges ~alphaTable ~undolist;
  name

(* Create a new temporary variable *)
let newTempVar ~ghost loc descr (descrpure:bool) typ =
  let t' = (!typeForInsertedVar) typ in
  let t' = Cil.typeRemoveAttributes ["const"] t' in
  let name = get_temp_name ghost () in
  let vi = makeVarinfo ~ghost ~temp:true ~loc false false name t' in
  vi.vdescr <- Some descr;
  vi.vdescrpure <- descrpure;
  alphaConvertVarAndAddToEnv false vi

let mkAddrOfAndMark loc ((b, off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  begin match lastOffset off with
    | NoOffset ->
      (match b with
       | Var vi -> vi.vaddrof <- true
       | _ -> ())
    | Index _ -> ()
    | Field(fi,_) -> fi.faddrof <- true
  end;
  mkAddrOf ~loc lval

(* Call only on arrays *)
let mkStartOfAndMark loc ((b, _off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  (match b with
   | Var vi -> vi.vaddrof <- true
   | _ -> ());
  new_exp ~loc (StartOf lval)

(* Keep a set of self compinfo for composite types *)
let compInfoNameEnv : (string, compinfo) H.t = H.create 113
let enumInfoNameEnv : (string, enuminfo) H.t = H.create 113


let lookupTypeNoError ghost kind name =
  let kn = kindPlusName kind name in
  let env = if ghost then ghost_env else env in
  match Datatype.String.Hashtbl.find env kn with
  | EnvTyp t, l -> t, l
  | _ -> raise Not_found

let lookupType ghost kind name =
  try
    lookupTypeNoError ghost kind name
  with Not_found ->
    abort_context "Cannot find type %s (kind:%s)" name kind

(* Create the self ref cell and add it to the map. Return also an indication
 * if this is a new one. *)
let createCompInfo (iss: bool) (n: string) ~(norig: string) : compinfo * bool =
  (* Add to the self cell set *)
  let key = (if iss then "struct " else "union ") ^ n in
  try
    H.find compInfoNameEnv key, false (* Only if not already in *)
  with Not_found -> begin
      (* Create a compinfo. This will have "cdefined" false. *)
      let res =
        mkCompInfo
          iss n ~norig (fun _ -> None) (fc_stdlib_attribute [])
      in
      H.add compInfoNameEnv key res;
      res, true
    end

(* Create the self ref cell and add it to the map. Return an indication
 * whether this is a new one. *)
let createEnumInfo (n: string) ~(norig:string) : enuminfo * bool =
  (* Add to the self cell set *)
  try
    H.find enumInfoNameEnv n, false (* Only if not already in *)
  with Not_found -> begin
      (* Create a enuminfo *)
      let enum =
        { eorig_name = norig; ename = n; eitems = [];
          eattr = fc_stdlib_attribute []; ereferenced = false; ekind = IInt ; }
      in
      H.add enumInfoNameEnv n enum;
      enum, true
    end


(* kind is either "struct" or "union" or "enum" and n is a name *)
let findCompType ghost kind name tattr =
  let makeForward () =
    (* This is a forward reference, either because we have not seen this
     * struct already or because we want to create a version with different
     * attributes  *)
    if kind = "enum" then
      let enum, isnew = createEnumInfo name ~norig:name in
      if isnew then
        begin
          if not (Machine.gccMode ()) then
            Kernel.error ~once:true
              ~source:(fst @@ Current_loc.get ())
              "forward declaration of enum %s" (Machdep.allowed_machdep "GCC");
          cabsPushGlobal (GEnumTagDecl (enum, Current_loc.get ()));
        end;
      mk_tenum ~tattr enum
    else
      let iss = kind = "struct" in
      let self, isnew = createCompInfo iss name ~norig:name in
      if isnew then
        cabsPushGlobal (GCompTagDecl (self, Current_loc.get ()));
      mk_tcomp ~tattr self
  in
  try
    let old, _ = lookupTypeNoError ghost kind name in (* already defined  *)
    let olda = typeAttrs old in
    let equal =
      try List.for_all2 Cil_datatype.Attribute.equal olda tattr
      with Invalid_argument _ -> false
    in
    if equal then old else makeForward ()
  with Not_found -> makeForward ()


(* A simple visitor that searches a statement for labels *)
class canDropStmtClass pRes = object
  inherit nopCilVisitor

  method! vstmt s =
    if s.labels != [] then
      (pRes := false; SkipChildren)
    else
    if !pRes then DoChildren else SkipChildren

  method! vinst _ = Cil.SkipChildren
  method! vexpr _ = Cil.SkipChildren
end

let canDropStatement (s: stmt) : bool =
  let pRes = ref true in
  let vis = new canDropStmtClass pRes in
  ignore (visitCilStmt vis s);
  !pRes

let fail_if_incompatible_sizeof ~ensure_complete op typ =
  if Cil.isFunctionType typ && Machine.sizeof_fun () < 0 then
    Kernel.error ~current:true "%s called on function %s" op
      (Machdep.allowed_machdep "GCC");
  let is_void = Cil.isVoidType typ in
  if is_void && Machine.sizeof_void () < 0 then
    Kernel.error ~current:true "%s on void type %s" op
      (Machdep.allowed_machdep "GCC/MSVC");
  if ensure_complete && not (Cil.isCompleteType typ) && not is_void then
    Kernel.error ~current:true
      "%s on incomplete type '%a'" op Cil_datatype.Typ.pretty typ

(******** CASTS *********)

let arithmeticConversion = Cil.arithmeticConversion

let integralPromotion = Cil.integralPromotion

(* C99 6.3.2.1:2: l-values used as r-values lose their qualifier. By default,
   we drop qualifiers, and recover them for the few operators that are
   exceptions, also listed in 6.3.2.1:2 *)
let dropQualifiers = Cil.type_remove_qualifier_attributes

(* A cast that is used for conditional expressions. Pointers are Ok.
   Abort if invalid *)
let checkBool (ot : typ) (_ : exp) =
  if not (Cil.isScalarType ot) then
    abort_context "cannot cast expr of type %a into a boolean value"
      Cil_datatype.Typ.pretty ot

(* Evaluate constants to CTrue (non-zero) or CFalse (zero) *)
let rec isConstTrueFalse c: [ `CTrue | `CFalse ] =
  match c with
  | CInt64 (n,_,_) ->
    if Integer.equal n Integer.zero then `CFalse else `CTrue
  | CChr c ->
    if Char.code c = 0 then `CFalse else `CTrue
  | CStr _ | CWStr _ -> `CTrue
  | CReal(f, _, _) ->
    if f = 0.0 then `CFalse else `CTrue
  | CEnum {eival = e} ->
    match isExpTrueFalse (Cil.constFold true e) with
    | `CTrue | `CFalse as r -> r
    | `CUnknown -> Kernel.fatal ~current:true "Non-constant enum"
(* Evaluate expressions to `CTrue, `CFalse or `CUnknown *)
and isExpTrueFalse e: [ `CTrue | `CFalse | `CUnknown ] =
  match e.enode with
  | Const c -> (isConstTrueFalse c :> [ `CTrue | `CFalse | `CUnknown ])
  | CastE _ -> begin (* Do not ignore the cast, because of possible overflows.
                        However, calling constFoldToInt might make some UB disappear... *)
      match Cil.constFoldToInt e with
      | None -> `CUnknown
      | Some i ->
        if Integer.(equal zero i) then `CFalse else `CTrue
    end
  | _ -> `CUnknown

let rec isCabsZeroExp e = match e.expr_node with
  | CAST (_, ie) ->
    (match ie with
     | SINGLE_INIT e -> isCabsZeroExp e
     | NO_INIT | COMPOUND_INIT _ -> false)
  | CONSTANT (CONST_INT i) ->
    Result.fold ~error:(fun _ -> false) ~ok:Integer.is_zero (Cil.parseIntRes i)
  | _ -> false

module BlockChunk =
struct
  type chunk = {
    stmts: (stmt * lval list * lval list * lval list * stmt ref list) list;
    (* statements of the chunk.

       This list is built on reverse order.

       Each statements comes with the list of
       pending modified, written and read values.
       The first category represents values which are to be modified during
       the execution of the chunk and whose new value depends on the
       statement (hence, it is legal to read them). They are removed
       syntactically from the list of reads, but we keep them to avoid
       spurious warnings in presence of aliases.
       The order of the write is supposed to be
       fixed at this level.
       We also maintain a list of function calls inside the chunk.
       E.g. for G[i] = j, the written lval is G[i], and the read lval are
       G, i, and j.
    *)

    unspecified_order:bool; (* order of evaluation of statements in the
                               chunk is unspecified.
                            *)
    locals: varinfo list; (* variables that are local to the chunk. *)
    statics: varinfo list; (* static variables whose syntactic scope is the
                              current chunk. *)
    cases: stmt list;                 (* A list of case statements
                                       * (statements with Case labels)
                                       * visible at the outer level *)
  }

  let d_stmt_chunk fmt (s,modified,write,reads,calls) =
    Format.fprintf fmt "@[<v 0>/*@[(%a) %a@ <-@ %a@]@;Calls:@;%a@;*/@;%a@]"
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) modified
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) write
      (Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval) reads
      (Pretty_utils.pp_list ~sep:",@ "
         (fun fmt x -> Cil_printer.pp_stmt fmt !x)) calls
      Cil_printer.pp_stmt s

  let d_chunk fmt (c: chunk) =
    Format.fprintf fmt "@[<v 0>@[%a%a@\n%a@]@;@[<v 2>{%a@]}@]"
      (fun fmt b -> if b then Format.fprintf fmt "/* UNDEFINED ORDER */@\n")
      c.unspecified_order
      (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) c.locals
      (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) c.statics
      (Pretty_utils.pp_list ~sep:";@\n" d_stmt_chunk)
      (List.rev c.stmts)

  let empty =
    { stmts = []; cases = []; locals = []; statics = [];
      unspecified_order = false; }

  let empty_stmts l =
    let rec is_empty_stmt s =
      match s.skind with
      | Instr (Skip _) -> s.labels = [] && s.sattr = []
      | Block b -> b.battrs = [] && List.for_all is_empty_stmt b.bstmts
      | UnspecifiedSequence seq ->
        List.for_all is_empty_stmt (List.map (fun (x,_,_,_,_) -> x) seq)
      | _ -> false
    in
    List.for_all is_empty_stmt (List.map (fun (x,_,_,_,_) -> x) l)

  let isEmpty c = empty_stmts c.stmts

  let isNotEmpty c = not (isEmpty c)

  let i2c (i,m,w,r) =
    let c = match i.skind with
      | Instr(Call _ | Local_init(_, ConsInit _, _)) -> [ref i]
      | _ -> []
    in
    { empty with stmts = [i,m,w,r,c]; }

  (* Keep track of the gotos *)
  let backPatchGotos : (string, stmt ref list ref) H.t = H.create 17
  let addGoto (lname: string) (bref: stmt ref) : unit =
    let gotos =
      try
        H.find backPatchGotos lname
      with Not_found -> begin
          let gotos = ref [] in
          H.add backPatchGotos lname gotos;
          gotos
        end
    in
    gotos := bref :: !gotos

  (* Keep track of the labels *)
  let labelStmt : (string, stmt) H.t = H.create 17
  let initLabels () =
    H.clear backPatchGotos;
    H.clear labelStmt

  let resolveGotos () =
    H.iter
      (fun lname gotos ->
         try
           let dest = H.find labelStmt lname in
           List.iter (fun gref -> gref := dest) !gotos;
           (* Format.eprintf "Label %s associated to %a@." lname d_stmt dest*)
         with Not_found -> begin
             Kernel.error ~once:true ~current:true "Label %s not found" lname
           end)
      backPatchGotos

  module Logic_labels = struct
    (* On the contrary to C, use of labels in the logic
       obeys block scope rules. We keep track of these scopes here.
    *)
    let labels: (string, stmt) H.t = H.create 7
    (* label held by the current statement. *)
    let label_current = ref []
    let add_current_label s = label_current := s::!label_current
    (* Don't remove all current label at once, as there might be some
       labels on nested statements. See bts 1536. *)
    let reset_current_label () =
      label_current:= List.tl !label_current

    let scope = Stack.create ()
    let enter_scope () = Stack.push (ref []) scope
    let exit_scope () =
      let scope_labels = Stack.pop scope in
      List.iter (H.remove labels) !scope_labels

    let add_label l stmt =
      let scope = Stack.top scope in
      scope := l::!scope;
      H.add labels l stmt

    let find_label s =
      try
        ref (H.find labels s)
      with Not_found when List.mem s !label_current ->
        (* just a placeholder that will never be used. no need to
           check for ghost status here. *)
        let my_stmt =
          mkEmptyStmt ~ghost:false ~valid_sid ~loc:(cabslu "_find_label") ()
        in
        my_stmt.labels <- [Label(s,cabslu "_find_label",true)];
        let my_ref = ref my_stmt in
        addGoto s my_ref; my_ref
  end

  let add_label l labstmt =
    Logic_labels.add_label l labstmt;
    H.add labelStmt l labstmt

  (* transforms a chunk into a block.
     Note that if the chunk has its unspecified_order flag set, the resulting
     block contains a single UnspecifiedSequence statement. However, whatever
     the unspecified_order value, if the chunk consists in a single block, this
     block will get returned directly, unless collapse_block is set to false.
     By default, the block is scoping. If force_non_scoping is true
     (and the block does not declare anything by itself), it is made
     non-scoping.
  *)
  let c2block ~ghost ?(collapse_block=true) ?(force_non_scoping=false) c =
    let declares_var = c.locals <> [] || c.statics <> [] in
    let vars = ref [] in
    let cleanup_types =
      object
        val replacements = Cil_datatype.Varinfo.Hashtbl.create 3
        inherit Cil.nopCilVisitor
        method! vvrbl vi =
          if List.memq vi c.locals && vi.vdefined then begin
            (* This can happen in particular in a SizeOfE used in the size
               of a declared array. As the actual definition of the referenced
               variable is deported to the Local_init node, we use an undefined
               temp variable instead as placeholder.
            *)
            let vi' =
              match Cil_datatype.Varinfo.Hashtbl.find_opt replacements vi with
              | None ->
                let vi' =
                  newTempVar
                    ~ghost vi.vdecl (vi.vname ^ " initialization") true vi.vtype
                in
                Cil_datatype.Varinfo.Hashtbl.add replacements vi vi';
                vars := vi' :: !vars;
                vi'
              | Some vi' -> vi'
            in
            ChangeTo vi'
          end else SkipChildren
      end
    in
    let cleanup_var vi =
      Cil.update_var_type vi (Cil.visitCilType cleanup_types vi.vtype)
    in
    List.iter cleanup_var c.locals;
    !currentFunctionFDEC.slocals <- !currentFunctionFDEC.slocals @ !vars;
    let vars = !vars @ c.locals in
    match c.stmts with
    | [{ skind = Block b } as s,_,_,_,_] when
        collapse_block && s.labels = []
        && (ghost = s.ghost || Cil.is_ghost_else b) ->
      b.blocals <- vars @ b.blocals;
      b.bstatics <- c.statics @ b.bstatics;
      b.bscoping <- b.bscoping || declares_var || not force_non_scoping;
      b
    | stmts ->
      if c.unspecified_order then begin
        if List.length stmts >= 2 then begin
          let first_stmt =
            (fun (s,_,_,_,_) -> s) (Extlib.last stmts) in
          Kernel.warning ~wkey:Kernel.wkey_cert_exp_10
            ~source:(fst (Stmt.loc first_stmt))
            "Potential unsequenced side-effects"
        end;
        let b =
          Cil.mkBlock
            [mkStmt ~ghost ~valid_sid (UnspecifiedSequence (List.rev stmts))]
        in
        b.blocals <- vars;
        b.bstatics <- c.statics;
        b.bscoping <- declares_var || not force_non_scoping;
        b
      end else
        let stmts = List.rev_map (fun (s,_,_,_,_) -> s) stmts in
        let b = Cil.mkBlock stmts in
        b.blocals <- vars;
        b.bstatics <- c.statics;
        b.bscoping <- declares_var || not force_non_scoping;
        b

  (* converts a chunk into a statement. *)
  let c2stmt ~ghost ?force_non_scoping c =
    let kind =
      if c.unspecified_order then begin
        if List.length c.stmts >= 2 then begin
          let first_stmt =
            (fun (s,_,_,_,_) -> s) (Extlib.last c.stmts) in
          Kernel.warning ~wkey:Kernel.wkey_cert_exp_10
            ~source:(fst (Stmt.loc first_stmt))
            "Potential unsequenced side-effects" end;
        let kind = UnspecifiedSequence (List.rev c.stmts) in
        if c.locals <> [] || c.statics <> [] then begin
          let b = Cil.mkBlock [mkStmt ~ghost ~valid_sid kind] in
          b.blocals <- c.locals;
          b.bstatics <- c.statics;
          Block b
        end else kind
      end else
        let block = c2block ~ghost ?force_non_scoping c in Block block
    in
    mkStmt ~ghost ~valid_sid kind

  let merge_effects (m1,w1,r1,c1) (m2,w2,r2,c2) =
    let add_uniq l x =
      if List.exists (Lval.equal x) l then l else x::l
    in
    List.fold_left add_uniq m1 m2,
    List.fold_left add_uniq w1 w2,
    List.fold_left add_uniq r1 r2,
    c1 @ c2

  let get_chunk_effects c =
    List.fold_left
      (fun c (_,x,y,z,t) -> merge_effects c (x,y,z,t)) ([],[],[],[]) c.stmts

  (* make a chunk element from a chunk.  *)
  let c2stmt_effect ~ghost c =
    let modified, writes, reads, calls = get_chunk_effects c in
    let stmt = c2stmt ~ghost ~force_non_scoping:true c in
    (stmt, modified, writes, reads, calls)

  let unspecified_chunk c = (* c *)
    (* to restore previous behavior (where unspecified evaluation order
       was not explicitly marked), comment out the line below and make
       unspecified_chunk the identity function.
    *)
    { c with unspecified_order = true }

  let local_var_chunk c v = { c with locals = v::c.locals }

  let static_var_chunk c v = { c with statics = v :: c.statics }

  let visit_chunk vis c =
    List.iter
      (fun (stmt, _, _, _, _) -> ignore (Cil.visitCilStmt vis stmt))
      c.stmts

  let remove_locals l =
    !currentFunctionFDEC.slocals <-
      List.filter
        (fun x -> not (List.exists (Cil_datatype.Varinfo.equal x) l))
        !currentFunctionFDEC.slocals

  let clean_up_block_locals (s, _, _, _, _) =
    let vis =
      object
        inherit Cil.nopCilVisitor
        method! vblock b = remove_locals b.blocals; DoChildren
      end
    in
    ignore (Cil.visitCilStmt vis s)

  (* if we're about to drop a chunk, clean up locals of current func. *)
  let clean_up_chunk_locals c =
    remove_locals c.locals;
    List.iter clean_up_block_locals c.stmts

  (* Gathers locals of blocks. *)
  class locals_visitor () = object
    inherit Cil.nopCilVisitor

    val mutable locals = []
    method get_locals () = locals

    method !vblock block =
      locals <- block.blocals @ locals;
      Cil.DoChildren
  end

  (* Returns the list of all locals in the chunk [c], including the locals
     of blocks in the list of statements of [c].  *)
  let locals_in_chunk c =
    let locals = c.locals in
    let visitor = new locals_visitor () in
    visit_chunk (visitor :> Cil.cilVisitor) c;
    visitor#get_locals () @ locals

  (* Removes the locals of the chunk [c] (including locals of blocks inside
     the chunk) from the locals of the current function. *)
  let full_clean_up_chunk_locals c =
    let locals = locals_in_chunk c in
    !currentFunctionFDEC.slocals <-
      List.filter
        (fun x -> not (List.exists (Cil_datatype.Varinfo.equal x) locals))
        !currentFunctionFDEC.slocals

  (* removes all labels found in the given chunk from the labels table.
     Use this function when you're about to drop a chunk _and_ you are sure
     that there are no references to such labels outside of the chunk (if there
     are, you should not drop it in the first place). Primarily used for
     dropping side-effects from sizeof of related C expressions, in which
     the only labels that might occur are generated by cabs2cil itself and
     are completely internal.
  *)
  let full_clean_up_chunk_labels c =
    let vis = object
      inherit Cil.nopCilVisitor
      method! vstmt s =
        List.iter
          (function
            | Label (s,_,_) ->
              H.remove labelStmt s;
              H.remove backPatchGotos s
            | Case _ | Default _ -> ())
          s.labels;
        Cil.DoChildren
    end
    in
    visit_chunk vis c

  (* drop the side effects coming from the given expression and takes care
     of cleaning the global environment (labels tables and locals list of
     the current function). First argument is used in the warning to indicate
     which construction is dropping side effects
  *)
  let drop_chunk ctxt c e e' =
    if isNotEmpty c then begin
      Kernel.feedback
        ~once:true ~current:true "Dropping side-effect in %s." ctxt;
      IgnoreSideEffectHook.apply (e, e');
      full_clean_up_chunk_labels c;
      let kept_vars, thrown_vars =
        List.partition (fun x -> Cil.appears_in_expr x e') c.locals
      in
      full_clean_up_chunk_locals {c with locals = thrown_vars};
      { empty with locals = kept_vars }
    end else empty

  (* Add a statement at the end. Never refer to this statement again
   * after you call this *)
  let (+++) (c: chunk) (i,m,w,r) =
    let call = match i.skind with
      | Instr (Call _ | Local_init (_, ConsInit _, _)) -> [ref i]
      | _ -> []
    in
    {c with stmts = (i,m,w,r,call) :: c.stmts; }

  (* Append two chunks. Never refer to the original chunks after you call
   * this. And especially never share c2 with somebody else *)
  let (@@@) (c1: chunk) (c2, ghost) =
    let r =
      if (c1.unspecified_order && c2.unspecified_order) ||
         (not c1.unspecified_order && not c2.unspecified_order)
      then
        { stmts = c2.stmts @ c1.stmts;
          cases = c1.cases @ c2.cases;
          locals = c1.locals @ c2.locals;
          statics = c1.statics @ c2.statics;
          unspecified_order = c1.unspecified_order;
        }
      else
        match c2.stmts with
        | [] ->
          (match c2.locals, c2.statics with
           | [],[] -> c1
           | ll, ls ->
             { c1 with
               locals = c1.locals @ ll ;
               statics = c1.statics @ ls })
        | [{skind = UnspecifiedSequence l} as s,_,_,_,_]
          when c1.unspecified_order ->
          let stmts =
            match l, s.labels with
            | [],[] -> []
            | [], _ ->
              let s' = mkStmtOneInstr (Skip (Cil_datatype.Stmt.loc s)) in
              s'.labels <- s.labels;
              [s,[],[],[],[]]
            | (h,_,_,_,_)::_, _ ->
              h.labels <- h.labels @ s.labels; l
          in
          { stmts = List.rev_append stmts c1.stmts;
            cases = c1.cases @ c2.cases;
            locals = c1.locals @ c2.locals;
            statics = c1.statics @ c2.statics;
            unspecified_order = c1.unspecified_order; }
        | [s] ->
          { stmts = s :: c1.stmts;
            cases = c1.cases @ c2.cases;
            locals = c1.locals @ c2.locals;
            statics = c1.statics @ c2.statics;
            unspecified_order = c1.unspecified_order;
          }
        | _ ->
          let locals = c1.locals @ c2.locals in
          let statics = c1.statics @ c2.statics in
          (* the lifespan of the locals is the whole chunk,
             not just c2, which may be transformed artificially
             in a block at this point. Likewise, the syntactic scope of
             static local variables is the whole chunk.
          *)
          let c2 = { c2 with locals = []; statics = [] } in
          { stmts = c2stmt_effect ~ghost c2 :: c1.stmts;
            cases = c1.cases @ c2.cases;
            locals; statics;
            unspecified_order = c1.unspecified_order;
          }
    in
    Kernel.debug ~dkey:Kernel.dkey_typing_chunk
      "Concat:@\n%a@\nWITH@\n%a@\nLEADS TO@\n%a@."
      d_chunk c1 d_chunk c2 d_chunk r;
    r

  let remove_reads lv c =
    Kernel.debug ~dkey:Kernel.dkey_typing_chunk
      "Removing %a from chunk@\n%a@."
      Cil_printer.pp_lval lv d_chunk c;
    let remove_list =
      List.filter (fun x -> not (LvalStructEq.equal lv x))
    in
    let remove_from_reads =
      List.map (fun (s,m,w,r,c) -> (s,lv::m,w,remove_list r,c)) in
    let res =
      { c with stmts = remove_from_reads c.stmts; }
    in
    (* Format.eprintf "Result is@\n%a@." d_chunk res; *)
    res

  let remove_effects_stmt (s,_,_,_,_) = (s,[],[],[],[])

  let remove_effects c =
    { c with stmts = List.map remove_effects_stmt c.stmts }

  (* Put chunk inside a block. Optionally take a list of varinfo to add to
     the newly created block. *)
  let enclose_chunk ~ghost ?(locals=[]) se =
    let block_chunk = c2stmt_effect ~ghost se in
    {empty with stmts = [ block_chunk ]; locals}

  (* This function is used to hide a chunk inside a block and only make
     visible its result.
     We first create a new tmp variable which stores se's result, we concat it
     with se, enclose them inside a block, and return the resulting chunk and
     our tmp variable as the new result.

     Result for type t :
     se' :
       t tmp_0;
       {
        se;
        tmp_0 = e;
       }
     e' : tmp_0
  *)
  let hide_chunk ~ghost ~loc read se e t =
    let descr =
      Format.asprintf "%a" Cil_descriptive_printer.pp_exp e
    in
    let tmp_res = newTempVar ~ghost loc descr true t in
    let tmp_res_lv = (Var tmp_res, NoOffset) in
    let res_instr = Set (tmp_res_lv, e, loc) in
    let res_stmt = Cil.mkStmtOneInstr ~ghost ~valid_sid res_instr in
    let res_chunk = i2c (res_stmt, [], [tmp_res_lv], read) in
    let se_res_chunk = se @@@ (res_chunk, ghost) in
    let se' = enclose_chunk ~ghost ~locals:[tmp_res] se_res_chunk in
    let e' = new_exp ~loc (Lval tmp_res_lv) in
    se', e'

  (* the chunks below are used in statements translation. Hence,
     their order of evaluation is always specified, and we can forget their
     effects.
  *)

  let skipChunk = empty

  (* return can be ghost but only in ghost functions *)
  let returnChunk ~ghost e (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Return(e, l)),[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let ifChunk ~ghost be (l: location) (t: chunk) (e: chunk) : chunk =
    let effects_t = get_chunk_effects t in
    let effects_e = get_chunk_effects e in
    let (m,r,w,c) = merge_effects effects_t effects_e in
    let stmt =
      mkStmt ~ghost ~valid_sid (If(be, c2block ~ghost t, c2block ~ghost e, l))
    in
    { stmts = [ stmt ,m,r,w,c ];
      cases = t.cases @ e.cases;
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let keepPureExpr ~ghost e loc =
    let fundec = !currentFunctionFDEC in
    let s = Cil.mkPureExpr ~ghost ~fundec ~loc e in
    match s.skind with
    | Block b ->
      { empty with
        stmts = List.map (fun s -> (s,[],[],[],[])) b.bstmts;
        locals = b.blocals }
    | _ -> i2c (s,[],[],[])

  (* We can duplicate a chunk if it has a few simple statements, and if
   * it does not have cases, locals or statics *)
  let duplicateChunk (c: chunk) = (* raises Failure if you should not
                                   * duplicate this chunk *)
    if not (Kernel.AllowDuplication.get ()) then
      raise (Failure "cannot duplicate: disallowed by user");
    if c.locals !=[] then
      raise (Failure "cannot duplicate: has locals");
    if c.statics != [] then
      raise (Failure "cannot duplicate: has static locals");
    if c.cases != [] then raise (Failure "cannot duplicate: has cases") else
      let pCount = ref 0 in
      let duplicate_stmt (s,m,w,r,c) =
        if s.labels != [] then
          raise (Failure "cannot duplicate: has labels");
        (match s.skind with
         | If _ | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
         | TryCatch _ | Throw _ | TryFinally _ | TryExcept _
           ->
           raise (Failure "cannot duplicate: complex stmt")
         | Instr _ | Goto _ | Return _ | Break _ | Continue _ ->
           incr pCount);
        if !pCount > 5 then raise
            (Failure ("cannot duplicate: too many instr"));
        (* We can just copy it because there is nothing to share here.
         * Except maybe for the ref cell in Goto but it is Ok to share
         * that, I think *)
        let s' = { s with sid = s.sid} in
        let c = match s.skind with
          | Instr (Call _ | Local_init (_, ConsInit _, _)) -> [ref s']
          | Instr _ | TryExcept _ | TryFinally _ | TryCatch _ | Throw _
          | UnspecifiedSequence _| Block _| Loop (_, _, _, _, _)
          | Switch (_, _, _, _)| If (_, _, _, _)| Continue _| Break _
          | Goto (_, _)| Return (_, _) -> assert (c = []); []
        in
        (s',m,w,r,c)
      in
      { stmts = List.map duplicate_stmt c.stmts;
        cases = []; unspecified_order = c.unspecified_order;
        locals = []; statics = [];
      }

  (* We can drop a chunk if it does not have labels inside *)
  let canDrop (c: chunk) =
    List.for_all (fun (s,_,_,_,_) -> canDropStatement s) c.stmts

  let loopChunk ~ghost ~sattr a (body: chunk) : chunk =
    (* Make the statement *)
    let loop =
      mkStmt ~ghost ~valid_sid ~sattr
        (Loop (a,c2block ~ghost body, Current_loc.get (), None, None))
    in
    { stmts = [ loop,[],[],[],[] ];
      cases = body.cases;
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* can be ghost inside a ghost loop *)
  let breakChunk ~ghost (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Break l),[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* can be ghost inside a ghost loop *)
  let continueChunk ~ghost (l: location) : chunk =
    { stmts = [ mkStmt ~ghost ~valid_sid (Continue l),[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  (* Get the first statement in a chunk. Might need to change the
   * statements in the chunk *)
  let getFirstInChunk ~ghost ~loc c =
    (* Get the first statement and add the label to it *)
    match c.stmts with
    | [] -> (* Add a statement *)
      let n = mkEmptyStmt ~ghost ~valid_sid ~loc () in
      n, [n,[],[],[],[]]
    | s ->
      let (st,_,_,_,_) = Extlib.last s in
      if not ghost && st.ghost then
        (* non-ghost label in front of a ghost statement. Keep the
           non-ghost status with a Skip.
           Note that the reverse case is not possible:
           /*@ ghost L1: */ stmt; will be directly translated
           by the parser as /*@ ghost L1: ; */ stmt;
        *)
        begin
          let n = mkEmptyStmt ~ghost ~valid_sid ~loc () in
          n, s @ [n,[],[],[],[]]
        end else st,s

  (* s2c must not be used during expression translation, as it does not
     take care of the effects of the statement. Use i2c instead.
  *)
  let s2c (s:stmt) : chunk =
    { stmts = [ s,[],[],[],[] ];
      cases = [];
      unspecified_order = false;
      locals = [];
      statics = [];
    }

  let gotoChunk ~ghost (ln: string) (l: location) : chunk =
    let dummy = {dummyStmt with labels = [Label (ln, l, false)]} in
    let gref = ref dummy in
    addGoto ln gref;
    { stmts = [ mkStmt ~ghost ~valid_sid (Goto (gref, l)),[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  let caseRangeChunk ~ghost el loc (next: chunk) =
    let fst, stmts' = getFirstInChunk ~ghost ~loc next in
    let labels = List.map (fun e -> Case (e, loc)) el in
    fst.labels <- labels @ fst.labels;
    { next with stmts = stmts'; cases = fst :: next.cases;
                unspecified_order = false
    }

  let defaultChunk ~ghost loc (next: chunk) =
    let fst, stmts' = getFirstInChunk ~ghost ~loc next in
    let lb = Default loc in
    fst.labels <- lb :: fst.labels;
    { next with stmts = stmts'; cases = fst :: next.cases;
                unspecified_order = false
    }

  let switchChunk ~ghost (e: exp) (body: chunk) (l: location) =
    (* Make the statement *)
    let defaultSeen = ref false in
    let t = typeOf e in
    let checkForDefaultAndCast lb =
      match lb with
      | Default _ as d ->
        if !defaultSeen then
          Kernel.error ~once:true ~current:true
            "Switch statement at %a has duplicate default entries."
            Cil_printer.pp_location l;
        defaultSeen := true;
        d
      | Label _ as l -> l
      | Case (e, loc) ->
        (* If needed, convert e to type t, and check in case the label
           was too big *)
        let e' = mkCast ~newt:t e in
        let constFold = constFold true in
        let e'' = if Machine.lower_constants () then constFold e' else e' in
        (match constFoldToInt e, constFoldToInt e'' with
         | Some i1, Some i2 when not (Integer.equal i1 i2) ->
           Kernel.feedback ~once:true ~source:(fst e.eloc)
             "Case label %a exceeds range of %a for switch expression. \
              Nothing to worry."
             Cil_printer.pp_exp e Cil_datatype.Typ.pretty t;
         | _ -> ()
        );
        Case (e'', loc)
    in
    let block = c2block ~ghost body in
    let cases = (* eliminate duplicate entries from body.cases. A statement
                   is added to body.cases for each case label it has. *)
      List.fold_right
        (fun s acc ->
           if List.memq s acc then acc
           else begin
             s.labels <- List.map checkForDefaultAndCast s.labels;
             s::acc
           end)
        body.cases
        []
    in
    let switch = mkStmt ~ghost ~valid_sid (Switch (e, block, cases, l)) in
    { stmts = [ switch,[],[],[],[] ];
      cases = [];
      locals = [];
      statics = [];
      unspecified_order = false;
    }

  exception Found

  let find_stmt b l s =
    let find = object
      inherit Cil.nopCilVisitor
      method! vstmt s' =
        if s == s' then begin
          (*Format.eprintf "Label %s is in the AST@." l;*)
          raise Found
        end else DoChildren
    end in
    try
      ignore (visitCilBlock find b);
      Kernel.warning ~current:true
        "Inconsistent AST: Statement %a,@ with label %s is not in the AST"
        Cil_printer.pp_stmt s l;
    with Found -> ()

  class cleanUnspecified =
    let is_annot_next_stmt = function
      | [] -> false
      | { skind = Instr (Code_annot (c,_)) } :: _ ->
        Logic_utils.is_annot_next_stmt c
      | _ -> false
    in
    object(self)
      inherit nopCilVisitor
      val unspecified_stack = Stack.create ()

      val mutable replace_table = []

      (* we start in a deterministic block. *)
      initializer Stack.push false unspecified_stack

      method private push: 'a.bool->'a->'a visitAction =
        fun flag x ->
        Stack.push flag unspecified_stack;
        ChangeDoChildrenPost
          (x,fun x -> ignore(Stack.pop unspecified_stack); x)


      method! vblock b =
        b.bstmts <-
          List.rev
            (List.fold_left(
                fun res s ->
                  match s.skind with
                  | Block b when
                      (not (Stack.top unspecified_stack)) &&
                      b.battrs = [] && b.blocals = [] &&
                      s.labels = [] && not (is_annot_next_stmt res)
                    -> List.rev_append b.bstmts res
                  | _ -> s ::res)
                [] b.bstmts);
        DoChildren

      method! vstmt s =
        let ghost = s.ghost in
        let change_label_stmt s s' =
          List.iter
            (function
              | Label (x,_,_) -> H.replace labelStmt x s'
              | Case _ | Default _ -> replace_table <- (s, s') :: replace_table
            ) s.labels;
          s'.labels <- s.labels @ s'.labels
        in
        match s.skind with
        | UnspecifiedSequence [s',_,_,_,_] ->
          change_label_stmt s s';
          ChangeDoChildrenPost(s', fun x -> x)
        | UnspecifiedSequence [] ->
          let s' = mkEmptyStmt ~ghost ~valid_sid ~loc:(cabslu "_useq") () in
          change_label_stmt s s';
          ChangeTo s';
        | UnspecifiedSequence _ -> self#push true s
        | Block { battrs = []; blocals = []; bstmts = [s']} ->
          change_label_stmt s s';
          ChangeDoChildrenPost (s', fun x -> x)
        | Block _ | If _ | Loop _
        | TryFinally _ | TryExcept _ | Throw _ | TryCatch _ ->
          self#push false s
        | Switch _ ->
          let change_cases stmt =
            match stmt.skind with
            | Switch(e,body,cases,loc) ->
              let newcases =
                List.map
                  (fun s ->
                     try List.assq s replace_table
                     with Not_found -> s)
                  cases
              in
              stmt.skind <- Switch(e,body,newcases,loc);
              ignore (Stack.pop unspecified_stack);
              stmt
            | _ -> assert false
          in Stack.push false unspecified_stack;
          ChangeDoChildrenPost(s,change_cases)
        | Instr _ | Return _ | Goto _ | Break _
        | Continue _ -> DoChildren
    end

  let mkFunctionBody ~ghost (c: chunk) : block =
    if c.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Switch cases not inside a switch statement\n";
    (* cleanup empty blocks and unspecified sequences.
       This can change some labels (the one attached to removed blocks),
       so it has to be done before resolveGotos. *)
    let res = visitCilBlock (new cleanUnspecified) (c2block ~ghost c) in
    H.iter (find_stmt res) labelStmt; resolveGotos (); initLabels (); res

  let add_reads ~ghost loc r c = match r with
    | [] -> c
    | _ :: _ -> c +++ (mkEmptyStmt ~ghost ~valid_sid ~loc (), [],[], r)

end

open BlockChunk

(* To avoid generating backward gotos, we treat while loops as non-while ones,
 * adding a marker for continue. (useful for Jessie) *)
let doTransformWhile = ref false

let setDoTransformWhile () = doTransformWhile := true

(* To avoid generating forward ingoing gotos, we translate conditionals in
 * an alternate way. (useful for Jessie) *)
let doAlternateConditional = ref false

let setDoAlternateConditional () = doAlternateConditional := true

(************ Labels ***********)
(* Since we turn dowhile and for loops into while we need to take care in
 * processing the continue statement. For each loop that we enter we place a
 * marker in a list saying what kinds of loop it is. When we see a continue
 * for a Non-while loop we must generate a label for the continue *)

type loopstate =
    While of string ref
  | NotWhile of string ref

let continues : loopstate list ref = ref []

(* Sometimes we need to create new label names *)
let newLabelName ghost base = fst (newAlphaName ghost false "label" base)

let continueOrLabelChunk ~ghost (l: location) : chunk =
  match !continues with
  | [] -> abort_context "continue not in a loop"
  | While lr :: _ ->
    if !doTransformWhile then
      begin
        if !lr = "" then begin
          lr := newLabelName ghost "__Cont"
        end;
        gotoChunk ~ghost !lr l
      end
    else continueChunk ~ghost l
  | NotWhile lr :: _ ->
    if !lr = "" then begin
      lr := newLabelName ghost "__Cont"
    end;
    gotoChunk ~ghost !lr l

(* stack of statements inside which break instruction can be found. *)
let break_env = Stack.create ()

let enter_break_env () = Stack.push () break_env

let breakChunk ~ghost l =
  if Stack.is_empty break_env then
    abort_context "break outside of a loop or switch";
  breakChunk ~ghost l

let exit_break_env () =
  if Stack.is_empty break_env then
    Kernel.fatal ~current:true
      "trying to exit a breakable env without having entered it";
  ignore (Stack.pop break_env)

(* In GCC we can have locally declared labels. *)
let genNewLocalLabel ghost l =
  (* Call the newLabelName to register the label name in the alpha conversion
   * table. *)
  let l' = newLabelName ghost l in
  (* Add it to the environment *)
  addLocalToEnv ghost (kindPlusName "label" l) (EnvLabel l');
  l'

let lookupLabel ghost l =
  try
    let env = if ghost then ghost_env else env in
    match Datatype.String.Hashtbl.find env (kindPlusName "label" l) with
    | EnvLabel l', _ -> l'
    | _ -> raise Not_found
  with Not_found -> l

class gatherLabelsClass : Cabsvisit.cabsVisitor = object (self)
  inherit Cabsvisit.nopCabsVisitor

  (* We have to know if a label is local to know if it is an error if
   * another label with the same name exists. But a local label can be
   * declared multiple times at different nesting depths. Since a
   * Hashtbl can maintain multiple mappings, we add and remove local
   * labels as we visit their blocks. We map each local label to a
   * location option indicating where it was defined (if it has been).
   * This enables us to raise an error if a local label is defined
   * twice, and we can issue warnings if local labels are declared but
   * never defined. *)
  val localLabels : (string, location option) H.t = H.create 5

  method private addLocalLabels blk =
    List.iter (fun lbl -> H.add localLabels lbl None) blk.blabels
  method private removeLocalLabels blk =
    List.iter
      (fun lbl ->
         if H.find localLabels lbl = None then
           Kernel.warning ~current:true
             "Local label %s declared but not defined" lbl;
         H.remove localLabels lbl)
      blk.blabels

  method! vblock blk =
    (* Add the local labels, process the block, then remove the local labels *)
    self#addLocalLabels blk;
    ChangeDoChildrenPost (blk, fun _ -> (self#removeLocalLabels blk; blk))

  method! vstmt s =
    let open Current_loc.Operators in
    let<> UpdatedCurrentLoc = get_statementloc s in
    (match s.stmt_node with
     | LABEL (lbl,_,_) ->
       (try
          (match H.find localLabels lbl with
           | Some oldloc ->
             Kernel.error ~once:true ~current:true
               "Duplicate local label '%s' (previous definition was at %a)"
               lbl Cil_printer.pp_location oldloc
           | None ->
             (* Mark this label as defined *)
             H.replace localLabels lbl (Some (Current_loc.get())))
        with Not_found -> (* lbl is not a local label *)
          let newname, oldloc =
            newAlphaName s.stmt_ghost false "label" lbl
          in
          if newname <> lbl then
            Kernel.error ~once:true ~current:true
              "Duplicate label '%s' (previous definition was at %a)"
              lbl Cil_printer.pp_location oldloc)
     | _ -> ());
    DoChildren
end


(* Enter all the labels into the alpha renaming table to prevent
   duplicate labels when unfolding short-circuiting logical operators
   and when creating labels for (some) continue statements. *)
class registerLabelsVisitor = object
  inherit Cabsvisit.nopCabsVisitor

  method! vstmt s =
    let currentLoc = convLoc (Cabshelper.get_statementloc s) in
    (match s.stmt_node with
     | Cabs.LABEL (lbl,_,_) ->
       Alpha.registerAlphaName ~alphaTable
         ~lookupname:(kindPlusName "label" lbl) ~data:currentLoc
     | _ -> ());
    DoChildren
end



(* Maps local variables that are variable sized arrays to the expression that
 * denotes their length *)
let varSizeArrays : exp IH.t = IH.create 17

(**** EXP actions ***)
type expAction =
    ADrop                               (* Drop the result. Only the
                                         * side-effect is interesting *)
  | AType                               (* Only the type of the result
                                           is interesting.  *)
  | ASet of bool * lval * lval list * typ
  (* Put the result in a given lval,
   * provided it matches the type. The
   * type is the type of the lval.
   * the flag indicates whether this
   * should be considered in the
   * effects of current
   * chunk.
   * The lval list is the list of location that are read to evaluate
   * the location of the lval.
   * The location of lval is guaranteed
   * not to depend on its own value,
   * e.g. p[p[0]] when p[0] is initially
   * 0, so the location won't change
   * after assignment.
  *)
  | AExp of typ option                  (* Return the exp as usual.
                                         * Optionally we can specify an
                                         * expected type. This is useful for
                                         * constants. The expected type is
                                         * informational only, we do not
                                         * guarantee that the converted
                                         * expression has that type.You must
                                         * use a doCast afterwards to make
                                         * sure. *)
  | AExpLeaveArrayFun                   (* Do it like an expression, but do
                                         * not convert arrays of functions
                                         * into pointers *)


type expConst =
  | CConst
  | CMayConst
  | CNoConst

(*** Result of compiling conditional expressions *)
type condExpRes =
    CEExp of chunk * exp (* Do a chunk and then an expression *)
  | CEAnd of condExpRes * condExpRes
  | CEOr  of condExpRes * condExpRes
  | CENot of condExpRes

let rec clean_up_cond_locals =
  function
  | CEAnd(ce1, ce2) | CEOr(ce1, ce2) ->
    clean_up_cond_locals ce1; clean_up_cond_locals ce2
  | CENot ce -> clean_up_cond_locals ce
  | CEExp (c,_) -> clean_up_chunk_locals c

(* We have our own version of add_attributes that does not allow duplicates *)
let cabsAddAttributes al0 (al: attributes) : attributes =
  if al0 == [] then al else
    List.fold_left
      (fun acc ((an, _) as a) ->
         (* See if the attribute is already in there *)
         match Ast_attributes.filter an acc with
         | [] -> Ast_attributes.add a acc (* Nothing with that name *)
         | a' :: _ ->
           if Cil_datatype.Attribute.equal a a' then
             acc (* Already in *)
           else begin
             Kernel.debug ~level:3
               "Duplicate attribute %a along with %a"
               Cil_printer.pp_attribute a Cil_printer.pp_attribute a' ;
             (* let acc' = drop_attribute an acc in *)
             (* Keep both attributes *)
             Ast_attributes.add a acc
           end)
      al
      al0

(* BY: nothing cabs here, plus seems to duplicate most of Cil.typeAddAttributes *)
(* see [combineAttributes] above for details about the [what] argument *)
let rec cabsTypeCombineAttributes what a0 t =
  let combine = combineAttributes what in
  begin
    match a0 with
    | [] ->
      (* no attributes, keep same type *)
      t
    | _ ->
      (* anything else: add a0 to existing attributes *)
      let add (a: attributes) = combine a0 a in
      let tattr = add t.tattr in
      match t.tnode with
      | TVoid -> mk_tvoid ~tattr ()
      | TInt ik ->
        (* Here we have to watch for the mode attribute *)
        (* sm: This stuff is to handle a GCC extension where you can request integers*)
        (* of specific widths using the "mode" attribute syntax; for example:     *)
        (*   typedef int int8_t __attribute__ ((__mode__ (  __QI__ ))) ;          *)
        (* The cryptic "__QI__" defines int8_t to be 8 bits wide, instead of the  *)
        (* 32 bits you'd guess if you didn't know about "mode".  The relevant     *)
        (* testcase is test/small2/mode_sizes.c, and it was inspired by my        *)
        (* /usr/include/sys/types.h.                                              *)
        (*                                                                        *)
        (* A consequence of this handling is that we throw away the mode          *)
        (* attribute, which we used to go out of our way to avoid printing anyway.*)
        let ik', a0' =
          (* Go over the list of new attributes and come back with a
           * filtered list and a new integer kind *)
          List.fold_left
            (fun (ik', a0') a0one ->
               match a0one with
               | ("mode", [ACons(mode,[])]) -> begin
                   (* (trace "gccwidth" (dprintf "I see mode %s applied to an int type\n"
                                        mode )); *)
                   (* the cases below encode the 32-bit assumption.. *)
                   match (ik', mode) with
                   | (IInt, "__QI__")      -> (IChar, a0')
                   | (IInt, "__byte__")    -> (IChar, a0')
                   | (IInt, "__HI__")      -> (IShort,  a0')
                   | (IInt, "__SI__")      -> (IInt, a0')   (* same as t *)
                   | (IInt, "__word__")    -> (IInt, a0')
                   | (IInt, "__pointer__") -> (IInt, a0')
                   | (IInt, "__DI__")      -> (ILongLong, a0')

                   | (IUInt, "__QI__")     -> (IUChar, a0')
                   | (IUInt, "__byte__")   -> (IUChar, a0')
                   | (IUInt, "__HI__")     -> (IUShort, a0')
                   | (IUInt, "__SI__")     -> (IUInt, a0')
                   | (IUInt, "__word__")   -> (IUInt, a0')
                   | (IUInt, "__pointer__")-> (IUInt, a0')
                   | (IUInt, "__DI__")     -> (IULongLong, a0')

                   | _ ->
                     Kernel.error ~once:true ~current:true
                       "GCC width mode %s applied to unexpected type, \
                        or unexpected mode"
                       mode;
                     (ik', a0one :: a0')

                 end
               | _ -> (ik', a0one :: a0'))
            (ik, [])
            a0
        in
        mk_tint ~tattr:(combine a0' t.tattr) ik'

      | TFloat fk  -> mk_tfloat ~tattr fk
      | TEnum enum -> mk_tenum  ~tattr enum
      | TPtr t     -> mk_tptr   ~tattr t
      | TFun (t, args, isva) -> mk_tfun ~tattr t args isva
      | TComp comp -> mk_tcomp  ~tattr comp
      | TNamed ti  -> mk_tnamed ~tattr ti
      | TBuiltin_va_list -> mk_tbuiltin ~tattr ()
      | TArray (bt, l) ->
        let att_elt, att_typ = Ast_attributes.split_array_attributes a0 in
        let bt' = cabsArrayPushAttributes what att_elt bt in
        let tattr = combineAttributes what att_typ t.tattr in
        mk_tarray ~tattr bt' l
  end
and cabsArrayPushAttributes what al t =
  match t.tnode with
  | TArray (bt, l) ->
    let bt' = cabsArrayPushAttributes what al bt in
    mk_tarray ~tattr:t.tattr bt' l
  | _ -> cabsTypeCombineAttributes what al t

let cabsTypeAddAttributes =
  cabsTypeCombineAttributes CombineOther


(* Do types *)

let get_qualifiers t = Ast_attributes.filter_qualifiers (Cil.typeAttrs t)

let castTo ?context ?(fromsource=false)
    (oldt : typ) (newt : typ) (e : exp) : (typ * exp) =
  mkCastTGen ?context ~force:fromsource ~fromsource ~oldt ~newt e

(* Create and cache varinfo's for globals. Starts with a varinfo but if the
 * global has been declared already it might come back with another varinfo.
 * Returns the varinfo to use (might be the old one), and an indication
 * whether the variable exists already in the environment *)
let makeGlobalVarinfo (isadef: bool) (vi: varinfo) : varinfo * bool =
  let res =
    try (* See if already defined, in the global environment. We could also
         * look it up in the whole environment but in that case we might see a
         * local. This can happen when we declare an extern variable with
         * global scope but we are in a local scope. *)
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "makeGlobalVarinfo isadef=%B vi.vname=%s(%d), vreferenced=%B"
        isadef vi.vname vi.vid vi.vreferenced;
      (* This may throw an exception Not_found
         Note that we always search in all the context, including ghost *)
      let oldvi, oldloc = lookupGlobalVar true vi.vname in
      if oldvi.vghost <> vi.vghost then
        abort_context "Inconsistent ghost specification for %s.@ \
                       Previous declaration was at: %a"
          vi.vname Cil_datatype.Location.pretty oldloc ;

      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "  %s(%d) already in the env at loc %a"
        vi.vname oldvi.vid Cil_printer.pp_location oldloc;
      (* It was already defined. We must reuse the varinfo. But clean up the
       * storage.  *)
      let newstorage = (* See 6.2.2 *)
        match oldvi.vstorage, vi.vstorage with
        | Extern, NoStorage when isadef -> NoStorage
        (* the case above is not strictly C standard, but will not accept
           more program and is more compatible with old implicit
           quasi-invariant that Extern == not defined. *)
        | Extern, (Extern | NoStorage) -> Extern
        | NoStorage, Extern -> if oldvi.vdefined then NoStorage else Extern
        | NoStorage, NoStorage -> NoStorage
        | Static, Extern -> Static (* 6.2.2§4 *)
        | Static, NoStorage when Cil.isFunctionType vi.vtype -> Static
        | _ ->
          if vi.vstorage != oldvi.vstorage then
            Kernel.error ~current:true
              "Inconsistent storage specification for %s. \
               Previous declaration: %a"
              vi.vname Cil_printer.pp_location oldloc;
          vi.vstorage
      in
      (* if _all_ declaration have the inline specifier, and none
         is extern we'll end up with an inline definition which must have
         a special treatment (see C11 6.7.4§7) *)
      oldvi.vinline <- oldvi.vinline && vi.vinline;
      (* If the new declaration has a section attribute, remove any
       * preexisting section attribute. This mimics behavior of gcc that is
       * required to compile the Linux kernel properly. *)
      if Ast_attributes.contains "section" vi.vattr then
        oldvi.vattr <- Ast_attributes.drop "section" oldvi.vattr;
      (* Before combining attributes, we need to check compatibility between
         qualifiers *)
      begin
        try
          let oldquals = get_qualifiers oldvi.vtype in
          let quals = get_qualifiers vi.vtype in
          if not (Cil_datatype.Attributes.equal oldquals quals) then
            raise (Cannot_combine
                     (Format.asprintf
                        "different qualifiers:@ '%a' and '%a'"
                        Cil_printer.pp_attributes oldquals
                        Cil_printer.pp_attributes quals));
          (* Union the attributes *)
          oldvi.vattr <- cabsAddAttributes oldvi.vattr vi.vattr;
          let what =
            if isadef then
              CombineFundef (Ast_attributes.contains "FC_OLDSTYLEPROTO" vi.vattr)
            else CombineOther
          in
          let mytype = combineTypes what oldvi.vtype vi.vtype in
          if not (Cil_datatype.Typ.equal oldvi.vtype vi.vtype)
          then begin
            DifferentDeclHook.apply (oldvi,vi);
            (* note: combineTypes is (purposedly) not very strict, so we
               use compatibleTypes here to perform more strict checks and
               raise Cannot_combine if necessary. However, due to old-style
               prototypes in GCC machdeps, we must support eccentric cases,
               for which we perform no such additional verification. *)
            if not (Ast_attributes.contains "FC_OLDSTYLEPROTO" vi.vattr) then
              ignore (compatibleTypes oldvi.vtype vi.vtype)
          end;
          Cil.update_var_type oldvi mytype;
        with Cannot_combine reason ->
          abort_context
            "Declaration of %s does not match previous declaration from \
             %a (%s)."
            vi.vname Cil_printer.pp_location oldloc reason
      end;
      (* Update the storage and vdecl if useful. Do so only after the hooks have
         been applied, as they may need to read those fields *)
      if oldvi.vstorage <> newstorage then begin
        oldvi.vstorage <- newstorage;
        (* Also update the location; [vi.vdecl] is a better
           declaration/definition site for [vi]. *)
        oldvi.vdecl <- vi.vdecl;
      end;
      (* Let's mutate the formals vid's name attribute and type for function
         prototypes. Logic specifications refer to the varinfo in this table. *)
      begin
        match vi.vtype.tnode with
        | TFun (_,Some formals , _) ->
          (try
             let old_formals_env = getFormalsDecl oldvi in
             List.iter2
               (fun old ((name,typ,attr) as decl) ->
                  let new_ghost = Cil.isGhostFormalVarDecl decl in
                  if old.vghost <> new_ghost then
                    abort_context
                      "Function %a redeclared with incompatible ghost status \
                       in formals (original declaration was at %a)"
                      Cil_datatype.Varinfo.pretty vi
                      Cil_datatype.Location.pretty oldloc
                  else if name <> "" then begin
                    Kernel.debug ~dkey:Kernel.dkey_typing_global
                      "replacing formal %s with %s" old.vname name;
                    old.vname <- name;
                    if not oldvi.vdefined || isadef then begin
                      Cil.update_var_type old typ;
                      old.vattr <- attr;
                    end;
                    match old.vlogic_var_assoc with
                    | None -> ()
                    | Some old_lv -> old_lv.lv_name <- name
                  end)
               old_formals_env
               formals
           with Not_found -> Cil.setFormalsDecl oldvi vi.vtype)
        | _ -> ()
      end ;
      (* if [isadef] is true, [vi] is a definition.  *)
      if isadef then begin
        (* always favor the location of the definition.*)
        oldvi.vdecl <- vi.vdecl;
        oldvi.vdefined <- true;
        oldvi.vattr <- fc_stdlib_attribute oldvi.vattr
      end;
      (* notice that [vtemp] is immutable, and cannot be updated. Hopefully,
         temporaries have sufficiently fresh names that this is not a problem *)
      oldvi, true
    with Not_found -> begin (* A new one.  *)
        Kernel.debug ~dkey:Kernel.dkey_typing_global
          "  %s not in the env already" vi.vname;
        (* Announce the name to the alpha conversion table. This will not
         * actually change the name of the vi. See the definition of
         * alphaConvertVarAndAddToEnv *)
        let vi = alphaConvertVarAndAddToEnv true vi in
        (* update the field [vdefined] *)
        if isadef then vi.vdefined <- true;
        vi.vattr <- Ast_attributes.drop "FC_OLDSTYLEPROTO" vi.vattr;
        vi.vattr <- fc_stdlib_attribute vi.vattr;
        vi, false
      end
  in
  NewGlobalHook.apply res;
  res

type args_or_argtypes = Args of varinfo list | ArgTypes of typ list

(* Register a builtin function *)
let setupBuiltin ?(force_keep=false) name ?spec (resTyp, args_or_argtypes, isva) =
  let funargs, args = match args_or_argtypes with
    | Args args ->
      Some (List.map (fun vi -> (vi.vname, vi.vtype, vi.vattr)) args), args
    | ArgTypes argTypes ->
      let funargs =
        List.mapi (fun i at ->
            ("__x" ^ string_of_int i, at, [Ast_attributes.anonymous_attribute])
          ) argTypes
      in
      Some funargs, List.map makeFormalsVarDecl funargs
  in
  let typ = mk_tfun resTyp funargs isva in
  let v = makeGlobalVar name typ in
  ignore (alphaConvertVarAndAddToEnv true v);
  (* Add it to the file as well *)
  let funspec = match spec with
    | None -> empty_funspec ()
    | Some s -> s
  in
  cabsPushGlobal (GFunDecl (funspec, v, Cil_builtins.builtinLoc));
  Cil.unsafeSetFormalsDecl v args;
  if force_keep then
    v.vattr <- Ast_attributes.add ("FC_BUILTIN",[]) v.vattr;
  v

(*  builtin is never ghost *)
let memoBuiltin ?force_keep ?spec name proto =
  try fst (lookupGlobalVar false name)
  with Not_found -> setupBuiltin ?force_keep ?spec name proto

let vla_alloc_fun () =
  let size_arg =
    Cil.makeVarinfo false true "size" (Machine.sizeof_type ())
  in
  let res_iterm =
    Logic_const.new_identified_term
      (Logic_const.tresult voidPtrType)
  in
  let behavior =
    Cil.mk_behavior ~assigns:(Writes [(res_iterm, From [])])
      ~allocation:(FreeAlloc ([], [res_iterm])) ()
  in
  let spec = { (Cil.empty_funspec ()) with spec_behavior = [behavior]} in
  memoBuiltin ~force_keep:true "__fc_vla_alloc" ~spec
    (voidPtrType, Args [size_arg], false)

let vla_free_fun () =
  let p_arg = Cil.makeVarinfo false true "p" voidPtrType in
  let p_iterm = Logic_const.new_identified_term
      (Logic_const.tvar (Cil.cvar_to_lvar p_arg))
  in
  let behavior =
    Cil.mk_behavior ~assigns:(Writes [])
      ~allocation:(FreeAlloc ([p_iterm], [])) ()
  in
  let spec = { (Cil.empty_funspec ()) with spec_behavior = [behavior]} in
  memoBuiltin ~force_keep:true ~spec "__fc_vla_free"
    (voidType, Args [p_arg], false)

let conditionalConversion (t2: typ) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    let t2' = unrollType t2 in
    let t3' = unrollType t3 in
    match t2'.tnode, t3'.tnode with
    | (TInt _ | TEnum _ | TFloat _), (TInt _ | TEnum _ | TFloat _) ->
      arithmeticConversion t2 t3
    | TComp comp2, TComp comp3
      when comp2.ckey = comp3.ckey -> t2
    | TVoid, TVoid  -> t2
    | TPtr _, TPtr { tnode = TVoid } -> t2
    | TPtr { tnode = TVoid }, TPtr _ -> t3
    | TPtr _, TPtr _ when Cil_datatype.Typ.equal t2 t3 -> t2
    | TPtr _, TInt _  -> t2 (* most likely comparison with 0 *)
    | TInt _, TPtr _ -> t3 (* most likely comparison with 0 *)

    (* When we compare two pointers of different types, we combine them
     * using the same algorithm when combining multiple declarations of
     * a global *)
    | TPtr _, TPtr _ -> begin
        try combineTypes CombineOther t2' t3'
        with Cannot_combine msg -> begin
            Kernel.warning ~current:true "A.QUESTION: %a does not match %a (%s)"
              Cil_datatype.Typ.pretty (unrollType t2) Cil_datatype.Typ.pretty (unrollType t3) msg;
            t2 (* Just pick one *)
          end
      end
    | _, _ ->
      abort_context "invalid implicit conversion from %a to %a"
        Cil_datatype.Typ.pretty t2 Cil_datatype.Typ.pretty t3
  in
  tresult

let logicConditionalConversion t1 t2 =
  match unrollTypeNode t1, unrollTypeNode t2 with
  | TPtr _ , TInt _ | TInt _, TPtr _ ->
    abort_context "invalid implicit conversion from %a to %a"
      Cil_datatype.Typ.pretty t2 Cil_datatype.Typ.pretty t1
  | _ -> conditionalConversion t1 t2

(* Some utilities for doing initializers *)

type preInit =
  | NoInitPre
  | SinglePre of exp * Cil_datatype.Lval.Set.t (* lval reads by the expression*)
  | CompoundPre of int ref (* the maximum used index *)
                   * preInit array ref (* an array with initializers *)

(* internal pretty-printing function for debugging purposes *)
let rec _pp_preInit fmt = function
  | NoInitPre -> Format.fprintf fmt "NoInitPre"
  | SinglePre (e,_) -> Format.fprintf fmt "SinglePre(%a)" Cil_printer.pp_exp e
  | CompoundPre (int_ref, preInit_a_ref) ->
    Format.fprintf fmt "CompoundPre(%d,@[%a@])" !int_ref
      (Pretty_utils.pp_array ~sep:",@ "
         (fun fmt index e -> Format.fprintf fmt "@[[%d -> %a]@]" index _pp_preInit e))
      !preInit_a_ref

(* special case for treating GNU extension on empty compound initializers. *)
let empty_preinit() =
  if Machine.(gccMode () || msvcMode ()) then
    CompoundPre (ref (-1), ref [| |])
  else abort_context "empty initializers %s" (Machdep.allowed_machdep "GCC/MSVC")

(* Set an initializer *)
let rec setOneInit this o preinit =
  let open Current_loc.Operators in
  let<?> UpdatedCurrentLoc =
    match o with
    | Index (e, _) -> Some e.eloc
    | _ -> None
  in
  match o with
  | NoOffset -> preinit
  | _ ->
    let idx, (* Index in the current comp *)
        restoff (* Rest offset *) =
      match o with
      | NoOffset -> assert false
      | Index({enode = Const(CInt64(i,_,_));eloc}, off) ->
        let i' = Current_loc.with_loc eloc to_integer i in
        i', off
      | Field (f, off) ->
        (* Find the index of the field *)
        let rec loop (idx: int) = function
          | [] ->
            (* We have managed to build a fieldinfo whose fcomp field is a
               compinfo that does not include the corresponding field. This
               is not a typechecking error, but an internal failure of cabs2cil
            *)
            Kernel.fatal ~current:true
              "Cannot find field %s for initialization of type %s"
              f.fname (Cil.compFullName f.fcomp)
          | f' :: _ when f'.fname = f.fname -> idx
          | _ :: restf -> loop (idx + 1) restf
        in
        loop 0 (Option.value ~default:[] f.fcomp.cfields), off
      | Index({ eloc },_) ->
        abort_context ~loc:eloc "setOneInit: non-constant index"
    in
    let pMaxIdx, pArray =
      match this  with
      | NoInitPre  -> (* No initializer so far here *)
        begin
          try
            ref idx, ref (Array.make (max 32 (idx + 1)) NoInitPre)
          with Invalid_argument _ | Out_of_memory ->
            abort_context
              "array length too large: %d" ((max 32 (idx + 1)))
        end

      | CompoundPre (pMaxIdx, pArray) ->
        if !pMaxIdx < idx then begin
          pMaxIdx := idx;
          (* Maybe we also need to grow the array *)
          let l = Array.length !pArray in
          if l <= idx then begin
            let growBy = max (max 32 (idx + 1 - l)) (l / 2) in
            try
              let newarray = Array.make (growBy + idx) NoInitPre in
              Array.blit !pArray 0 newarray 0 l;
              pArray := newarray
            with Invalid_argument _ | Out_of_memory ->
              abort_context
                "array length too large for Frama-C: %d" (idx)
          end
        end;
        pMaxIdx, pArray
      | SinglePre _ ->
        Kernel.fatal ~current:true "Index %d is already initialized" idx
    in
    assert (idx >= 0 && idx < Array.length !pArray);
    let this' = setOneInit !pArray.(idx) restoff preinit in
    !pArray.(idx) <- this';
    CompoundPre (pMaxIdx, pArray)

(* collect a CIL initializer, given the original syntactic initializer
 * 'preInit'; this returns a type too, since initialization of an array
 * with unspecified size actually changes the array's type
 * (ANSI C, 6.7.8, para 22).
 * Finally, we return the set of lvals that are read for the evaluation of
 * the initializer (for unspecified sequences)
*)
let rec collectInitializer
    reads (* lval already read by the rest of the initializer. *)
    (this: preInit)
    (thistype: typ) ~(parenttype: typ) :
  (init * typ * Cil_datatype.Lval.Set.t) =
  (* parenttype is used to identify a tentative flexible array member
     initialization *)
  let dkey = Kernel.dkey_typing_init in
  let loc = Current_loc.get() in
  if this = NoInitPre then begin
    Kernel.debug ~dkey "zero-initializing object of type %a"
      Cil_datatype.Typ.pretty thistype;
    (makeZeroInit ~loc thistype), thistype, reads
  end else
    match unrollType thistype, this with
    | _ , SinglePre (e, r) ->
      Kernel.debug ~dkey "Initializing object of type %a to %a"
        Cil_datatype.Typ.pretty thistype Cil_printer.pp_exp e;
      SingleInit e, thistype, Cil_datatype.Lval.Set.union r reads
    | { tnode = TArray (bt, leno); tattr }, CompoundPre (pMaxIdx, pArray) ->
      Kernel.debug ~dkey
        "Initialization of an array object of type %a with index max %d"
        Cil_datatype.Typ.pretty thistype !pMaxIdx;
      let len, initializer_len_used =
        (* normal case: use array's declared length, newtype=thistype *)
        match leno with
        | Some len -> begin
            match constFoldToInt len with
            | Some ni when Integer.ge ni Integer.zero -> to_integer ni, false
            | _ -> (* VLA cannot have initializers, and this should have
                      been captured beforehand. *)
              Kernel.fatal "Trying to initialize a variable-length array"
          end
        | _ ->
          (* unsized array case, length comes from initializers *)
          (!pMaxIdx + 1), true
      in
      if !pMaxIdx >= len then
        abort_context
          "collectInitializer: too many initializers(%d >= %d)"
          (!pMaxIdx+1) len;
(*
        (* len could be extremely big. So omit the last initializers, if they
         * are many (more than 16). doInit will take care of that by
         * mem-setting everything to 0 in that case.
         *)
        let endAt =
          if len - 1 > !pMaxIdx + 16 then
            !pMaxIdx
          else
            len - 1
        in
        (* Make one zero initializer to be used next *)
        let oneZeroInit = makeZeroInit ~loc bt in
        let rec collect (acc: (offset * init) list) (idx: int) =
          if idx = -1 then acc
          else
            let thisi =
              if idx > !pMaxIdx then oneZeroInit
              else (fst (collectInitializer !pArray.(idx) bt))
            in
            collect ((Index(integer ~loc idx,NoOffset), thisi) :: acc) (idx - 1)
        in
*)
      let collect_one_init v (idx,init,typ,reads,len_used) =
        match v with
        | NoInitPre -> (idx-1,init,typ,reads,len_used)
        | _ ->
          let (vinit,typ', reads') =
            collectInitializer reads v typ ~parenttype:typ
          in
          let len_used =
            len_used || not (Cil_datatype.Typ.equal typ typ')
          in
          (idx-1,
           (Index (integer ~loc idx,NoOffset), vinit)::init,
           typ',
           Cil_datatype.Lval.Set.union reads' reads,
           len_used)
      in
      let (_,init,typ, reads, len_used) =
        Array.fold_right collect_one_init
          !pArray (Array.length !pArray - 1, [], bt, reads,initializer_len_used)
      in
      let newtype =
        (* detect flexible array member initialization *)
        if Cil.isUnsizedArrayType thistype &&
           Cil.isStructType parenttype &&
           len > 0
        then
          begin
            (* incomplete array type inside a struct => FAM, with
               a non-empty initializer (len > 0) *)
            Kernel.debug ~dkey
              "Detected initialization of a flexible array member \
               (length %d, parenttype %a)" len Cil_datatype.Typ.pretty parenttype;
            Kernel.error ~once:true ~current:true
              "static initialization of flexible array members is an \
               unsupported GNU extension";
            mk_tarray ~tattr typ None
          end
        else
          begin
            (* not a flexible array member *)
            if len = 0 && not Machine.(gccMode () || msvcMode ()) then
              Kernel.error ~once:true ~current:true
                "arrays of size zero not supported in C99@ \
                 (only allowed as compiler extensions)";
            mk_tarray ~tattr typ (Some (integer ~loc len))
          end
      in
      CompoundInit (newtype, (* collect [] endAt*)init),
      (* If the sizes of the initializers have not been used anywhere,
         we can fold back an eventual typedef. Otherwise, push the
         attributes to the elements of the array *)
      (if len_used then newtype else thistype),
      reads

    | { tnode = TComp comp } as t,
      CompoundPre (pMaxIdx, pArray) when comp.cstruct ->
      Kernel.debug ~dkey
        "Initialization of an object of type %a with at least %d components"
        Cil_datatype.Typ.pretty thistype !pMaxIdx;
      let rec collect (idx: int) reads = function
          [] -> [], reads
        | [ _ ] when Cil.has_flexible_array_member t && idx > !pMaxIdx ->
          (* Do not add an empty initializer to the FAM, making an ill-formed
             AST. An explicit initialization is allowed in gcc-mode. *)
          [], reads
        | f :: restf ->
          if f.fname = missingFieldName then
            collect (idx + 1) reads restf
          else
            let thisi, reads' =
              if idx > !pMaxIdx then
                makeZeroInit ~loc f.ftype, reads
              else
                collectFieldInitializer
                  reads !pArray.(idx) f ~parenttype:thistype
            in
            let rest, reads' = collect (idx+1) reads' restf in
            (Field(f, NoOffset), thisi) :: rest, reads'
      in
      let init, reads =
        collect 0 reads (Option.value ~default:[] comp.cfields) in
      CompoundInit (thistype, init), thistype, reads

    | { tnode = TComp comp }, CompoundPre (pMaxIdx, pArray) when not comp.cstruct ->
      Kernel.debug ~dkey
        "Initialization of an object of type %a with at least %d components"
        Cil_datatype.Typ.pretty thistype !pMaxIdx;
      (* Find the field to initialize *)
      let rec findField (idx: int) = function
        | [] ->
          (* This code should only be reachable with GCC/MSVC machdeps *)
          if Machine.(gccMode () || msvcMode ()) then
            [], reads
          else
            Kernel.fatal ~current:true "collectInitializer: union"
        | _ :: rest when idx < !pMaxIdx && !pArray.(idx) = NoInitPre ->
          findField (idx + 1) rest
        | f :: _ when idx = !pMaxIdx ->
          let init, reads =
            collectFieldInitializer reads !pArray.(idx) f ~parenttype:thistype
          in
          [ (Field(f, NoOffset), init) ], reads

        | _ ->
          abort_context "Can initialize only one field for union"
      in
      (* CompoundPre is initialized with pMaxId = -1 for empty compound init
         (cf. empty_preinit), so we need to check if it is greater than 0
         instead of different. *)
      if Machine.msvcMode () && !pMaxIdx > 0 then
        Kernel.warning ~current:true
          "On MSVC we can initialize only the first field of a union";
      let init, reads = findField 0 (Option.value ~default:[] comp.cfields) in
      CompoundInit (thistype, init), thistype, reads

    | _ -> Kernel.fatal ~current:true "collectInitializer"

and collectFieldInitializer
    reads
    (this: preInit)
    (f: fieldinfo) ~(parenttype: typ) =
  (* collect, and rewrite type *)
  let init,newtype,reads =
    (collectInitializer reads this f.ftype ~parenttype)
  in
  f.ftype <- newtype;
  init, reads

type stackElem =
    InArray of offset * typ * int * int ref (* offset of parent, base type,
                                             * length, current index. If the
                                             * array length is unspecified we
                                             * use Int.max_int  *)
  | InComp  of offset * compinfo * offset list (* offset of parent,
                                                     base comp, current fields *)


(* A subobject is given by its address. The address is read from the end of
 * the list (the bottom of the stack), starting with the current object *)
type subobj = { mutable stack: stackElem list; (* With each stack element we
                                                * store the offset of its
                                                * PARENT  *)
                mutable eof: bool; (* The stack is empty and we reached the
                                    * end *)
                mutable soTyp: typ; (* The type of the subobject. Set using
                                     * normalSubobj after setting stack. *)
                mutable soOff: offset; (* The offset of the subobject. Set
                                        * using normalSubobj after setting
                                        * stack.  *)
                curTyp: typ; (* Type of current object. See ISO for
                              * the definition of the current object *)
                curOff: offset; (* The offset of the current obj *)
                host: varinfo; (* The host that we are initializing.
                                * For error messages *)
              }

(* maps vid to visitor used to perform renaming on function spec when there's
   a spec on a declaration and a definition for the function. This is done after
   typing.
*)
let alpha_renaming = Hashtbl.create 59

let rename_spec = function
  | GFunDecl(spec,v,_) ->
    (try
       let alpha = Hashtbl.find alpha_renaming v.vid in
       ignore (Cil.visitCilFunspec alpha spec)
     with Not_found -> ())
  | _ -> ()

(* Make a subobject iterator *)
let rec makeSubobj
    (host: varinfo)
    (curTyp: typ)
    (curOff: offset) =
  let so =
    { host = host; curTyp = curTyp; curOff = curOff;
      stack = []; eof = false;
      (* The next are fixed by normalSubobj *)
      soTyp = voidType; soOff = NoOffset } in
  normalSubobj so;
  so

(* Normalize a stack so the we always point to a valid subobject. Do not
 * descend into type *)
and normalSubobj (so: subobj) : unit =
  match so.stack with
  | [] -> so.soOff <- so.curOff; so.soTyp <- so.curTyp
  (* The array is over *)
  | InArray (parOff, bt, leno, current) :: rest ->
    if leno = !current then begin (* The array is over *)
      Kernel.debug ~dkey:Kernel.dkey_typing_init "Past the end of array";
      so.stack <- rest;
      advanceSubobj so
    end else begin
      so.soTyp <- bt;
      so.soOff <-
        addOffset
          (Index(integer ~loc:(Current_loc.get()) !current, NoOffset))
          parOff
    end

  (* The fields are over *)
  | InComp (parOff, compinfo, nextflds) :: rest ->
    if nextflds == [] then begin (* No more fields here *)
      Kernel.debug ~dkey:Kernel.dkey_typing_init "Past the end of structure";
      so.stack <- rest;
      advanceSubobj so
    end else begin
      let fst = List.hd nextflds
      and baseTyp = mk_tcomp compinfo in
      so.soTyp <- Cil.typeOffset baseTyp fst;
      so.soOff <- addOffset fst parOff
    end

(* Advance to the next subobject. Always apply to a normalized object *)
and advanceSubobj (so: subobj) : unit =
  if so.eof then Kernel.fatal ~current:true "advanceSubobj past end";
  match so.stack with
  | [] ->
    Kernel.debug ~dkey:Kernel.dkey_typing_init "Setting eof to true";
    so.eof <- true
  | InArray (_, _, _, current) :: _ ->
    Kernel.debug ~dkey:Kernel.dkey_typing_init
      "  Advancing to [%d]" (!current + 1);
    (* so.stack <- InArray (parOff, bt, leno, current + 1) :: rest; *)
    incr current;
    normalSubobj so

  (* The fields are over *)
  | InComp (parOff, comp, nextflds) :: rest ->
    let fi, flds' =
      match nextflds with
      | Field (fi,_) :: flds' -> fi, flds'
      | _ -> Kernel.fatal ~current:true "advanceSubobj"
    in
    Kernel.debug ~dkey:Kernel.dkey_typing_init
      "Advancing past .%s" fi.fname;
    so.stack <- InComp(parOff, comp, flds') :: rest;
    normalSubobj so

let anonCompFieldNameId = ref 0
let anonCompFieldName = "__anonCompField"

(* Find the fields to initialize in a composite. *)
let fieldsToInit
    (comp: compinfo)
    (designator: string option)
  : offset list =
  (* Traversal of the comp fields (also goes through anonymous comp)
     the resulting fields are in reverse order *)
  let rec add_comp (offset : offset) (comp : compinfo) acc =
    let in_union = not comp.cstruct in
    add_fields offset in_union (Option.value ~default:[] comp.cfields) acc
  and add_fields (offset : offset) (in_union : bool) (l : fieldinfo list) acc =
    match l with
    | [] -> acc
    | f :: l ->
      let (found, _ as acc) = add_field offset f acc in
      if found && in_union
      then acc (* only consider one field in an union - stop if we found it *)
      else add_fields offset in_union l acc
  and add_field (offset : offset) (f : fieldinfo) (found, loff as acc) =
    (* update current offset *)
    let offset = Cil.addOffset (Field (f, NoOffset)) offset in
    (* Ignore anonymous non-comp fields *)
    if f.fname = missingFieldName then
      acc
      (* if we have already found the designator, just append the current field *)
    else if found then
      found, offset :: loff
      (* if this field is an anonymous comp, search for the designator inside *)
    else if prefix anonCompFieldName f.fname && not found
            && f.forig_name <> f.fname then
      match unrollTypeNode f.ftype with
      | TComp comp ->
        add_comp offset comp acc (* go deeper inside *)
      | _ ->
        abort_context "unnamed field type is not a struct/union"
        (* does this field match the designator ? *)
    else match designator with
      | Some fn when f.fname = fn -> (true, [offset])
      | _ -> acc
  in
  let found, r = add_comp NoOffset comp (designator = None, []) in
  begin if not found then
      let fn = Option.get designator in
      abort_context "Cannot find designated field %s" fn;
  end;
  List.rev r

let integerArrayLength (leno: exp option) : int =
  match leno with
  | None -> max_int
  | Some len ->
    try lenOfArray leno
    with
    | LenOfArray cause ->
      abort_context ~loc:len.eloc
        "Array length %a is %a: no explicit initializer allowed."
        Cil_printer.pp_exp len Cil.pp_incorrect_array_length cause

let find_field_offset cond (fidlist: fieldinfo list) : offset =
  (* Depth first search for the field. This appears to be what GCC does.
   * MSVC checks that there are no ambiguous field names, so it does not
   * matter how we search *)
  let rec search = function
      [] -> raise Not_found
    | fid :: _ when cond fid ->
      Field(fid, NoOffset)
    | fid :: rest when prefix anonCompFieldName fid.fname -> begin
        match unrollTypeNode fid.ftype with
        | TComp ci ->
          (try
             let off = search (Option.value ~default:[] ci.cfields) in
             Field(fid,off)
           with Not_found -> search rest  (* Continue searching *))
        | _ ->
          abort_context "unnamed field type is not a struct/union"
      end
    | _ :: rest -> search rest
  in
  search fidlist

let findField n comp =
  try
    find_field_offset (fun x -> x.fname = n) (Option.value ~default:[] comp.cfields)
  with Not_found ->
    abort_context "Cannot find field %s in type %s" n (Cil.compFullName comp)

(* Utility ***)
let rec replaceLastInList
    (lst: Cabs.expression list)
    (how: Cabs.expression -> Cabs.expression) : Cabs.expression list=
  match lst with
  | [] -> []
  | [e] -> [how e]
  | h :: t -> h :: replaceLastInList t how

let convBinOp (bop: Cabs.binary_operator) : binop =
  match bop with
  | Cabs.ADD -> PlusA
  | Cabs.SUB -> MinusA
  | Cabs.MUL -> Mult
  | Cabs.DIV -> Div
  | Cabs.MOD -> Mod
  | Cabs.BAND -> BAnd
  | Cabs.BOR -> BOr
  | Cabs.XOR -> BXor
  | Cabs.SHL -> Shiftlt
  | Cabs.SHR -> Shiftrt
  | Cabs.EQ -> Eq
  | Cabs.NE -> Ne
  | Cabs.LT -> Lt
  | Cabs.LE -> Le
  | Cabs.GT -> Gt
  | Cabs.GE -> Ge
  | _ -> Kernel.fatal ~current:true "convBinOp"

(**** PEEP-HOLE optimizations ***)

(* Should we collapse [tmp = f(); lv = tmp;] where the result type of [f]
   is [tf], and the [lv] has type [tlv] *)
let allow_return_collapse ~tlv ~tf =
  Cil_datatype.Typ.equal tlv tf ||
  Kernel.DoCollapseCallCast.get () &&
  (match unrollTypeNode tlv, unrollTypeNode tf with
   | TPtr _, TPtr _ -> true (* useful for malloc and others. Could be
                                restricted to void* -> any if needed *)
   | TInt iklv, TInt ikf ->
     Cil.isSigned iklv = Cil.isSigned ikf &&
     Cil.bitsSizeOfBitfield tlv = Cil.bitsSizeOf tf (* && *)
   (* not (Cil.typeHasQualifier "volatile" tlv) *)
   | TFloat fklv, TFloat fkf -> fklv = fkf
   | _, _ -> false
  )

let tcallres f =
  match unrollTypeNode (typeOf f) with
  | TFun (rt, _, _) -> rt
  | _ -> abort_context "Function call to a non-function"

let can_collapse vi vi' destlv cast f =
  let tf = tcallres f in
  not vi.vglob && vi' == vi &&
  String.length vi.vname >= 3 &&
  (* Watch out for the possibility that we have an implied cast in
   * the call *)
  IH.mem callTempVars vi.vid &&
  Cil_datatype.Typ.equal cast (typeOfLval destlv) &&
  (* Depending on circumstances, temp var might either have the type of
     the destination variable or the returned type of f. We collapse in both
     cases. *)
  (Cil_datatype.Typ.equal vi.vtype cast ||
   Cil_datatype.Typ.equal vi.vtype tf)
  &&
  allow_return_collapse ~tf ~tlv:cast

let collapseCallCast (s1,s2) = match s1.skind, s2.skind with
  | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
    Instr (Set(destlv,
               {enode = CastE (newt,
                               {enode = Lval(Var vi', NoOffset)})}, _)) ->
    if can_collapse vi vi' destlv newt f then begin
      s1.skind <- Instr(Call(Some destlv, f, args, l));
      Some [ s1 ]
    end
    else None
  | Instr (Call(Some(Var vi, NoOffset), f, args, l)),
    Instr (Set(destlv, {enode = Lval(Var vi', NoOffset)}, _)) ->
    if can_collapse vi vi' destlv (typeOfLval destlv) f then begin
      s1.skind <- Instr(Call(Some destlv, f, args, l));
      Some [ s1 ]
    end else None
  | Instr (Call (Some (Var vi, NoOffset),
                 ({ enode = Lval (Var f, NoOffset)} as ef), args, l)),
    Instr (
      Local_init(
        destv,
        AssignInit(
          SingleInit
            { enode = CastE(newt, { enode = Lval(Var vi', NoOffset)})}),_))->
    if can_collapse vi vi' (Cil.var destv) newt ef then begin
      s1.skind <- Instr(Local_init(destv, ConsInit(f,args,Plain_func),l));
      Some [s1]
    end else None
  | Instr (Call (Some (Var v1, NoOffset),
                 ({ enode = Lval (Var f, NoOffset)} as ef), args, l)),
    Instr (
      Local_init(
        v2, AssignInit(SingleInit { enode = Lval (Var v1', NoOffset) }),_)) ->
    if can_collapse v1 v1' (Cil.var v2) v2.vtype ef then begin
      s1.skind <- Instr(Local_init(v2, ConsInit(f,args,Plain_func),l));
      Some [ s1 ];
    end else None
  | _ -> None

let afterConversion ~ghost (c: chunk) : chunk =
  (* Now scan the statements and find Instr blocks *)
  (* We want to collapse sequences of the form "tmp = f(); v = tmp". This
   * will help significantly with the handling of calls to malloc, where it
   * is important to have the cast at the same place as the call *)
  let block = c2block ~ghost ~collapse_block:false c in
  let sl =
    if Kernel.DoCollapseCallCast.get () then
      peepHole2 ~aggressive:false collapseCallCast block.bstmts
    else block.bstmts
  in
  (* the call to c2block has taken care of a possible unspecified sequence.
     We do not need to keep track of effects at this level. *)
  let res =
    { c with stmts = (List.rev_map (fun x -> x,[],[],[],[]) sl);
             locals = block.blocals
    }
  in
  (*  Format.eprintf "Before conversion@\n%a@\nAfter conversion@\n%a@\n@."
      d_chunk c d_chunk res;
  *)
  res

(***** Try to suggest a name for the anonymous structures *)
let suggestAnonName (nl: Cabs.name list) =
  match nl with
  | [] -> ""
  | (n, _, _, _) :: _ -> n

(** Optional constant folding of binary operations *)
let optConstFoldBinOp loc machdep bop e1 e2 t =
  if Machine.lower_constants () then
    constFoldBinOp ~loc machdep bop e1 e2 t
  else
    new_exp ~loc (BinOp(bop, e1, e2, t))

let integral_cast ty t =
  raise
    (Failure
       (Format.asprintf "term %a has type %a, but %a is expected"
          Cil_printer.pp_term t Cil_printer.pp_logic_type Linteger Cil_datatype.Typ.pretty ty))

(* Exception raised by the instance of Logic_typing local to this module.
   See document of [error] below. *)
exception LogicTypeError of location * string

module C_logic_env =
struct
  let nb_loop = ref 0
  let is_loop () = !nb_loop > 0
  let anonCompFieldName = anonCompFieldName
  let conditionalConversion = logicConditionalConversion
  let find_macro _ = raise Not_found
  let find_var ?label var =
    let find_from_curr_env test =
      (* logic has always access to the ghost variables. *)
      match Datatype.String.Hashtbl.find ghost_env var with
      | EnvVar vi, _ when test vi -> cvar_to_lvar vi
      | _ -> raise Not_found
    in
    match label with
    | None -> find_from_curr_env (fun _ -> true)
    | Some "Here" | Some "Old" | Some "Post" ->
      (* the last two labels can only be found in contracts and refer
         to the pre/post state of the contracts: all local variables
         in scope at current point are also in scope in the labels. *)
      find_from_curr_env (fun _ -> true)
    | Some "Pre" ->
      find_from_curr_env (fun vi -> vi.vformal || vi.vglob)
    | Some "Init" -> find_from_curr_env (fun vi -> vi.vglob)
    | Some lab ->
      cvar_to_lvar
        (Datatype.String.Map.find var
           (Datatype.String.Hashtbl.find label_env lab))

  let find_enum_tag x =
    match Datatype.String.Hashtbl.find ghost_env x with
    | EnvEnum item,_ ->
      dummy_exp (Const (CEnum item)), typeOf item.eival
    | _ -> raise Not_found

  let find_comp_field info s = findField s info

  let find_type namespace s =
    match namespace with
    | Logic_typing.Typedef -> let t,_ = lookupTypeNoError true "type" s in t
    | Logic_typing.Union -> findCompType true "union" s []
    | Logic_typing.Struct -> findCompType true "struct" s []
    | Logic_typing.Enum -> findCompType true "enum" s []

  include Logic_labels

  let integral_cast = integral_cast

  (* This function raises a non-recoverable when [-kernel-warn-key annot-error]
     is not set, and [LogicTypeError] otherwise. This exception must *not*
     escape Cabs2cil. Hence, each call to a function of module [Ltyping] below
     must catch it. *)
  let error loc msg =
    Pretty_utils.ksfprintf (fun e -> raise (LogicTypeError (loc,e))) msg

  let on_error f rollback x =
    try f x with
    | LogicTypeError (loc,e) as exn -> rollback (loc,e); raise exn

end

module Ltyping = Logic_typing.Make (C_logic_env)

let startLoop iswhile =
  incr C_logic_env.nb_loop;
  add_label_env "LoopEntry";
  add_label_env "LoopCurrent";
  continues :=
    (if iswhile then While (ref "") else NotWhile (ref "")) :: !continues;
  enter_break_env ()

let exitLoop () =
  decr C_logic_env.nb_loop;
  exit_break_env ();
  remove_label_env "LoopEntry";
  remove_label_env "LoopCurrent";
  match !continues with
  | [] -> Kernel.error ~once:true ~current:true "exit Loop not in a loop"
  | _ :: rest -> continues := rest

let enterScope () =
  scopes := (ref []) :: !scopes;
  C_logic_env.enter_scope ()

(* Exit a scope and clean the environment. We do not yet delete from
 * the name table *)
let exitScope () =
  let this, rest = match !scopes with
    | [] -> Kernel.fatal ~current:true "Not in a scope"
    | car :: cdr -> car, cdr
  in
  scopes := rest;
  let rec loop = function
      [] -> ()
    | UndoRemoveFromEnv (ghost, n) :: t ->
      Datatype.String.Hashtbl.remove ghost_env n;
      if not ghost then Datatype.String.Hashtbl.remove env n;
      loop t
    | UndoAlphaEnv undolist :: t ->
      Alpha.undoAlphaChanges ~alphaTable ~undolist;
      loop t
  in
  loop !this;
  C_logic_env.exit_scope ()

let consLabel ~ghost (l: string) (c: chunk) (loc: location)
    (in_original_program_text : bool) : chunk =
  (* Get the first statement and add the label to it *)
  let labstmt, stmts' = getFirstInChunk ~ghost ~loc c in
  (* Add the label *)
  add_label l labstmt;
  labstmt.labels <- Label (l, loc, in_original_program_text) ::
                    labstmt.labels;
  if c.stmts == stmts' then c else {c with stmts = stmts'}

let consLabContinue ~ghost (c: chunk) =
  match !continues with
  | [] -> Kernel.fatal ~current:true "labContinue not in a loop"
  | While lr :: _ ->
    begin
      assert (!doTransformWhile);
      if !lr = "" then c else consLabel ~ghost !lr c (Current_loc.get ()) false
    end
  | NotWhile lr :: _ ->
    if !lr = "" then c else consLabel ~ghost !lr c (Current_loc.get ()) false

(* Was a continue instruction used inside the current loop *)
let continueUsed () =
  match !continues with
  | [] -> Kernel.fatal ~current:true "not in a loop"
  | (While lr | NotWhile lr) :: _ -> !lr <> ""

(****** TYPE SPECIFIERS *******)


type local_env =
  { authorized_reads: Lval.Set.t;
    known_behaviors: string list;
    is_ghost: bool;
    is_paren: bool; (* true for expressions whose parent is Cabs.PAREN *)
    inner_paren: bool
    (* used during unop/binop traversal to distinguish between
       Cabs.PAREN (Cabs.UNOP(...)) and Cabs.UNOP(Cabs.PAREN(...)) *)
  }

let empty_local_env =
  { authorized_reads = Lval.Set.empty;
    known_behaviors = [];
    is_ghost = false;
    is_paren = false;
    inner_paren = false;
  }

let ghost_local_env ghost = {empty_local_env with is_ghost = ghost }
let add_ghost_to_local_env env ghost =
  { env with is_ghost = env.is_ghost || ghost }

let paren_local_env env = { env with is_paren = true }
let no_paren_local_env env = { env with is_paren = false }
let inner_paren env = { env with inner_paren = true }
let no_inner_paren env = { env with inner_paren = false }

(* weimer: Sat Dec 8 17:30:47 2001 MSVC NT kernel headers include
 * functions like long convert(x) { __asm { mov eax, x \n cdq } }
 * That set a return value via an ASM statement. As a result, I
 * am changing this so a final ASM statement does not count as
 * "fall through" for the purposes of this warning.  *)
(* matth: But it's better to assume assembly will fall through,
 * since  most such blocks do.  It's probably better to print an
 * unnecessary warning than to break CIL's invariant that
 * return statements are inserted properly.  *)
let rec compute_from_root f = function
    [] -> false

  (* We have a label, perhaps we can jump here *)
  | s :: rest when s.labels <> [] ->
    Kernel.debug ~level:4 "computeFromRoot call f from stmt %a"
      Cil_printer.pp_location (Stmt.loc s);
    f (s :: rest)

  | _ :: rest -> compute_from_root f rest

let rec stmtFallsThrough (s: stmt) : bool =
  Kernel.debug ~level:4 "stmtFallsThrough stmt %a"
    Cil_printer.pp_location (Stmt.loc s);
  match s.skind with
  | Instr il -> Cil.instr_falls_through il
  | UnspecifiedSequence seq ->
    blockFallsThrough (block_from_unspecified_sequence seq)
  | Return _ | Break _ | Continue _ | Throw _ -> false
  | Goto _ -> false
  | If (_, b1, b2, _) ->
    blockFallsThrough b1 || blockFallsThrough b2
  | Switch (_e, b, targets, _) ->
    (* See if there is a "default" case *)
    if not
        (List.exists
           (fun s ->
              List.exists (function Default _ -> true | _ -> false)
                s.labels)
           targets)
    then begin
      true (* We fall through because there is no default *)
    end else begin
      (* We must examine all cases. If any falls through,
       * then the switch falls through. *)
      blockFallsThrough b || blockCanBreak b
    end
  | Loop (_,b, _, _, _) ->
    (* A loop falls through if it can break. *)
    blockCanBreak b
  | Block b -> blockFallsThrough b
  | TryCatch (b, l, _) ->
    List.fold_left
      (fun acc (_,b) -> acc || blockFallsThrough b)
      (blockFallsThrough b) l
  | TryFinally (_b, h, _) -> blockFallsThrough h
  | TryExcept (_b, _, _h, _) -> true (* Conservative *)
and stmtListFallsThrough = function
    [] -> true
  | s :: rest ->
    if stmtFallsThrough s then begin
      stmtListFallsThrough rest
    end else begin
      (* If we are not falling through then maybe there
       * are labels who are *)
      compute_from_root stmtListFallsThrough rest
    end
and blockFallsThrough b =
  stmtListFallsThrough b.bstmts

(* will we leave this statement or block with a break command? *)
and stmtCanBreak (s: stmt) : bool =
  Kernel.debug ~level:4 "stmtCanBreak stmt %a"
    Cil_printer.pp_location (Stmt.loc s);
  match s.skind with
  | Instr _ | Return _ | Continue _ | Goto _ | Throw _ -> false
  | Break _ -> true
  | UnspecifiedSequence seq ->
    blockCanBreak (block_from_unspecified_sequence seq)
  | If (_, b1, b2, _) ->
    blockCanBreak b1 || blockCanBreak b2
  | Switch _ | Loop _ ->
    (* switches and loops catch any breaks in their bodies *)
    false
  | Block b -> blockCanBreak b
  | TryCatch (b,l,_) ->
    List.fold_left
      (fun acc (_,b) -> acc || blockCanBreak b)
      (blockCanBreak b)
      l
  | TryFinally (b, h, _) -> blockCanBreak b || blockCanBreak h
  | TryExcept (b, _, h, _) -> blockCanBreak b || blockCanBreak h
and blockCanBreak b =
  let rec aux = function
      [] -> false
    | s::tl ->
      Kernel.debug ~level:4 "blockCanBreak from stmt %a"
        Cil_printer.pp_location (Stmt.loc s);
      stmtCanBreak s ||
      (if stmtFallsThrough s then aux tl
       else compute_from_root aux tl)
  in aux b.bstmts

let chunkFallsThrough c =
  let get_stmt (s,_,_,_,_) = s in
  let stmts = List.rev_map get_stmt c.stmts in
  stmtListFallsThrough stmts

let has_local_init chunk =
  List.exists
    (fun (s,_,_,_,_) ->
       match s.skind with Instr (Local_init _) -> true | _ -> false)
    chunk.stmts

let append_chunk_to_annot ~ghost annot_chunk current_chunk =
  match current_chunk.stmts with
  | [] -> annot_chunk @@@ (current_chunk, ghost)
  (* don't forget locals of current_chunk *)

  (* if we have a single statement,
     we can avoid enclosing it into a block. *)
  | [ (_s,_,_,_,_) ] ->
    (*     Format.eprintf "Statement is: %a@." d_stmt _s;  *)
    annot_chunk @@@ (current_chunk, ghost)
  (* Make a block, and put labels of the first statement
     on the block itself, so as to respect scoping rules
     for \at in further annotations. *)
  | _ ->
    if has_local_init current_chunk then begin
      (* See if we can collapse the statements of the chunk into a single one.
         Otherwise, we can't handle the combination, as putting the Local_init
         into a new block would change the scope of the local variable, at
         least in the pretty-printed code. Furthermore, the usefulness of
         such annotations is dubious at best.
      *)
      let res =
        match current_chunk.stmts with
        | [(s1, m1, w1, r1, c1); (s2, m2, w2, r2, c2)] ->
          Fun.flip
            Option.bind
            (function
              | [ s1' ] -> Some (s1', m1 @ m2, w1 @ w2, r1 @ r2, c1 @ c2)
              | _ -> None (* should not happen. *))
            (collapseCallCast (s2,s1)) (* the chunk list is reversed.*)
        | _ -> None
      in
      match res with
      | Some s -> annot_chunk @@@ ({current_chunk with stmts = [s]}, ghost)
      | None ->
        Kernel.warning ~wkey:Kernel.wkey_annot_error ~current:true
          "Statement contract and ACSL pragmas over a local definition \
           are not implemented. Ignoring annotation";
        current_chunk
    end else begin
      let b = c2block ~ghost current_chunk in
      (* The statement may contain some local variable
         declarations (but no definitions) coming from userland.
         We have to shift them from the inner block, otherwise they will not
         be accessible in the next statements.
      *)
      let locals = b.blocals in
      b.blocals <- [];
      b.battrs <-
        Ast_attributes.add_list [(frama_c_keep_block,[])] b.battrs;
      let block = mkStmt ~ghost ~valid_sid (Block b) in
      let chunk = s2c block in
      let chunk = { chunk with cases = current_chunk.cases } in
      annot_chunk @@@ (List.fold_left
                         local_var_chunk chunk (List.rev locals), ghost)
    end

let default_argument_promotion idx exp =
  let name = "x_" ^ string_of_int idx in
  let arg_type = typeOf exp in
  let typ =
    let t = unrollType arg_type in
    match t.tnode with
    | TVoid -> voidType
    | TInt k when Cil.rank k < Cil.rank IInt ->
      if intTypeIncluded k IInt then intType
      else (* This may happen when char or short have the same size as int *)
        uintType
    | TInt ik -> mk_tint ik
    | TFloat FFloat -> doubleType
    | TFloat fk -> mk_tfloat fk
    | TPtr t | TArray (t, _) -> mk_tptr t
    | TFun _  -> mk_tptr t
    | TComp ci -> mk_tcomp ci
    | TEnum ei -> mk_tenum ei
    | TBuiltin_va_list ->
      abort_context "implicit prototype cannot have variadic arguments"
    | TNamed _ -> assert false (* unrollType *)
  in
  (* if we make a promotion, take it explicitly
     into account in the argument itself *)
  let (_,e) = castTo arg_type typ exp in
  (name,typ,[]), e

(* Promote variadic arguments with standard argument promotions.*)
let promote_variadic_arguments (chunk,args) =
  let args =
    List.mapi
      (fun i arg -> snd (default_argument_promotion i arg))
      args
  in
  (chunk,args)

let rec evaluate_cond_exp = function
  | CEExp (_,e) ->
    (match Cil.constFoldToInt e with
     | None -> `CUnknown
     | Some z when Integer.is_zero z -> `CFalse
     | Some _ -> `CTrue)
  | CEAnd (e1,e2) ->
    let r = evaluate_cond_exp e1 in
    if r = `CTrue then evaluate_cond_exp e2 else r
  | CEOr(e1,e2) ->
    let r = evaluate_cond_exp e1 in
    if r = `CFalse then evaluate_cond_exp e2 else r
  | CENot e ->
    match evaluate_cond_exp e with
    | `CTrue -> `CFalse
    | `CFalse -> `CTrue
    | `CUnknown -> `CUnknown

let get_lval_compound_assigned op expr =
  match expr.enode with
  | Lval x
  (* A GCC extension. The operation is done at the cast type.
     The result is also of the cast type *)
  | CastE (_, {enode = Lval x}) ->
    if Cil.is_modifiable_lval x then x else
      abort_context
        "Cannot assign to non-modifiable lval %a"
        Cil_printer.pp_lval x
  | _ -> abort_context "Expected lval for %s" op

type var_decl_kind =
  [ `FormalDecl | `GlobalDecl | `LocalDecl | `LocalStaticDecl ]
type type_context =
  [ var_decl_kind | `FieldDecl | `Typedef | `OnlyType ]

(* The way formals are handled now might generate incorrect types, in the
   sense that they refer to a varinfo (in the case of VLA depending on a
   previously declared formal) that exists only during the call to doType.
   We replace them here with the definitive version of the formals' varinfos.
   A global refactoring of cabs2cil would be welcome, though.
*)
let fixFormalsType formals =
  let table = Hashtbl.create 5 in
  let vis =
    object
      inherit Cil.nopCilVisitor
      method! vvrbl v =
        if v.vformal then begin
          try
            ChangeTo (Hashtbl.find table v.vname)
          with Not_found ->
            Kernel.fatal ~current:true "Formal %a not tied to a varinfo"
              Cil_printer.pp_varinfo v;
        end else SkipChildren
    end
  in
  let treat_one_formal v =
    Cil.update_var_type v (Cil.visitCilType vis v.vtype);
    Hashtbl.add table v.vname v;
  in
  List.iter treat_one_formal formals

(* Map from standard int type names like [uint16_t] to their expected sizes,
   and a flag whether the given size is exact (or a lower bound). That is,
   [uint16_t] maps to [(16, true)], and [uint_least16_t] to [(16, false)].
   Used by [checkTypedefSize] below. *)
let stdIntegerSizes = Hashtbl.create 5

(* Initialize the stdIntegerSizes table. *)
let initStdIntegerSizes () =
  let bases = ["int"; "uint"] in
  let sizes = [8; 16; 32; 64] in
  let add_std_type base size =
    let add_variant (variant, exact) =
      let key = base ^ variant ^ (string_of_int size) ^ "_t" in
      Hashtbl.add stdIntegerSizes key (size, exact)
    in
    (* Store exact "normal" variant, inexact "fast" and "least" variants. *)
    List.iter add_variant [("", true); ("_fast", false); ("_least", false)]
  in
  List.iter (fun b -> List.iter (add_std_type b) sizes) bases;
  (* Also store variants of [intptr_t] using the size of [void *], and
     [intmax_t] variants using the size of [long long]. *)
  let add_special_types name size =
    let add base =
      Hashtbl.add stdIntegerSizes (base ^ name ^ "_t") (size, true)
    in
    List.iter add bases
  in
  add_special_types "ptr" (Cil.bitsSizeOf voidPtrType);
  add_special_types "max" (Cil.bitsSizeOf longLongType)

(* [checkTypedefSize name typ] checks if [name] is acceptable as a typedef
   name for type [typ]. If [name] is one of the standard integer type names
   like [uint16_t] but [typ] has the wrong bit size, emits a warning. *)
let checkTypedefSize name typ =
  if Hashtbl.length stdIntegerSizes = 0 then
    initStdIntegerSizes ();
  if Cil.isIntegralType typ then begin
    let size = Cil.bitsSizeOf typ in
    try
      let intended_size, exact = Hashtbl.find stdIntegerSizes name in
      if (exact && size <> intended_size) ||
         (not exact && size < intended_size)
      then
        Kernel.warning ~current:true
          "bad type '%a' (%d bits) for typedef '%s' using machdep %s;@ \
           check for mismatch between -machdep flag and headers used"
          Typ.pretty typ size name (Machine.machdep_name ())
    with
    (* Not a standard integer type, ignore it. *)
      Not_found -> ()
  end

(* Checks for invalid 'restrict' qualifiers,
   and reports [Kernel.error] if they are found. *)
let rec checkRestrictQualifierDeep t =
  if typeHasQualifier "restrict" t then
    match unrollTypeNode t with
    | TArray (bt, _) | TPtr bt ->
      if isFunctionType bt then
        Kernel.error ~once:true ~current:true
          "function pointer type does not allow 'restrict' qualifier"
      else
        checkRestrictQualifierDeep bt
    | _ -> Kernel.error ~once:true ~current:true
             "invalid usage of 'restrict' qualifier"
  else
    match unrollTypeNode t with
    | TArray (bt, _) | TPtr bt ->
      checkRestrictQualifierDeep bt
    | TFun (rt, args, _) ->
      checkRestrictQualifierDeep rt;
      begin
        match args with
        | None -> ()
        | Some args ->
          List.iter (fun (_, t, _) -> checkRestrictQualifierDeep t) args
      end
    | _ -> ()

(* Return true if the given expression is a call or a struct-returning call. *)
let rec nested_call e =
  match e.expr_node with
  | CALL _ -> true
  | MEMBEROF (e, _ ) -> nested_call e
  | _ -> false

(* Used to remember if we encountered a access to a field of type array from
   a struct-returning call. *)
let contains_temp_subarray = ref false

let rec doSpecList loc ghost (suggestedAnonName: string)
    (* This string will be part of
     * the names for anonymous
     * structures and enums  *)
    (specs: Cabs.spec_elem list)
  (* Returns the base type, the storage, whether it is inline and the
   * (unprocessed) attributes *)
  : typ * storage * bool * Cabs.attribute list =
  (* Do one element and collect the type specifiers *)
  let isinline = ref false in (* If inline appears *)
  (* The storage is placed here *)
  let storage : storage ref = ref NoStorage in

  (* Collect the attributes.  Unfortunately, we cannot treat GCC
   * __attributes__ and ANSI C const/volatile the same way, since they
   * associate with structures differently.  Specifically, ANSI
   * qualifiers never apply to structures (ISO 6.7.3), whereas GCC
   * attributes always do (GCC manual 4.30).  Therefore, they are
   * collected and processed separately. *)
  let attrs : Cabs.attribute list ref = ref [] in      (* __attribute__, etc. *)
  let cvattrs : Cabs.cvspec list ref = ref [] in       (* const/volatile *)
  let suggestedAnonName =
    if suggestedAnonName <> "" then suggestedAnonName
    else if get_current_stdheader () = "" then ""
    else "fc_stdlib"
  in
  let doSpecElem (se: Cabs.spec_elem)
      (acc: Cabs.typeSpecifier list)
    : Cabs.typeSpecifier list =
    match se with
    | Cabs.SpecTypedef -> acc
    | Cabs.SpecInline -> isinline := true; acc
    | Cabs.SpecStorage st ->
      if !storage <> NoStorage then
        Kernel.error ~once:true ~current:true "Multiple storage specifiers";
      let sto' =
        match st with
        | Cabs.NO_STORAGE -> NoStorage
        | Cabs.AUTO -> NoStorage
        | Cabs.REGISTER -> Register
        | Cabs.STATIC -> Static
        | Cabs.EXTERN -> Extern
      in
      storage := sto';
      acc

    | Cabs.SpecCV cv -> cvattrs := cv :: !cvattrs; acc
    | Cabs.SpecAttr a -> attrs := a :: !attrs; acc
    | Cabs.SpecType ts -> ts :: acc
  in
  (* Now scan the list and collect the type specifiers. Preserve the order *)
  let tspecs = List.fold_right doSpecElem specs [] in

  let tspecs' =
    (* GCC allows a named type that appears first to be followed by things
     * like "short", "signed", "unsigned" or "long". *)
    match tspecs with
    | Cabs.Tnamed _ :: (_ :: _ as rest) when Machine.gccMode () ->
      (* If rest contains "short" or "long" then drop the Tnamed *)
      if List.exists (function Cabs.Tshort -> true
                             | Cabs.Tlong -> true | _ -> false) rest then
        rest
      else
        tspecs

    | _ -> tspecs
  in
  let tspecs'' =
    match specs, List.rev tspecs' with
    | Cabs.SpecTypedef :: _, Cabs.Tnamed _ :: [] ->
      tspecs'
    | Cabs.SpecTypedef :: _, Cabs.Tnamed _ :: rest ->
      List.rev rest
    | _ -> tspecs'
  in
  (* Sort the type specifiers *)
  let sortedspecs =
    let order = function (* Don't change this *)
      | Cabs.Tvoid -> 0
      | Cabs.Tsigned -> 1
      | Cabs.Tunsigned -> 2
      | Cabs.Tchar -> 3
      | Cabs.Tshort -> 4
      | Cabs.Tlong -> 5
      | Cabs.Tint -> 6
      | Cabs.Tint64 -> 7
      | Cabs.Tfloat -> 8
      | Cabs.Tdouble -> 9
      | _ -> 10 (* There should be at most one of the others *)
    in
    List.stable_sort (fun ts1 ts2 ->
        Datatype.Int.compare (order ts1) (order ts2)) tspecs''
  in
  let getTypeAttrs () : Cabs.attribute list =
    (* Partitions the attributes in !attrs.
       Type attributes are removed from attrs and returned, so that they
       can go into the type definition.  Name attributes are left in attrs,
       so they will be returned by doSpecAttr and used in the variable
       declaration.
       Testcase: small1/attr9.c *)
    let an, af, at =
      cabsPartitionAttributes ghost ~default:Ast_attributes.AttrType !attrs
    in
    attrs := an;      (* Save the name attributes for later *)
    if af <> [] then
      Kernel.error ~once:true ~current:true
        "Invalid position for function type attributes.";
    at
  in

  (* And now try to make sense of it. See ISO 6.7.2 *)
  let bt =
    match sortedspecs with
    | [Cabs.Tvoid] -> voidType
    | [Cabs.Tchar] -> charType
    | [Cabs.Tbool] -> boolType
    | [Cabs.Tsigned; Cabs.Tchar] -> scharType
    | [Cabs.Tunsigned; Cabs.Tchar] -> ucharType

    | [Cabs.Tshort]
    | [Cabs.Tsigned; Cabs.Tshort]
    | [Cabs.Tshort; Cabs.Tint]
    | [Cabs.Tsigned; Cabs.Tshort; Cabs.Tint] -> shortType

    | [Cabs.Tunsigned; Cabs.Tshort]
    | [Cabs.Tunsigned; Cabs.Tshort; Cabs.Tint] -> ushortType

    | [] ->
      Kernel.warning ~current:true ~wkey:Kernel.wkey_implicit_int
        "type specifier missing, defaults to 'int'; ISO C99 and later do not \
         support implicit int";
      intType
    | [Cabs.Tint]
    | [Cabs.Tsigned]
    | [Cabs.Tsigned; Cabs.Tint] -> intType

    | [Cabs.Tunsigned]
    | [Cabs.Tunsigned; Cabs.Tint] -> uintType

    | [Cabs.Tlong]
    | [Cabs.Tsigned; Cabs.Tlong]
    | [Cabs.Tlong; Cabs.Tint]
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tint] -> longType

    | [Cabs.Tunsigned; Cabs.Tlong]
    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tint] -> ulongType

    | [Cabs.Tlong; Cabs.Tlong]
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tlong]
    | [Cabs.Tlong; Cabs.Tlong; Cabs.Tint]
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tlong; Cabs.Tint] -> longLongType

    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tlong]
    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tlong; Cabs.Tint] -> ulongLongType

    (* int64 is to support MSVC *)
    | [Cabs.Tint64]
    | [Cabs.Tsigned; Cabs.Tint64] -> longLongType

    | [Cabs.Tunsigned; Cabs.Tint64] -> ulongLongType

    | [Cabs.Tfloat]  -> floatType
    | [Cabs.Tdouble] -> doubleType

    | [Cabs.Tlong; Cabs.Tdouble] -> longDoubleType

    (* Now the other type specifiers *)
    | [Cabs.Tnamed "__builtin_va_list"]
      when Machine.has_builtin_va_list () -> mk_tbuiltin ()
    | [Cabs.Tnamed "__fc_builtin_size_t"] -> Machine.sizeof_type ()
    | [Cabs.Tnamed n] ->
      (match lookupType ghost "type" n with
       | { tnode = TNamed _ } as t, _ -> t
       | _ ->
         Kernel.fatal ~current:true "Named type %s is not mapped correctly" n)

    | [Cabs.Tstruct (n, None, _)] -> (* A reference to a struct *)
      if n = "" then
        Kernel.error ~once:true ~current:true
          "Missing struct tag on incomplete struct";
      findCompType ghost "struct" n []
    | [Cabs.Tstruct (n, Some nglist, extraAttrs)] -> (* A definition of a struct *)
      let n' =
        if n <> "" then n else anonStructName "struct" suggestedAnonName in
      (* Use the (non-cv, non-name) attributes in !attrs now *)
      let a = extraAttrs @ (getTypeAttrs ()) in
      makeCompType loc ghost true n' ~norig:n nglist (doAttributes ghost a)

    | [Cabs.Tunion (n, None, _)] -> (* A reference to a union *)
      if n = "" then
        Kernel.error ~once:true ~current:true
          "Missing union tag on incomplete union";
      findCompType ghost "union" n []
    | [Cabs.Tunion (n, Some nglist, extraAttrs)] -> (* A definition of a union *)
      let n' =
        if n <> "" then n else anonStructName "union" suggestedAnonName in
      (* Use the attributes now *)
      let a = extraAttrs @ (getTypeAttrs ()) in
      makeCompType loc ghost false n' ~norig:n nglist (doAttributes ghost a)

    | [Cabs.Tenum (n, None, _)] -> (* Just a reference to an enum *)
      if n = "" then
        Kernel.error ~once:true ~current:true
          "Missing enum tag on incomplete enum";
      findCompType ghost "enum" n []

    | [Cabs.Tenum (n, Some eil, extraAttrs)] -> (* A definition of an enum *)
      let n' =
        if n <> "" then n else anonStructName "enum" suggestedAnonName in
      (* make a new name for this enumeration *)
      let n'', _  = newAlphaName ghost true "enum" n' in

      (* Create the enuminfo, or use one that was created already for a
       * forward reference *)
      let enum, _ = createEnumInfo n'' ~norig:n in
      let a = extraAttrs @ (getTypeAttrs ()) in
      enum.eattr <- enum.eattr @ (doAttributes ghost a);
      let res = mk_tenum enum in
      let smallest = ref Integer.zero in
      let largest = ref Integer.zero in
      (* Life is fun here. ANSI says: enum constants are ints,
         and there's an implementation-dependent underlying integer
         type for the enum, which must be capable of holding all the
         enum's values.
         For MSVC, we follow these rules and assume the enum's
         underlying type is int.
         GCC allows enum constants that don't fit in int: the enum
         constant's type is the smallest type (but at least int) that
         will hold the value, with a preference for unsigned types.
         The underlying type EI of the enum is picked as follows:
         – let T be the smallest integer type that holds all the enum's
           values; T is signed if any enum value is negative, unsigned otherwise
         – if the enum is packed or sizeof(T) >= sizeof(int), then EI = T
         – otherwise EI = int if T is signed and unsigned int otherwise
         Note that these rules make the enum unsigned if possible *)
      let updateEnum i : ikind =
        if Integer.lt i !smallest then
          smallest := i;
        if Integer.gt i !largest then
          largest := i;
        if Machine.msvcMode () then
          IInt
        else begin
          match Kernel.Enums.get () with
          (* gcc-short-enum will try to pack the enum _type_, not the enum
             constant... *)
          | "" | "help" | "gcc-enums" | "gcc-short-enums" ->
            if fitsInInt IInt i then IInt
            else if fitsInInt IUInt i then IUInt
            else if fitsInInt ILongLong i then ILongLong
            else IULongLong
          | "int" -> IInt
          | s ->
            Kernel.fatal ~current:true "Unknown enums representations '%s'" s
        end
      in
      (* as each name,value pair is determined, this is called *)
      let rec processName kname (i: exp) loc rest = begin
        (* add the name to the environment, but with a faked 'typ' field;
         * we don't know the full type yet (since that includes all of the
         * tag values), but we won't need them in here  *)

        (* add this tag to the list so that it ends up in the real
         * environment when we're finished  *)
        let newname, _  = newAlphaName ghost true "" kname in
        let item = { eiorig_name = kname;
                     einame = newname;
                     eival = i;
                     eiloc = loc;
                     eihost = enum }
        in
        addLocalToEnv ghost kname (EnvEnum item);
        (kname, item) :: loop (increm i 1) rest
      end

      and loop i = function
          [] -> []
        | (kname, { expr_node = Cabs.NOTHING}, cloc) :: rest ->
          (* use the passed-in 'i' as the value, since none specified *)
          processName kname i (convLoc cloc) rest

        | (kname, e, cloc) :: rest ->
          (* constant-eval 'e' to determine tag value *)
          let e' = getIntConstExp ghost e in
          let e' = match constFoldToInt e' with
            | None ->
              abort_context
                "Constant initializer %a not an integer"
                Cil_printer.pp_exp e'
            | Some i ->
              let ik = updateEnum i in
              if Machine.lower_constants () then
                kinteger64 ~loc:e.expr_loc ~kind:ik i
              else
                e'
          in
          processName kname e' (convLoc cloc) rest
      in

      (*TODO: find a better loc*)
      let fields = loop (zero ~loc:(Current_loc.get())) eil in
      (* Now set the right set of items *)
      enum.eitems <- List.map (fun (_, x) -> x) fields;
      (* Pick the enum's kind - see discussion above *)
      begin
        let unsigned = Integer.ge !smallest Integer.zero in
        let smallKind = intKindForValue !smallest unsigned in
        let largeKind = intKindForValue !largest unsigned in
        let real_kind =
          if (bytesSizeOfInt smallKind) > (bytesSizeOfInt largeKind) then
            smallKind
          else
            largeKind
        in
        let ekind =
          match Kernel.Enums.get () with
          | "" | "help" | "gcc-enums" ->
            if Ast_attributes.contains "packed" enum.eattr ||
               bytesSizeOfInt real_kind >= bytesSizeOfInt IInt
            then real_kind
            else if unsigned then IUInt else IInt
          | "int" -> IInt
          | "gcc-short-enums" -> real_kind
          | s -> Kernel.fatal ~current:true "Unknown enum representation '%s'" s
        in
        enum.ekind <- ekind;
      end;
      (* Record the enum name in the environment *)
      addLocalToEnv ghost (kindPlusName "enum" n') (EnvTyp res);
      (* And define the tag *)
      cabsPushGlobal (GEnumTag (enum, Current_loc.get ()));
      res

    | [Cabs.TtypeofE e] ->
      let (_, s, e', t) =
        doExp (ghost_local_env ghost) CNoConst e AExpLeaveArrayFun
      in
      clean_up_chunk_locals s;
      let t' =
        match e'.enode with
        (* If this is a string literal, then we treat it as in sizeof*)
        | Const (CStr s) -> begin
            match (typeOf e').tnode with
            | TPtr bt-> (* This is the type of array elements *)
              mk_tarray bt (Some (new_exp ~loc:e'.eloc (SizeOfStr s)))
            | _ -> abort_context "The typeOf a string is not a pointer type"
          end
        | _ -> t
      in
      (*
        ignore (E.log "typeof(%a) = %a\n" d_exp e' d_type t');
       *)
      t'

    | [Cabs.TtypeofT (specs, dt)] -> doOnlyType loc ghost specs dt

    | l ->
      abort_context
        "Invalid combination of type specifiers:@ %a"
        (pp_list ~sep:"@ " Cprint.print_type_spec) l;
  in
  bt,!storage,!isinline,List.rev (!attrs @ (convertCVtoAttr !cvattrs))

(* given some cv attributes, convert them into named attributes for
 * uniform processing *)
and convertCVtoAttr (src: Cabs.cvspec list) : Cabs.attribute list =
  match src with
  | [] -> []
  | CV_CONST    :: tl -> ("const",[])    :: (convertCVtoAttr tl)
  | CV_VOLATILE :: tl -> ("volatile",[]) :: (convertCVtoAttr tl)
  | CV_RESTRICT :: tl -> ("restrict",[]) :: (convertCVtoAttr tl)
  | CV_GHOST    :: tl -> ("ghost",[]) :: (convertCVtoAttr tl)

and makeVarInfoCabs
    ~(ghost:bool)
    ~(kind:var_decl_kind)
    ?(isgenerated=false)
    ?(referenced=false)
    (ldecl : location)
    (bt, sto, inline, attrs)
    (n,ndt,a)
  : varinfo =
  let isglobal = kind = `GlobalDecl || kind = `LocalStaticDecl in
  let isformal = kind = `FormalDecl in
  let vtype, nattr =
    doType ghost (kind:>type_context) (Ast_attributes.AttrName false)
      ~allowVarSizeArrays:isformal
      (* For locals we handle var-sized arrays before makeVarInfoCabs;
         Hence, at this point only formals can have a VLA type *)
      bt (Cabs.PARENTYPE(attrs, ndt, a))
  in
  if Ast_attributes.contains "thread" nattr then begin
    let wkey = Kernel.wkey_inconsistent_specifier in
    let source = fst ldecl in
    if isFunctionType vtype then
      Kernel.warning ~wkey ~source "only objects can be thread-local"
    else if not isglobal && (sto = NoStorage || sto = Register) then
      Kernel.warning ~wkey ~source "a local object cannot be thread-local";
  end;
  if not isgenerated && ghost then begin
    if Ast_attributes.contains "ghost" (Cil.typeAttrs vtype) then
      Kernel.warning
        ~wkey:Kernel.wkey_ghost_already_ghost ~once:true ~current:true
        "'%s' is already ghost" n;
    if isArrayType vtype then
      if Ast_attributes.contains "ghost" (Cil.typeAttrs (typeOf_array_elem vtype)) then
        Kernel.warning
          ~wkey:Kernel.wkey_ghost_already_ghost ~once:true ~current:true
          "'%s' elements are already ghost" n;
  end ;

  if inline && not (isFunctionType vtype) then
    Kernel.error ~once:true ~current:true "inline for a non-function: %s" n;
  checkRestrictQualifierDeep vtype;
  let vi =
    makeVarinfo ~ghost ~referenced ~temp:isgenerated ~loc:ldecl isglobal isformal n vtype
  in
  vi.vstorage <- sto;
  vi.vattr <- nattr;
  vi.vdefined <-
    not (isFunctionType vtype) && isglobal && (sto = NoStorage || sto = Static);
  vi

(* Process a local variable declaration and allow variable-sized arrays *)
and makeVarSizeVarInfo ghost (ldecl : location)
    spec_res
    (n,ndt,a)
  : varinfo * chunk * exp * bool =
  let kind = `LocalDecl in
  if not (Machine.msvcMode ()) then
    match isVariableSizedArray ghost ndt with
    | None ->
      makeVarInfoCabs ~ghost ~kind ldecl spec_res (n,ndt,a),
      empty, zero ~loc:ldecl, false
    | Some (ndt', se, len) ->
      (* In this case, we have changed the type from VLA to pointer: add the
         qualifier to the elements. *)
      let spec_res = match spec_res with
        | (t, sto , inline , attrs) when ghost ->
          (t, sto , inline , ("ghost", []) :: attrs)
        | normal -> normal
      in
      makeVarInfoCabs ~ghost ~kind ldecl spec_res (n,ndt',a), se, len, true
  else
    makeVarInfoCabs ~ghost ~kind ldecl spec_res (n,ndt,a),
    empty, zero ~loc:ldecl, false

and doAttr ghost (a: Cabs.attribute) : attribute list =
  (* Strip the leading and trailing underscore *)
  match a with
  | ("__attribute__", []) -> []  (* An empty list of gcc attributes *)
  | (s, el) ->

    let rec attrOfExp (strip: bool)
        ?(foldenum=true)
        (a: Cabs.expression) : attrparam =
      let loc = a.expr_loc in
      match a.expr_node with
      | Cabs.VARIABLE n -> begin
          let n' = if strip then stripUnderscore n else n in
          (* See if this is an enumeration *)
          try
            if not foldenum then raise Not_found;
            let env = if ghost then ghost_env else env in
            match Datatype.String.Hashtbl.find env n' with
            | EnvEnum item, _ -> begin
                match constFoldToInt item.eival with
                | Some i64 when Machine.lower_constants () ->
                  AInt i64
                |  _ -> ACons(n', [])
              end
            | _ -> ACons (n', [])
          with Not_found -> ACons(n', [])
        end
      | Cabs.CONSTANT (Cabs.CONST_STRING s) -> AStr s
      | Cabs.CONSTANT (Cabs.CONST_INT str) -> begin
          match parseIntExpRes ~loc str with
          | Ok {enode = Const (CInt64 (v64,_,_)) } ->
            AInt v64
          | _ ->
            Kernel.error ~current:true "Invalid attribute constant: %s" str;
            AInt Integer.one
        end
      | Cabs.CONSTANT (Cabs.CONST_FLOAT str) ->
        ACons ("__fc_float", [AStr str])
      | Cabs.CALL({expr_node = Cabs.VARIABLE n}, args, []) -> begin
          let n' = if strip then stripUnderscore n else n in
          let ae' = List.map ae args in
          ACons(n', ae')
        end
      | Cabs.EXPR_SIZEOF e -> ASizeOfE (ae e)
      | Cabs.TYPE_SIZEOF (bt, dt) -> ASizeOf (doOnlyType loc ghost bt dt)
      | Cabs.EXPR_ALIGNOF e -> AAlignOfE (ae e)
      | Cabs.TYPE_ALIGNOF (bt, dt) -> AAlignOf (doOnlyType loc ghost bt dt)
      | Cabs.BINARY(Cabs.AND, aa1, aa2) ->
        ABinOp(LAnd, ae aa1, ae aa2)
      | Cabs.BINARY(Cabs.OR, aa1, aa2) ->
        ABinOp(LOr, ae aa1, ae aa2)
      | Cabs.BINARY(Cabs.ASSIGN,aa1,aa2) ->
        (* Bit of a hack to account for OSX specific syntax. *)
        ACons ("__fc_assign", [ae aa1; ae aa2])
      | Cabs.BINARY(abop, aa1, aa2) ->
        ABinOp (convBinOp abop, ae aa1, ae aa2)
      | Cabs.UNARY(Cabs.PLUS, aa) -> ae aa
      | Cabs.UNARY(Cabs.MINUS, aa) -> AUnOp (Neg, ae aa)
      | Cabs.UNARY(Cabs.BNOT, aa) -> AUnOp(BNot, ae aa)
      | Cabs.UNARY(Cabs.NOT, aa) -> AUnOp(LNot, ae aa)
      | Cabs.MEMBEROF (e, s) -> ADot (ae e, s)
      | Cabs.PAREN(e) -> attrOfExp strip ~foldenum:foldenum e
      | Cabs.UNARY(Cabs.MEMOF, aa) -> AStar (ae aa)
      | Cabs.UNARY(Cabs.ADDROF, aa) -> AAddrOf (ae aa)
      | Cabs.MEMBEROFPTR (aa1, s) -> ADot(AStar(ae aa1), s)
      | Cabs.INDEX(aa1, aa2) -> AIndex(ae aa1, ae aa2)
      | Cabs.QUESTION(aa1, aa2, aa3) -> AQuestion(ae aa1, ae aa2, ae aa3)
      | _ ->
        Kernel.fatal ~current:true
          "cabs2cil: invalid expression in attribute: %a"
          Cprint.print_expression a

    and ae (e: Cabs.expression) = attrOfExp false e in

    (* Sometimes we need to convert attrarg into attr *)
    let arg2attrs = function
      | ACons (s, _) when List.mem s erased_attributes -> []
      | ACons (s, args) -> [(s, args)]
      | a ->
        Kernel.fatal ~current:true
          "Invalid form of attribute: %a"
          Cil_printer.pp_attrparam a;
    in
    let fold_attrs f el = List.fold_left (fun acc e -> acc @ arg2attrs (f e)) [] el in
    if s = "__attribute__" then (* Just a wrapper for many attributes*)
      fold_attrs (attrOfExp true ~foldenum:false) el
    else if s = "__blockattribute__" then (* Another wrapper *)
      fold_attrs (attrOfExp true ~foldenum:false) el
    else if s = "__declspec" then
      fold_attrs (attrOfExp false ~foldenum:false) el
    else
      [(stripUnderscore s, List.map (attrOfExp ~foldenum:false false) el)]

and doAttributes (ghost:bool) (al: Cabs.attribute list) : attribute list =
  List.fold_left (fun acc a -> cabsAddAttributes (doAttr ghost a) acc) [] al

(* A version of Ast_attributes.partition_attributes that works on CABS attributes.
   It would be better to use Ast_attributes.partition_attributes instead to avoid
   the extra doAttr conversions here, but that's hard to do in doSpecList.*)
and cabsPartitionAttributes
    ghost
    ~(default:Ast_attributes.attribute_class)
    (attrs:  Cabs.attribute list) :
  Cabs.attribute list * Cabs.attribute list * Cabs.attribute list =
  let rec loop (n,f,t) = function
      [] -> n, f, t
    | a :: rest ->
      let an, kind = match doAttr ghost a with
        | [] -> "", default
        | (an, _)::_ ->
          (* doAttr already strip underscores of the attribute if necessary so
             we do not need to strip then before calling get_attribute_class
             here. *)
          an, Ast_attributes.get_class ~default an
      in
      match kind with
      | AttrName _ -> loop (a::n, f, t) rest
      | AttrFunType _ -> loop (n, a::f, t) rest
      | AttrType -> loop (n, f, a::t) rest
      | AttrStmt ->
        Kernel.warning
          ~current:true "Ignoring statement attribute %s found in declaration"
          an;
        loop (n,f,t) rest
      | AttrIgnored -> loop (n, f, t) rest
  in
  loop ([], [], []) attrs

and doType (ghost:bool) (context: type_context)
    (nameortype: Ast_attributes.attribute_class) (* This is AttrName if we are doing
                                                  * the type for a name, or AttrType
                                                  * if we are doing this type in a
                                                  * typedef *)
    ?(allowZeroSizeArrays=false)
    ?(allowVarSizeArrays=false)
    (bt: typ)                    (* The base type *)
    (dt: Cabs.decl_type)
  (* Returns the new type and the accumulated name (or type attribute
     if nameoftype =  AttrType) attributes *)
  : typ * attribute list =

  (* Now do the declarator type. But remember that the structure of the
   * declarator type is as printed, meaning that it is the reverse of the
   * right one *)
  let rec doDeclType (bt: typ) (acc: attribute list) decl_type =
    checkRestrictQualifierDeep bt;
    match decl_type with
    | Cabs.JUSTBASE -> bt, acc
    | Cabs.PARENTYPE (a1, d, a2) ->
      let a1' = doAttributes ghost a1 in
      let a1n, a1f, a1t = Ast_attributes.partition ~default:AttrType a1' in
      let a2' = doAttributes ghost a2 in
      let a2n, a2f, a2t = Ast_attributes.partition ~default:nameortype a2' in
      let bt' = cabsTypeAddAttributes a1t bt in
      let bt'', a1fadded =
        match unrollTypeNode bt with
        | TFun _ -> cabsTypeAddAttributes a1f bt', true
        | _ -> bt', false
      in
      (* Now recurse *)
      let restyp, nattr = doDeclType bt'' acc d in
      (* Add some more type attributes *)
      let restyp = cabsTypeAddAttributes a2t restyp in
      (* See if we can add some more type attributes *)
      let restyp' =
        let t = unrollType restyp in
        match t.tnode with
        | TFun _ ->
          if a1fadded then
            cabsTypeAddAttributes a2f restyp
          else
            cabsTypeAddAttributes a2f
              (cabsTypeAddAttributes a1f restyp)
        | TPtr ({ tnode = TFun _ } as tf)
          when not (Machine.msvcMode ()) ->
          if a1fadded then
            mk_tptr ~tattr:t.tattr (cabsTypeAddAttributes a2f tf)
          else
            let t' = cabsTypeAddAttributes a2f (cabsTypeAddAttributes a1f tf) in
            mk_tptr ~tattr:t.tattr t'
        | _ ->
          if a1f <> [] && not a1fadded then
            Kernel.error ~once:true ~current:true
              "Invalid position for (prefix) function type attributes:%a"
              Cil_printer.pp_attributes a1f;
          if a2f <> [] then
            Kernel.error ~once:true ~current:true
              "Invalid position for (post) function type attributes:%a"
              Cil_printer.pp_attributes a2f;
          restyp
      in

      (* Now add the name attributes and return *)
      restyp', cabsAddAttributes a1n (cabsAddAttributes a2n nattr)

    | Cabs.PTR (al, d) ->
      let al' = doAttributes ghost al in
      let an, af, at = Ast_attributes.partition ~default:AttrType al' in
      (* Now recurse *)
      let t = mk_tptr ~tattr:at bt in
      let restyp, nattr = doDeclType t acc d in
      (* See if we can do anything with function type attributes *)
      let restyp' =
        let t = unrollType restyp in
        match t.tnode with
        | TFun _ -> cabsTypeAddAttributes af restyp
        | TPtr ({ tnode = TFun _ } as tf) ->
          mk_tptr ~tattr:t.tattr (cabsTypeAddAttributes af tf)
        | _ ->
          if af <> [] then
            Kernel.error ~once:true ~current:true
              "Invalid position for function type attributes:%a"
              Cil_printer.pp_attributes af;
          restyp
      in
      (* Now add the name attributes and return *)
      restyp', cabsAddAttributes an nattr

    | Cabs.ARRAY (d, al, len) ->
      if Cil.isFunctionType bt then
        Kernel.error ~once:true ~current:true
          "declaration of array of function type '%a`"
          Cil_datatype.Typ.pretty bt
      else if not (Cil.isCompleteType ~allowZeroSizeArrays:true bt) then
        Kernel.error ~once:true ~current:true
          "declaration of array of incomplete type '%a`"
          Cil_datatype.Typ.pretty bt
      else if not allowZeroSizeArrays &&
              not (Cil.isCompleteType ~allowZeroSizeArrays:false bt)
      then
        (* because we tested previously for incomplete types and now tested again
           forbidding zero-length arrays, bt is necessarily a zero-length array *)
        if Machine.(gccMode () || msvcMode ()) then
          Kernel.warning ~once:true ~current:true
            "declaration of array of 'zero-length arrays' ('%a`);@ \
             zero-length arrays are a compiler extension"
            Cil_datatype.Typ.pretty bt
        else
          Kernel.error ~once:true ~current:true
            "declaration of array of 'zero-length arrays' ('%a`);@ \
             zero-length arrays are not allowed in C99"
            Cil_datatype.Typ.pretty bt;
      let lo =
        match len.expr_node with
        | Cabs.NOTHING -> None
        | _ ->
          (* Check that len is a constant expression.
             We used to also cast the length to int here, but that's
             theoretically too restrictive on 64-bit machines. *)
          let len' = doPureExp (ghost_local_env ghost) len in
          if not (isIntegralType (typeOf len')) then
            Kernel.error ~once:true ~current:true
              "Array length %a does not have an integral type."
              Cil_printer.pp_exp len';
          (* Check that len' is admissible *)
          let cst = constFold true len' in
          (match cst.enode with
           | Const(CInt64(i, _, _)) ->
             begin
               if Integer.lt i Integer.zero then
                 Kernel.error ~once:true ~current:true
                   "Array length is negative."
               else
                 (* Check if array size (nb elem * size elem) is smaller than
                    max size. *)
                 try
                   let elem_size =
                     if Cil.isCompleteType bt &&
                        not (Cil.is_variably_modified_type bt)
                     then
                       Integer.of_int @@ bytesSizeOf bt
                     else
                       (* Incomplete types can't be array elements,
                          and multi-dimensional VLAs are currently unsupported.
                          In both cases an error has already been raised,
                          we just check here that the size is not widely off.*)
                       Integer.one
                   in
                   let size_t = bitsSizeOfInt (Machine.sizeof_kind ()) in
                   let size_max = Cil.max_unsigned_number size_t in
                   let array_size = Integer.mul i elem_size in
                   if Integer.gt array_size size_max then
                     Kernel.warning ~wkey:Kernel.wkey_large_array
                       ~once:true ~current:true
                       "Array length is too large.";
                 with
                 | SizeOfError (msg,_) ->
                   Kernel.error ~once:true ~current:true
                     "Unable to compute the size of array element '%a': %s"
                     Cil_printer.pp_typ bt
                     msg
                 | Invalid_argument msg ->
                   Kernel.fatal ~current:true "%s" msg
             end
           | _  when not allowVarSizeArrays ->
             if isConstant cst then
               (* e.g., there may be a float constant involved.
                * We'll leave it to the user to ensure the length is
                * non-negative, etc.*)
               Kernel.warning ~once:true ~current:true
                 "Unable to do constant-folding on array length %a. \
                  Some CIL operations on this array may fail."
                 Cil_printer.pp_exp cst
             else begin
               match context with
               | `FieldDecl ->
                 Kernel.error ~once:true ~current:true
                   "\"Variable length array in structure\" extension \
                    is not supported"
               | `GlobalDecl ->
                 Kernel.error ~once:true ~current:true
                   "Global arrays cannot have variable size"
               | `LocalStaticDecl ->
                 Kernel.error ~once:true ~current:true
                   "Static arrays cannot have variable size"
               | `Typedef ->
                 Kernel.error ~once:true ~current:true
                   "A type definition cannot be a variable-length array"
               | `LocalDecl ->
                 Kernel.not_yet_implemented
                   "For multi-dimensional arrays, variable length is only \
                    supported on the first dimension"
               | `OnlyType | `FormalDecl ->
                 Kernel.fatal "VLA should be accepted in this context"
             end
           | _ -> ());
          if Cil.isZero len' && not allowZeroSizeArrays &&
             not Machine.(gccMode () || msvcMode ())
          then
            Kernel.error ~once:true ~current:true
              "zero-length arrays %s" (Machdep.allowed_machdep "GCC/MSVC");
          Some len'
      in
      let al' = doAttributes ghost al in
      if context <> `FormalDecl && Ast_attributes.contains "static" al' then
        Kernel.error ~once:true ~current:true
          "static specifier inside array argument is allowed only in \
           function argument";
      let t = mk_tarray ~tattr:al' bt lo in
      doDeclType t acc d

    | Cabs.PROTO (d, args, ghost_args, isva) ->
      (* Start a scope for the parameter names *)
      enterScope ();
      (* Intercept the old-style use of varargs.h. On GCC this means that
       * we have ellipsis and a last argument "builtin_va_alist:
       * builtin_va_alist_t". On MSVC we do not have the ellipsis and we
       * have a last argument "va_alist: va_list" *)
      let args', isva' =
        if args != [] && Machine.msvcMode () = not isva then begin
          let newisva = ref isva in
          let rec doLast = function
              [([Cabs.SpecType (Cabs.Tnamed atn)], (an, Cabs.JUSTBASE, [], _))]
              when isOldStyleVarArgTypeName atn &&
                   isOldStyleVarArgName an -> begin
                (* Turn it into a vararg *)
                newisva := true;
                (* And forget about this argument *)
                []
              end

            | a :: rest -> a :: doLast rest
            | [] -> []
          in
          let args' = doLast args in
          (args', !newisva)
        end else (args, isva)
      in
      (* Make the argument as for a formal *)
      let doOneArg argl_length is_ghost (s, (n, ndt, a, cloc)) : varinfo =
        let ghost = is_ghost || ghost in
        let s' = doSpecList cloc ghost n s in
        let vi =
          makeVarInfoCabs ~ghost ~kind:`FormalDecl (convLoc cloc) s' (n,ndt,a)
        in
        if isVoidType vi.vtype then begin
          if argl_length > 1 then
            Kernel.error ~once:true ~current:true
              "'void' must be the only parameter if specified";
          if vi.vname <> "" then
            Kernel.error ~once:true ~current:true
              "named parameter '%s' has void type" vi.vname
        end;
        (* Add the formal to the environment, so it can be referenced by
           other formals  (e.g. in an array type, although that will be
           changed to a pointer later, or though typeof).  *)
        addLocalToEnv ghost vi.vname (EnvVar vi);
        vi
      in
      let make_noopt_targs ghost args =
        let argl_length = List.length args in
        List.map (doOneArg argl_length ghost) args
      in
      let noopt_targs = make_noopt_targs false args' in
      let noopt_ghost_targs = make_noopt_targs true ghost_args in
      let targs : varinfo list option =
        match noopt_targs with
        | [] -> None (* No argument list *)
        | [t] when isVoidType t.vtype -> Some []
        | l -> Some l
      in
      let ghost_targs : varinfo list =
        match noopt_ghost_targs with
        | [t] when isVoidType t.vtype ->
          Kernel.error ~once:true ~current:true
            "ghost parameters list cannot be void" ;
          []
        | l -> l
      in
      let all_targs =
        match targs, ghost_targs with
        | None, [] -> None
        | None, g -> Some g
        | Some ng, g -> Some (ng @ g)
      in
      exitScope ();
      (* Turn [] types into pointers in the arguments and the result type.
       * Turn function types into pointers to respective. This simplifies
       * our life a lot, and is what the standard requires. *)
      let turnArrayIntoPointer (bt: typ)
          (lo: exp option) (a: attributes) : typ =
        let main_attrs = Ast_attributes.drop "static" a in
        let a' : attributes =
          match lo with
          | None -> []
          | Some l -> begin
              let static = if Ast_attributes.contains "static" a then
                  [("static",[])]
                else []
              in
              (* Transform the length into an attribute expression *)
              try
                let la : attrparam = expToAttrParam l in
                ("arraylen", [ la ]) :: static
              with NotAnAttrParam _ -> begin
                  Kernel.warning ~once:true ~current:true
                    "Cannot represent the length '%a' of array as an attribute"
                    Cil_printer.pp_exp l
                  ;
                  static (* Leave unchanged *)
                end
            end
        in
        let tattr = Ast_attributes.add_list a' main_attrs in
        mk_tptr ~tattr bt
      in
      let rec fixupArgumentTypes (argidx: int) (args: varinfo list) : unit =
        match args with
        | [] -> ()
        | a :: args' ->
          let t = unrollType a.vtype in
          (match t.tnode with
           | TArray (bt, lo) ->
             (* Note that for multi-dimensional arrays we strip off only
                the first TArray and leave bt alone. *)
             let real_type = turnArrayIntoPointer bt lo t.tattr in
             Cil.update_var_type a real_type
           | TFun _ -> Cil.update_var_type a (mk_tptr a.vtype)
           | TComp _ -> begin
               match isTransparentUnion a.vtype with
               | None ->  ()
               | Some fstfield ->
                 transparentUnionArgs :=
                   (argidx, a.vtype) :: !transparentUnionArgs;
                 Cil.update_var_type a fstfield.ftype;
             end
           | _ -> ());
          fixupArgumentTypes (argidx + 1) args'
      in
      let args =
        match all_targs with
        | None -> None
        | Some argl ->
          fixupArgumentTypes 0 argl;
          let arg_type_from_vi vi =
            let attrs =
              if vi.vghost then
                cabsAddAttributes [(Ast_attributes.frama_c_ghost_formal, [])] vi.vattr
              else
                vi.vattr
            in (vi.vname, vi.vtype, attrs)
          in
          Some (List.map arg_type_from_vi argl)
      in
      let tres =
        match unrollType bt with
        | { tnode = TArray(t,lo); tattr } -> turnArrayIntoPointer t lo tattr
        | _ -> bt
      in
      (* Drop qualifiers on the return type. They are meaningless (qualifiers
         make sense only on l-values), and they make life more complicated:
         the return type of the function is used e.g. for the type of retres,
         and probably in many other places. *)
      let tres = Cil.type_remove_qualifier_attributes tres in
      let t = mk_tfun tres args isva' in
      doDeclType t acc d
  in
  doDeclType bt [] dt

(* If this is a declarator for a variable size array then turn it into a
   pointer type and a length *)
and isVariableSizedArray ghost (dt: Cabs.decl_type)
  : (Cabs.decl_type * chunk * exp) option =
  let res = ref None in
  let rec findArray = function
      ARRAY (JUSTBASE, al, lo) when lo.expr_node != Cabs.NOTHING ->
      (* Checks whether the expression is an integer constant expression,
         that is:
         – it contains no side-effect
         – it can be evaluated at compile-time
         Note that we should not pass true as asconst argument for doExp,
         since we are precisely trying to determine whether the expression
         is a constant or not.
      *)
      let (_, se, e', _) =
        doExp (ghost_local_env ghost) CMayConst lo (AExp (Some intType)) in
      if isNotEmpty se || not (isConstant e') then begin
        res := Some (se, e');
        PTR (al, JUSTBASE)
      end else
        ARRAY (JUSTBASE, al, lo)
    | ARRAY (dt, al, lo) -> ARRAY (findArray dt, al, lo)
    | PTR (al, dt) -> PTR (al, findArray dt)
    | JUSTBASE -> JUSTBASE
    | PARENTYPE (prea, dt, posta) -> PARENTYPE (prea, findArray dt, posta)
    | PROTO (dt, f, g, a) -> PROTO (findArray dt, f, g, a)
  in
  let dt' = findArray dt in
  match !res with
  | None -> None
  | Some (se, e) -> Some (dt', se, e)

and doOnlyType loc ghost specs dt =
  let bt',sto,inl,attrs = doSpecList loc ghost "" specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true "Storage or inline specifier in type only";
  let tres, nattr =
    doType ghost `OnlyType AttrType bt' (Cabs.PARENTYPE(attrs, dt, []))
      ~allowVarSizeArrays:true
  in
  if nattr <> [] then
    Kernel.error ~once:true ~current:true
      "Name attributes in only_type: %a" Cil_printer.pp_attributes nattr;
  tres


and makeCompType loc ghost (isstruct: bool)
    (n: string)
    ~(norig: string)
    (nglist: Cabs.field_group list)
    (a: attribute list) =
  (* Make a new name for the structure *)
  let kind = if isstruct then "struct" else "union" in
  let n', _  = newAlphaName ghost true kind n in
  (* Create the self cell for use in fields and forward references. Or maybe
   * one exists already from a forward reference  *)
  let comp, _ = createCompInfo isstruct n' ~norig in
  let rec fold f acc = function
    | [] -> acc
    | [x] -> f ~last:true acc x
    | x :: l -> fold f (f ~last:false acc x) l
  in

  let addFieldGroup ~last:last_group (flds : fieldinfo list)
      ((s: Cabs.spec_elem list), (nl: (Cabs.name * Cabs.expression option) list)) =
    let open Current_loc.Operators in
    (* Do the specifiers exactly once *)
    let sugg,loc = match nl with
      | [] -> "", Current_loc.get()
      | ((n, _, _, loc), _) :: _ -> n,loc
    in
    let bt, sto, inl, attrs = doSpecList loc ghost sugg s in
    (* Do the fields *)
    let addFieldInfo ~last:last_field (flds : fieldinfo list)
        (((n,ndt,a,cloc) : Cabs.name), (widtho : Cabs.expression option))
      : fieldinfo list =
      let source = fst cloc in
      let<> UpdatedCurrentLoc = cloc in
      if sto <> NoStorage || inl then
        Kernel.error ~once:true ~source "Storage or inline not allowed for fields";
      let allowZeroSizeArrays = Machine.(gccMode () || msvcMode ()) in
      let ftype, fattr =
        doType
          ~allowZeroSizeArrays ghost `FieldDecl (AttrName false) bt
          (Cabs.PARENTYPE(attrs, ndt, a))
      in
      (* check for fields whose type is incomplete. In particular, this rules
         out circularity:
         struct C1 { struct C2 c2; };          //This line is now an error.
         struct C2 { struct C1 c1; int dummy; };
      *)
      if Cil.isFunctionType ftype then
        Kernel.error ~source
          "field `%s' declared as a function" n
      else if Cil.has_flexible_array_member ftype && isstruct then begin
        if not (last_group && last_field) then
          Kernel.error ~source
            "non-final field `%s' declared with a type containing a flexible \
             array member."
            n
        else if not Machine.(gccMode() || msvcMode ()) then
          Kernel.error ~source
            "field `%s' declared with a type containing a flexible array \
             member %s."
            n (Machdep.allowed_machdep "GCC/MSVC")
      end
      else if not (Cil.isCompleteType ~allowZeroSizeArrays ftype)
      then begin
        if Cil.isUnsizedArrayType ftype && last_group && last_field
        then
          (* possible flexible array member; check if struct contains at least
               one other field *)
          if flds = [] then (* struct is empty *)
            Kernel.error ~source
              "flexible array member '%s' (type %a) \
               not allowed in otherwise empty struct"
              n Cil_datatype.Typ.pretty ftype
          else (* valid flexible array member *) ()
        else
          Kernel.error ~source
            "field `%s' is declared with incomplete type %a"
            n Cil_datatype.Typ.pretty ftype
      end;
      let fbitfield, ftype =
        match widtho with
        | None -> None, ftype
        | Some w -> begin
            let source = fst w.expr_loc in
            (match unrollTypeNode ftype with
             | TInt _ -> ()
             | TEnum _ -> ()
             | _ ->
               Kernel.abort ~once:true ~source
                 "Base type for bitfield is not an integer type");
            match isIntegerConstant ghost w with
            | None ->
              Kernel.abort ~source
                "bitfield width is not a valid integer constant";
            | Some s as w ->
              begin
                if s < 0 then
                  Kernel.abort ~source "negative bitfield width (%d)" s;
                try
                  if s > Cil.bitsSizeOf ftype then
                    Kernel.error ~source
                      "bitfield width (%d) exceeds its type (%a, %d bits)"
                      s Cil_datatype.Typ.pretty ftype (Cil.bitsSizeOf ftype)
                with
                  SizeOfError _ ->
                  Kernel.fatal ~source
                    "Unable to compute size of %a" Cil_datatype.Typ.pretty ftype
              end;
              let ftype =
                typeAddAttributes
                  [(Ast_attributes.bitfield_attribute_name, [AInt (Integer.of_int s)])]
                  ftype
              in
              w, ftype
          end
      in
      (* Compute the order of the field in the structure *)
      let forder = match flds with
        | [] -> 0
        | { forder=previous_order } :: _ -> previous_order + 1
      in
      (* If the field is unnamed and its type is a structure of union type
       * then give it a distinguished name  *)
      let fname =
        if n = missingFieldName then
          if isStructOrUnionType ftype then
            begin
              Kernel.warning ~wkey:Kernel.wkey_c11 ~once:true ~current:true
                "unnamed fields are a C11 extension";
              incr anonCompFieldNameId;
              anonCompFieldName ^ (string_of_int !anonCompFieldNameId)
            end
          else
            n
        else
          begin
            if fbitfield = Some 0 then
              Kernel.error ~source:(fst cloc)
                "named bitfield (%s) with zero width" n;
            n
          end
      in
      let rec is_circular t =
        match unrollTypeNode t with
        | TArray (bt, _) -> is_circular bt
        | TComp comp' ->
          if Cil_datatype.Compinfo.equal comp comp' then begin
            (* abort and not error, as this circularity could lead
               to infinite recursion... *)
            abort_context
              "field %s declaration contains a circular reference to type %s %s"
              fname
              (if comp.cstruct then "struct" else "union")
              comp.cname;
          end else
            List.iter
              (fun f -> is_circular f.ftype)
              (Option.value ~default:[] comp'.cfields);
        | _ -> ()
      in
      is_circular ftype;
      { fcomp =  comp;
        forder;
        forig_name = n;
        fname;
        ftype;
        fbitfield;
        fattr;
        floc =  convLoc cloc;
        faddrof = false;
        fsize_in_bits = None;
        foffset_in_bits = None;
      } :: flds
    in
    fold addFieldInfo flds nl
  in

  (* Do regular fields first. *)
  let to_field = function
    | STATIC_ASSERT_FG (e, s, loc) ->
      let (_, _, cond_exp, _) = doExp empty_local_env CConst e ADrop in
      begin
        match Cil.constFoldToInt ~machdep:true cond_exp with
        | Some i ->
          if Integer.(equal i zero) then
            Kernel.error ~source:(fst loc) "static assertion failed%s%s@."
              (if s <> "" then ": " else "") s
        | None ->
          Kernel.error ~source:(fst loc)
            "failed to evaluate constant expression in static assertion:@ \
             @[%a@]"
            Cprint.print_expression e
      end;
      (* _Static_assert is not stored in the Cil AST *)
      None
    | FIELD (f,g) -> Some (f,g) in
  let flds = List.filter_map to_field nglist in
  let flds = List.rev (fold addFieldGroup [] flds) in

  let fld_table = Hashtbl.create 17 in
  let check f =
    try
      let oldf = Hashtbl.find fld_table f.fname in
      let source = fst f.floc in
      Kernel.error ~source
        "field %s occurs multiple times in aggregate %a. \
         Previous occurrence is at line %d."
        f.fname Cil_datatype.Typ.pretty (mk_tcomp comp)
        (fst oldf.floc).Filepath.pos_lnum
    with Not_found ->
      (* Do not add unnamed bitfields: they can share the empty name. *)
      if f.fname <> "" then Hashtbl.add fld_table f.fname f
  in
  if flds = [] && not (Machine.acceptEmptyCompinfo ()) then
    Kernel.error ~current:true ~once:true
      "empty %ss %s"
      (if comp.cstruct then "struct" else "union")
      (Machdep.allowed_machdep "GCC/MSVC");
  List.iter check flds;
  if comp.cfields <> None then begin
    let old_fields = Option.get comp.cfields in
    (* This appears to be a multiply defined structure. This can happen from
     * a construct like "typedef struct foo { ... } A, B;". This is dangerous
     * because at the time B is processed some forward references in { ... }
     * appear as backward references, which could lead to circularity in
     * the type structure. We do a thorough check and then we reuse the type
     * for A *)
    if List.length old_fields <> List.length flds
    || (List.exists2 (fun f1 f2 -> not (Cil_datatype.Typ.equal f1.ftype f2.ftype))
          old_fields flds)
    then
      Kernel.error ~once:true ~current:true
        "%s seems to be multiply defined" (compFullName comp)
  end else
    begin
      comp.cfields <- Some flds;
      let fields_with_pragma_attrs =
        List.map (fun fld ->
            (* note: in the call below, we CANNOT use fld.fcomp.cattr because it has not
               been filled in yet, so we need to pass the list of attributes [a] to it *)
            {fld with fattr = (process_pragmas_pack_align_field_attributes fld fld.fattr a)}
          ) flds
      in
      comp.cfields <- Some fields_with_pragma_attrs
    end;

  (*  ignore (E.log "makeComp: %s: %a\n" comp.cname d_attrlist a); *)
  let a = Ast_attributes.add_list comp.cattr a in
  comp.cattr <- process_pragmas_pack_align_comp_attributes loc comp a;
  let res = mk_tcomp comp in
  (* Create a typedef for this one *)
  cabsPushGlobal (GCompTag (comp, Current_loc.get ()));

  (* There must be a self cell created for this already *)
  addLocalToEnv ghost (kindPlusName kind n) (EnvTyp res);
  (* Now create a typedef with just this type *)
  res

and preprocessCast loc ghost (specs: Cabs.specifier)
    (dt: Cabs.decl_type)
    (ie: Cabs.init_expression)
  : Cabs.specifier * Cabs.decl_type * Cabs.init_expression =
  let typ = doOnlyType loc ghost specs dt in
  (* If we are casting to a union type then we have to treat this as a
   * constructor expression. This is to handle the gcc extension that allows
   * cast from a type of a field to the type of the union  *)
  (* However, it may just be casting of a whole union to its own type.  We
   * will resolve this later, when we'll convert casts to unions. *)
  let ie' =
    match unrollTypeNode typ, ie with
    | TComp c, Cabs.SINGLE_INIT _ when not c.cstruct ->
      Cabs.COMPOUND_INIT [(Cabs.INFIELD_INIT ("___matching_field",
                                              Cabs.NEXT_INIT),
                           ie)]
    | _, _ -> ie
  in
  (* Maybe specs contains an unnamed composite. Replace with the name so that
   * when we do again the specs we get the right name  *)
  let specs1 =
    match typ.tnode with
    | TComp ci ->
      List.map
        (function
            Cabs.SpecType (Cabs.Tstruct ("", _, [])) ->
            Cabs.SpecType (Cabs.Tstruct (ci.cname, None, []))
          | Cabs.SpecType (Cabs.Tunion ("", _, [])) ->
            Cabs.SpecType (Cabs.Tunion (ci.cname, None, []))
          | s -> s) specs
    | _ -> specs
  in
  specs1, dt, ie'

and getIntConstExp ghost (aexp) : exp =
  let loc = aexp.expr_loc in
  let _, c, e, _ = doExp (ghost_local_env ghost) CConst aexp (AExp None) in
  if not (isEmpty c) then
    Kernel.error ~once:true ~current:true "Constant expression %a has effects"
      Cil_printer.pp_exp e;
  match e.enode with
  (* first, filter for those Const exps that are integers *)
  | Const (CInt64 _ ) -> e
  | Const (CEnum _) -> e
  | Const (CChr i) -> new_exp ~loc (Const(charConstToIntConstant i))

  (* other Const expressions are not ok *)
  | Const _ ->
    abort_context "Expected integer constant and got %a"
      Cil_printer.pp_exp e

  (* now, anything else that 'doExp true' returned is ok (provided
     that it didn't yield side effects); this includes, in particular,
     the various sizeof and alignof expression kinds *)
  | _ -> e

and isIntegerConstant ghost (aexp) : int option =
  match doExp (ghost_local_env ghost) CMayConst aexp (AExp None) with
  | (_, c, e, _) when isEmpty c -> constFoldToInteger e
  | _ -> None

(* Process an expression and in the process do some type checking,
 * extract the effects as separate statements.
 * doExp returns the following 4-uple:
 * - a list of read accesses performed for the evaluation of the expression
 * - a chunk representing side-effects occurring during evaluation
 * - the CIL expression
 * - its type.
*)
and doExp local_env
    (asconst: expConst)   (* This expression is used as a constant *)
    (e: Cabs.expression)
    (what: expAction)
  =
  let open Current_loc.Operators in
  let ghost = local_env.is_ghost in
  let loc = e.expr_loc in
  (* will be reset at the end of the compilation of current expression. *)
  let<> UpdatedCurrentLoc = loc in
  let checkVoidLval e t =
    if (match e.enode with Lval _ -> true | _ -> false) && isVoidType t then
      abort_context "lvalue of type void: %a@\n" Cil_printer.pp_exp e
  in
  (* A subexpression of array type is automatically turned into StartOf(e).
   * Similarly an expression of function type is turned into AddrOf. So
   * essentially doExp should never return things of type TFun or TArray *)
  let processArrayFun e t =
    let loc = e.eloc in
    let t' = unrollType t in
    match e.enode, t'.tnode with
    | (Lval(lv) | CastE(_, {enode = Lval lv})), TArray(tbase, _) ->
      mkStartOfAndMark loc lv, mk_tptr ~tattr:t'.tattr tbase
    | (Lval(lv) | CastE(_, {enode = Lval lv})), TFun _  ->
      mkAddrOfAndMark loc lv, mk_tptr t
    | _, (TArray _ | TFun _) ->
      abort_context
        "Array or function expression is not lval: %a@\n"
        Cil_printer.pp_exp e
    | _ -> e, t
  in
  (* Before we return we call finishExp *)
  let finishExp ?(newWhat=what) reads (se: chunk) (e: exp) (t: typ) =
    match newWhat with
    | ADrop
    | AType ->
      let (e', t') = processArrayFun e t in
      (reads, se, e', t')
    | AExpLeaveArrayFun ->
      (reads, se, e, t)
    (* It is important that we do not do "processArrayFun" in
     * this case. We exploit this when we process the typeOf construct *)
    | AExp _ ->
      let (e', t') = processArrayFun e t in
      checkVoidLval e' t';
      (*
        ignore (E.log "finishExp: e'=%a, t'=%a\n"
        Cil_printer.pp_exp e' d_type t');
       *)
      (reads, se, e', t')

    | ASet (is_real_write,lv, r, lvt) -> begin
        (* See if the set was done already *)
        match e.enode with
        | Lval(lv') when lv == lv' ->
          (reads,se, e, t) (* if this is the case, the effects have also been
                              taken into account in the chunk. *)
        | _ ->
          let (e', t') = processArrayFun e t in
          let (t'', e'') = castTo t' lvt e' in
          checkVoidLval e'' t'';
          (*Kernel.debug "finishExp: e = %a\n  e'' = %a\n" Cil_printer.pp_exp e Cil_printer.pp_exp e'';*)
          let writes = if is_real_write then [lv] else [] in
          ([], (* the reads are incorporated in the chunk. *)
           ((unspecified_chunk empty) @@@ (remove_reads lv se, ghost))
           +++
           (mkStmtOneInstr ~ghost ~valid_sid (Set(lv, e'', Current_loc.get ())),
            writes,writes,
            List.filter (fun x -> not (LvalStructEq.equal x lv)) r @ reads),
           e'', t'')

      end
  in
  let result =
    match e.expr_node with
    | Cabs.PAREN e -> doExp (paren_local_env local_env) asconst e what
    | Cabs.NOTHING when what = ADrop ->
      finishExp [] (unspecified_chunk empty) (integer ~loc 0) intType
    | Cabs.NOTHING ->
      let res = new_exp ~loc (Const(CStr "exp_nothing")) in
      finishExp [] (unspecified_chunk empty) res (typeOf res)
    (* Do the potential lvalues first *)
    | Cabs.VARIABLE n -> begin
        if is_stdlib_function_macro n then begin
          (* These must be macros. They can be implemented with a function
             of the same name, but in that case, it is not possible to
             take the address of the function (or do anything else than
             calling the function, which is matched later on). *)
          Kernel.warning ~wkey:Kernel.wkey_cert_msc_38 ~current:true
            "%s is a standard macro. Its definition cannot be suppressed, \
             see CERT C coding rules MSC38-C" n
        end;
        (* Look up in the environment *)
        try
          let env = if ghost then ghost_env else env in
          let envdata = Datatype.String.Hashtbl.find env n in
          match envdata with
          | EnvVar vi, _ ->
            let lval = var vi in
            let reads =
              if
                (* Always allow to read the address of an
                   array or a function, as it will never be written to:
                   no read/write interference is possible. *)
                Cil.isArrayType vi.vtype ||
                Cil.isFunctionType vi.vtype ||
                Lval.Set.mem lval local_env.authorized_reads
              then []
              else [ lval ]
            in
            (* if isconst &&
               not (isFunctionType vi.vtype) &&
               not (isArrayType vi.vtype)then
               Cil.error "variable appears in constant"; *)
            finishExp
              reads (unspecified_chunk empty)
              (new_exp ~loc (Lval lval)) (dropQualifiers vi.vtype)
          | EnvEnum item, _ ->
            let typ = Cil.typeOf item.eival in
            (*Kernel.debug "Looking for %s got enum %s : %a of type %a"
              n item.einame Cil_printer.pp_exp item.eival
              Cil_datatype.Typ.pretty typ; *)
            if Machine.lower_constants () then
              finishExp [] (unspecified_chunk empty) item.eival typ
            else
              finishExp []
                (unspecified_chunk empty)
                (new_exp ~loc (Const (CEnum item)))
                typ
          | _ -> raise Not_found
        with Not_found -> begin
            if isOldStyleVarArgName n then
              abort_context
                "Cannot resolve variable %s. \
                 This could be a CIL bug due to \
                 the handling of old-style variable argument functions"
                n
            else if only_ghost_symbol n then
              abort_context
                "Variable %s is a ghost symbol. \
                 It cannot be used in non-ghost context. \
                 Did you forget a /*@@ ghost ... /?" n
            else
              abort_context "Cannot resolve variable %s" n
          end
      end
    | Cabs.INDEX (e1, e2) -> begin
        (* Recall that doExp turns arrays into StartOf pointers *)
        let (r1, se1, e1', t1) =
          doExp (no_paren_local_env local_env) CNoConst e1 (AExp None) in
        let (r2,se2, e2', t2) =
          doExp (no_paren_local_env local_env) CNoConst e2 (AExp None) in
        let se = se1 @@@ (se2, ghost) in
        let (e1'', t1, e2'', tresult) =
          (* Either e1 or e2 can be the pointer *)
          match unrollTypeNode t1, unrollTypeNode t2 with
          | TPtr t1e, (TInt _|TEnum _) -> e1', t1, e2', t1e
          | (TInt _|TEnum _), TPtr t2e -> e2', t2, e1', t2e
          | _ ->
            abort_context
              "Expecting exactly one pointer type in array access %a[%a] (%a \
               and %a)"
              Cil_printer.pp_exp e1' Cil_printer.pp_exp e2'
              Cil_datatype.Typ.pretty t1 Cil_datatype.Typ.pretty t2
        in
        (* We have to distinguish the construction based on the type of e1'' *)
        let res =
          match e1''.enode with
          | StartOf array -> (* A real array indexing operation *)
            addOffsetLval (Index(e2'', NoOffset)) array
          | _ -> (* Turn into *(e1 + e2) *)
            mkMem
              ~addr:(new_exp ~loc:e1''.eloc (BinOp(PlusPI, e1'', e2'', t1)))
              ~off:NoOffset
        in
        (* Do some optimization of StartOf *)
        let reads =
          let l = r1 @ r2 in
          if Lval.Set.mem res local_env.authorized_reads
          then l
          else res :: l
        in
        finishExp reads se (new_exp ~loc (Lval res)) (dropQualifiers tresult)
      end
    | Cabs.UNARY (Cabs.MEMOF, e) ->
      if asconst = CConst then
        Kernel.warning ~current:true "MEMOF in constant";
      let (r,se, e', t) =
        doExp (no_paren_local_env local_env) CNoConst e (AExp None)
      in
      let tresult =
        match unrollTypeNode t with
        | TPtr te -> te
        | _ ->
          abort_context
            "attempted to dereference an expression of non-pointer type %a"
            Cil_datatype.Typ.pretty t
      in
      let res = mkMem ~addr:e' ~off:NoOffset in
      let reads =
        if Lval.Set.mem res local_env.authorized_reads
        then r
        else res :: r
      in
      finishExp reads se (new_exp ~loc (Lval res)) (dropQualifiers tresult)

    (* e.str = (& e + off(str)). If e = (be + beoff) then e.str = (be
     * + beoff + off(str))  *)
    | Cabs.MEMBEROF (e, str) ->
      (* member of is actually allowed if we only take the address *)
      (* if isconst then Cil.error "MEMBEROF in constant";  *)
      let (r,se, e', t') =
        doExp (no_paren_local_env local_env) CNoConst e (AExp None)
      in
      let lv =
        match e'.enode with
        | Lval x -> x
        | CastE(_, { enode = Lval x}) -> x
        | _ ->
          Kernel.fatal ~current:true
            "expected an lvalue as left-hand side of access to field %s" str
      in
      (* We're not reading the whole lval, just a chunk of it. *)
      let r =
        List.filter (fun x -> not (Lval.equal x lv)) r
      in
      let field_offset =
        match unrollTypeNode t' with
        | TComp comp -> findField str comp
        | _ ->
          abort_context "expecting a struct with field %s" str
      in
      let lv' = addOffsetLval field_offset lv in
      let field_type = typeOfLval lv' in
      let reads =
        if Lval.Set.mem lv' local_env.authorized_reads
        then r
        else lv':: r
      in
      contains_temp_subarray := (Cil.isArrayType field_type && nested_call e);
      finishExp reads se (new_exp ~loc (Lval lv')) (dropQualifiers field_type)

    (* e->str = * (e + off(str)) *)
    | Cabs.MEMBEROFPTR (e, str) ->
      if asconst = CConst then
        Kernel.warning ~current:true "MEMBEROFPTR in constant";
      let (r,se, e', t') =
        doExp (no_paren_local_env local_env) CNoConst e (AExp None)
      in
      let pointedt = match unrollTypeNode t' with
        | TPtr t1 -> t1
        | TArray (t1,_) -> t1
        | _ -> abort_context "expecting a pointer to a struct"
      in
      let field_offset = match unrollType pointedt with
        | { tnode = TComp comp } -> findField str comp
        | t ->
          abort_context
            "expecting a struct with field %s. Found %a. t1 is %a"
            str Cil_datatype.Typ.pretty t Cil_datatype.Typ.pretty t'
      in
      let lv' = mkMem ~addr:e' ~off:field_offset in
      let field_type = typeOfLval lv' in
      let reads =
        if Lval.Set.mem lv' local_env.authorized_reads
        then r
        else lv' :: r
      in
      finishExp reads se (new_exp ~loc (Lval lv')) (dropQualifiers field_type)

    | Cabs.CONSTANT ct -> begin
        match ct with
        | Cabs.CONST_INT str -> begin
            let res =
              match parseIntExpRes ~loc str with
              | Ok e -> e
              | Error msg ->
                Kernel.error ~current:true "%s" msg;
                (* assign an arbitrary expression,
                   since we must return something *)
                Cil.one ~loc
            in
            finishExp [] (unspecified_chunk empty) res (typeOf res)
          end

        | Cabs.CONST_WSTRING (ws: int64 list) ->
          let res =
            new_exp ~loc
              (Const(CWStr ((* intlist_to_wstring *) ws)))
          in
          finishExp [] (unspecified_chunk empty) res (typeOf res)

        | Cabs.CONST_STRING s ->
          (* Maybe we buried __FUNCTION__ in there *)
          let s' =
            try
              let start = String.index s (Char.chr 0) in
              let l = String.length s in
              let tofind = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" in
              let past = start + String.length tofind in
              if past <= l &&
                 String.sub s start (String.length tofind) = tofind then
                (if start > 0 then String.sub s 0 start else "") ^
                !currentFunctionFDEC.svar.vname ^
                (if past < l then String.sub s past (l - past) else "")
              else
                s
            with Not_found -> s
          in
          let res = new_exp ~loc (Const(CStr s')) in
          finishExp [] (unspecified_chunk empty) res (typeOf res)

        | Cabs.CONST_CHAR char_list ->
          let a, b = (interpret_character_constant char_list) in
          finishExp [] (unspecified_chunk empty) (new_exp ~loc (Const a)) b

        | Cabs.CONST_WCHAR char_list ->
          (* matth: I can't see a reason for a list of more than one char
           * here, since the kinteger64 below will take only the lower 16
           * bits of value.  ('abc' makes sense, because CHAR constants have
           * type int, and so more than one char may be needed to represent
           * the value.  But L'abc' has type wchar, and so is equivalent to
           * L'c').  But gcc allows L'abc', so I'll leave this here in case
           * I'm missing some architecture dependent behavior. *)
          let value = reduce_multichar (Machine.wchar_type ()) char_list in
          let result = kinteger64 ~loc ~kind:(Machine.wchar_kind ())
              (Integer.of_int64 value)
          in
          finishExp [] (unspecified_chunk empty) result (typeOf result)

        | Cabs.CONST_FLOAT str -> begin
            Floating_point.(set_rounding_mode Nearest_even) ;
            let Parsed (format, parsed) = Typed_float.parse_exn str in
            let nearest_float = Typed_float.to_float parsed.nearest in
            if Typed_float.(parsed.lower <> parsed.upper) then
              Kernel.warning ~wkey:Kernel.wkey_decimal_float ~current:true
                "Floating-point constant %s is not represented exactly. \
                 Will use %a."
                str (Floating_point.pretty_normal ~use_hex:true) nearest_float;
            let kind = Typed_float.parsed_fkind format in
            let node = Const (CReal (nearest_float, kind, Some str)) in
            let typ = mk_tfloat kind in
            finishExp [] (unspecified_chunk empty) (new_exp ~loc node) typ
          end
      end

    | Cabs.TYPE_SIZEOF (bt, dt) ->
      let typ = doOnlyType loc local_env.is_ghost bt dt in
      fail_if_incompatible_sizeof ~ensure_complete:true "sizeof" typ;
      let res = new_exp ~loc (SizeOf typ) in
      finishExp [] (unspecified_chunk empty) res (Machine.sizeof_type ())

    | Cabs.EXPR_SIZEOF e ->
      (* Allow non-constants in sizeof *)
      (* Do not convert arrays and functions into pointers. *)
      let (_, se, e', lvt) =
        doExp (no_paren_local_env local_env) CNoConst e AExpLeaveArrayFun
      in
      fail_if_incompatible_sizeof ~ensure_complete:false "sizeof()" lvt;
      let scope_chunk = drop_chunk "sizeof" se e e' in
      let size =
        match e'.enode with
        (* Maybe we are taking the sizeof a variable-sized array *)
        | Lval (Var vi, NoOffset) -> begin
            try
              IH.find varSizeArrays vi.vid
            with Not_found -> new_exp ~loc (SizeOfE e')
          end
        | Const (CStr s) -> new_exp ~loc (SizeOfStr s)
        | _ -> new_exp ~loc (SizeOfE e')
      in
      finishExp [] scope_chunk size (Machine.sizeof_type ())

    | Cabs.TYPE_ALIGNOF (bt, dt) ->
      let typ = doOnlyType loc local_env.is_ghost bt dt in
      fail_if_incompatible_sizeof ~ensure_complete:true "alignof" typ;
      let res = new_exp ~loc (AlignOf typ) in
      finishExp [] (unspecified_chunk empty) res (Machine.sizeof_type ())

    | Cabs.EXPR_ALIGNOF e ->
      let (_, se, e', lvt) =
        doExp (no_paren_local_env local_env) CNoConst e AExpLeaveArrayFun
      in
      fail_if_incompatible_sizeof ~ensure_complete:false "alignof()" lvt;
      let scope_chunk = drop_chunk "alignof" se e e' in
      let e'' =
        match e'.enode with (* If we are taking the alignof an
                             * array we must drop the StartOf  *)
        | StartOf(lv) -> new_exp ~loc:e'.eloc (Lval(lv))

        | _ -> e'
      in
      finishExp [] scope_chunk (new_exp ~loc (AlignOfE(e'')))
        (Machine.sizeof_type ())

    (* In cparser, the types used as arguments of certain builtins are converted
       to casts so that they can be represented as expressions. The following
       matches are special cases to type those expressions. They are then
       converted to `sizeof typ` for CIL. *)
    | Cabs.CAST ((specs, dt), info)
      when is_for_builtin "__builtin_types_compatible_p" info ->
      let typ = doOnlyType loc local_env.is_ghost specs dt in
      let res = new_exp ~loc (SizeOf typ) in
      finishExp [] (unspecified_chunk empty) res (Machine.sizeof_type ())

    | Cabs.CAST ((specs, dt), info)
      when is_for_builtin "__builtin_va_arg" info ->
      let typ = doOnlyType loc local_env.is_ghost specs dt in
      if not (Cil.isCompleteType typ) then
        Kernel.error ~current:true "__builtin_va_arg on incomplete type '%a'"
          Cil_datatype.Typ.pretty typ;
      let res = new_exp ~loc (SizeOf typ) in
      finishExp [] (unspecified_chunk empty) res (Machine.sizeof_type ())
    (* End of special casts. *)

    | Cabs.CAST ((specs, dt), ie) ->
      let s', dt', ie' = preprocessCast loc local_env.is_ghost specs dt ie in
      (* We know now that we can do s' and dt' many times *)
      let typ = doOnlyType loc local_env.is_ghost s' dt' in
      let what' =
        match what with
        | AExp (Some _) -> AExp (Some typ)
        | AExp None -> what
        | ADrop | AType | AExpLeaveArrayFun -> what
        | ASet (_, _, _, lvt) ->
          (* If the cast from typ to lvt would be dropped, then we
           * continue with a Set *)
          if false && Cil_datatype.Typ.equal typ lvt then
            what
          else
            AExp None (* We'll create a temporary *)
      in
      (* Remember here if we have done the Set *)
      let (r,se, e', t'), (needcast: bool) =
        match ie' with
        | Cabs.SINGLE_INIT e ->
          doExp (no_paren_local_env local_env) asconst e what', true

        | Cabs.NO_INIT -> abort_context "missing expression in cast"

        | Cabs.COMPOUND_INIT _ -> begin
            (* Pretend that we are declaring and initializing a brand new
             * variable  *)
            let newvar = "__constr_expr_" ^ string_of_int (!constrExprId) in
            incr constrExprId;
            let spec_res = doSpecList loc local_env.is_ghost "" s' in
            let se1 =
              if !scopes == [] then begin
                (* This is a global.  Mark the new vars as static *)
                let spec_res' =
                  let t, _, inl, attrs = spec_res in
                  t, Static, inl, attrs
                in
                ignore (createGlobal loc local_env.is_ghost None spec_res'
                          ((newvar, dt', [], loc), ie'));
                (unspecified_chunk empty)
              end else
                createLocal
                  local_env.is_ghost spec_res ((newvar, dt', [], loc), ie')
            in
            (* Now pretend that e is just a reference to the newly created
             * variable *)
            let v = { expr_node = Cabs.VARIABLE newvar; expr_loc = loc } in
            let r, se, e', t' =
              doExp (no_paren_local_env local_env) asconst v what'
            in
            (* If typ is an array then the doExp above has already added a
             * StartOf. We must undo that now so that it is done once by
             * the finishExp at the end of this case *)
            let e2, t2 =
              match unrollTypeNode typ, e'.enode with
              | TArray _, StartOf lv -> new_exp ~loc (Lval lv), typ
              | _, _ -> e', t'
            in
            (* If we are here, then the type t2 is guaranteed to match the
             * type of the expression e2, so we do not need a cast. We have
             * to worry about this because otherwise, we might need to cast
             * between arrays or structures. *)
            (r, se1 @@@ (se, ghost), e2, t2), false
          end
      in
      let (t'', e'') =
        match typ.tnode with
        | TVoid when what' = ADrop -> (t', e') (* strange GNU thing *)
        |  _ ->
          (* Do this to check the cast, unless we are sure that we do not
           * need the check. *)
          let newtyp, newexp =
            if needcast then
              castTo ~fromsource:true t' typ e'
            else
              t', e'
          in
          newtyp, newexp
      in
      finishExp r se e'' t''

    | Cabs.UNARY(Cabs.MINUS, e) ->
      let (r, se, e', t) =
        doExp (no_paren_local_env local_env) asconst e (AExp None)
      in
      if isIntegralType t then
        let tres = integralPromotion t in
        let e'' =
          new_exp ~loc (UnOp(Neg, mkCastT ~oldt:t ~newt:tres e', tres))
        in
        finishExp r se e'' tres
      else
      if isArithmeticType t then
        finishExp r se (new_exp ~loc:e'.eloc (UnOp(Neg,e',t))) t
      else
        abort_context "Unary - on a non-arithmetic type"

    | Cabs.UNARY(Cabs.BNOT, e) ->
      let (r, se, e', t) =
        doExp (no_paren_local_env local_env) asconst e (AExp None)
      in
      if isIntegralType t then
        let tres = integralPromotion t in
        let e'' =
          new_exp ~loc (UnOp(BNot, mkCastT ~oldt:t ~newt:tres e', tres))
        in
        finishExp r se e'' tres
      else
        abort_context "Unary ~ on a non-integral type"

    | Cabs.UNARY(Cabs.PLUS, e) ->
      let (r, se, e, t as v) = doExp (no_paren_local_env local_env) asconst e what in
      if isIntegralType t then
        let newt = integralPromotion t in
        let e' = mkCastT ~oldt:t ~newt e in
        finishExp r se e' newt
      else
      if isArithmeticType t then
        v
      else
        abort_context "Unary + on a non-arithmetic type"

    | Cabs.UNARY(Cabs.ADDROF, e) ->
      (* some normalization is needed here to remove potential COMMA, QUESTION
         and PAREN. the normalization will take care of setting
         local_env.is_paren as appropriate while removing PAREN. *)
      let action local_env e what =
        match e.expr_node with
        | Cabs.COMMA _ | Cabs.QUESTION _ | Cabs.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | Cabs.VARIABLE s when
            isOldStyleVarArgName s
            && (match !currentFunctionFDEC.svar.vtype.tnode with
                  TFun (_, _, true) -> true | _ -> false) ->
          (* We are in an old-style variable argument function and we are
           * taking the address of the argument that was removed while
           * processing the function type. We compute the address based on
           * the address of the last real argument *)
          if Machine.msvcMode () then begin
            let rec getLast = function
              | [] ->
                abort_context
                  "old-style variable argument function without real \
                   arguments"
              | [ a ] -> a
              | _ :: rest -> getLast rest
            in
            let last = getLast !currentFunctionFDEC.sformals in
            let res = mkAddrOfAndMark e.expr_loc (var last) in
            let tres = typeOf res in
            let tres', res' = castTo tres ulongType res in
            (* Now we must add to this address to point to the next
             * argument. Round up to a multiple of 4  *)
            let sizeOfLast =
              (((bitsSizeOf last.vtype) + 31) / 32) * 4
            in
            let res'' =
              new_exp ~loc
                (BinOp(PlusA, res', kinteger ~loc IULong sizeOfLast, tres'))
            in
            finishExp [] (unspecified_chunk empty) res'' tres'
          end else begin (* On GCC the only reliable way to do this is to
                          * call builtin_next_arg. If we take the address of
                          * a local we are going to get the address of a copy
                          * of the local ! *)

            doExp local_env asconst
              (cabs_exp loc
                 (Cabs.CALL (cabs_exp loc (Cabs.VARIABLE "__builtin_next_arg"),
                             [cabs_exp loc (Cabs.CONSTANT (Cabs.CONST_INT "0"))],[])))
              what
          end

        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues *)
        | Cabs.CONSTANT (Cabs.CONST_STRING _) | Cabs.CONSTANT (Cabs.CONST_WSTRING _)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.GENERIC _ | Cabs.CAST (_, Cabs.COMPOUND_INIT _) ->
          begin
            let (r, se, e', t) =
              doExp local_env CNoConst e (AExp None)
            in
            (* ignore (E.log "ADDROF on %a : %a\n" Cil_printer.pp_exp e'
               Cil_datatype.Typ.pretty t); *)
            match e'.enode with
            | Lval x | CastE(_, {enode = Lval x}) | StartOf x ->
              (* Recover type qualifiers that were dropped by dropQualifiers
                 when the l-value was created *)
              let tres = match e'.enode with
                | Lval x | StartOf x -> Cil.typeOfLval x
                | _ -> t
              in
              let reads =
                match r with
                | x' :: r when LvalStructEq.equal x x' -> r
                | _ -> r
              in
              finishExp reads se (mkAddrOfAndMark loc x) (mk_tptr tres)

            | Const (CStr _ | CWStr _) ->
              (* string to array *)
              finishExp r se e' (mk_tptr t)

            (* Function names are converted into pointers to the function.
             * Taking the address-of again does not change things *)
            | AddrOf (Var v, NoOffset) when isFunctionType v.vtype ->
              finishExp r se e' t

            | _ ->
              abort_context "Expected lval for addrof. Got %a"
                Cil_printer.pp_exp e'
          end
        | _ -> abort_context "Unexpected operand for addrof"
      in
      normalize_unop Cabs.ADDROF action CNoConst
        (no_paren_local_env local_env) e what
    | Cabs.UNARY((Cabs.PREINCR|Cabs.PREDECR) as uop, e) ->
      let action local_env e _what =
        match e.expr_node with
        | Cabs.COMMA _ | Cabs.QUESTION _ | Cabs.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | (Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) | (* Regular lvalues *)
           Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _ |
           Cabs.GENERIC _ | Cabs.CAST _ (* A GCC extension *)) -> begin
            let uop' = if uop = Cabs.PREINCR then PlusA else MinusA in
            if asconst = CConst then
              Kernel.warning ~current:true "PREINCR or PREDECR in constant";
            let (r, se, e', t) = doExp local_env CNoConst e (AExp None) in
            let lv = get_lval_compound_assigned "++ or --" e' in
            let se' = remove_reads lv se in
            let r' =
              List.filter (fun x -> not (Lval.equal x lv)) r
            in
            let tresult, result =
              doBinOp loc uop' e' t (one ~loc:e'.eloc) intType
            in
            finishExp []
              (se' +++
               (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                  (Set(lv, snd (castTo tresult t result),
                       Current_loc.get ())),[],[lv],r'))
              e'
              t
          end
        | _ ->
          abort_context "Unexpected operand for prefix -- or ++"
      in
      normalize_unop uop action asconst (no_paren_local_env local_env) e what

    | Cabs.UNARY((Cabs.POSINCR|Cabs.POSDECR) as uop, e) ->
      let action local_env e what =
        match e.expr_node with
        | Cabs.COMMA _ | Cabs.QUESTION _ | Cabs.PAREN _ ->
          Kernel.fatal ~current:true "normalization of unop failed"
        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues *)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.GENERIC _ | Cabs.CAST _ (* A GCC extension *) -> begin
            if asconst = CConst then
              Kernel.warning ~current:true "POSTINCR or POSTDECR in constant";
            (* If we do not drop the result then we must save the value *)
            let uop' = if uop = Cabs.POSINCR then PlusA else MinusA in
            let (r,se, e', t) = doExp local_env CNoConst e (AExp None) in
            let lv = get_lval_compound_assigned "++ or --" e' in
            let se' = remove_reads lv se in
            let r' =
              List.filter (fun x -> not (Lval.equal x lv)) r
            in
            let tresult, opresult =
              doBinOp loc uop' e' t (one ~loc:e'.eloc)
                intType
            in
            let reads, se', result =
              if what <> ADrop && what <> AType then
                let descr =
                  Format.asprintf "%a%s"
                    Cil_descriptive_printer.pp_exp  e'
                    (if uop = Cabs.POSINCR then "++" else "--") in
                let tmp = newTempVar ~ghost loc descr true t in
                ([var tmp],
                 local_var_chunk se' tmp +++
                 (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                    (Set(var tmp, e', Current_loc.get ())),[],[],[]),
                 (* the tmp variable should not be investigated for
                    unspecified writes: it occurs at the right place in
                    the sequence.
                 *)
                 new_exp ~loc (Lval(var tmp)))
              else
                [],se, e'
            in
            finishExp reads
              (se' +++
               (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                  (Set(lv,
                       snd (castTo tresult (typeOfLval lv) opresult),
                       Current_loc.get ())),
                [],[lv], r'))
              result
              t
          end
        | _ ->
          abort_context "Unexpected operand for suffix ++ or --"
      in
      normalize_unop uop action asconst (no_paren_local_env local_env) e what

    | Cabs.BINARY(Cabs.ASSIGN, e1, e2) ->
      let action local_env asconst e what =
        match e.expr_node with
        | Cabs.COMMA _ | Cabs.QUESTION _ | Cabs.CAST (_,Cabs.SINGLE_INIT _) | Cabs.PAREN _ ->
          Kernel.fatal
            ~current:true "normalization of lval in assignment failed"
        | (Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) | (* Regular lvalues *)
           Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _ |
           Cabs.GENERIC _ ) -> begin
            if asconst = CConst then
              Kernel.warning ~current:true "ASSIGN in constant";
            let se0 = unspecified_chunk empty in
            let (r1,se1, e1', lvt) =
              doExp local_env CNoConst e AExpLeaveArrayFun
            in
            let lv =
              match e1'.enode with
              | Lval x when Cil.is_modifiable_lval x -> x
              | Lval x ->
                abort_context
                  "Cannot assign to non-modifiable lval %a"
                  Cil_printer.pp_lval x
              | StartOf lv ->
                abort_context
                  "Cannot assign array %a" Cil_printer.pp_lval lv
              | _ ->
                abort_context
                  "Expected lval for assignment. Got %a"
                  Cil_printer.pp_exp e1'
            in
            let se1' = remove_reads lv se1 in
            let r1' = List.filter (fun x -> not (Lval.equal x lv)) r1 in
            let local_env =
              { local_env with
                authorized_reads =
                  Lval.Set.add lv local_env.authorized_reads }
            in
            (*[BM]: is this useful?
              let (_, _, _) = doExp ghost false e2 (ASet(lv, lvt)) in*)
            (* Catch the case of an lval that might depend on itself,
               e.g. p[p[0]] when p[0] == 0.  We need to use a temporary
               here if the result of the expression will be used:
               tmp := e2; lv := tmp; use tmp as the result
               Test: small1/assign.c *)
            let needsTemp =
              not (isBitfield lv) && (* PC: BTS 933, 968 *)
              match what, lv with
              | (ADrop|AType), _ -> false
              | _, (Mem e, off) ->
                not (isConstant e) || not (isConstantOffset off)
              | _, (Var _, off) -> not (isConstantOffset off)
            in
            let r1, tmplv, se3 =
              if needsTemp then
                let descr =
                  Format.asprintf "%a" Cil_descriptive_printer.pp_lval lv
                in
                let tmp = newTempVar ~ghost loc descr true lvt in
                let chunk =
                  i2c
                    (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                       (Set(lv, new_exp ~loc:e1'.eloc (Lval(var tmp)), loc)),
                     [lv],[lv], r1')
                in
                ([],var tmp, local_var_chunk chunk tmp)
              else r1',lv, empty
            in
            let (r2,se2, _, _) =
              doExp local_env CNoConst e2 (ASet (not needsTemp, tmplv,  r1, lvt))
            in
            let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
            (* Format.eprintf "chunk for assigns is %a@." d_chunk se2; *)
            (* r1 is read in the assignment part itself *)
            finishExp r2  ((empty @@@ ((se0 @@@ se1') @@@ se2)) @@@ se3)
              (new_exp ~loc (Lval tmplv)) lvt
          end
        | _ -> abort_context "Invalid left operand for ASSIGN"
      in
      normalize_binop
        Cabs.ASSIGN action (no_paren_local_env local_env) asconst e1 e2 what
    | Cabs.BINARY((Cabs.ADD|Cabs.SUB|Cabs.MUL|Cabs.DIV|Cabs.MOD|Cabs.BAND|Cabs.BOR|Cabs.XOR|
                   Cabs.SHL|Cabs.SHR|Cabs.EQ|Cabs.NE|Cabs.LT|Cabs.GT|Cabs.GE|Cabs.LE) as bop,
                  e1, e2) ->
      let check_bitwise = is_bitwise_bop bop && not local_env.is_paren in
      let se0 = unspecified_chunk empty in
      let bop' = convBinOp bop in
      let (r1,se1, e1', t1) =
        doExp (no_paren_local_env local_env) asconst e1 (AExp None) in
      let (r2,se2, e2', t2) =
        doExp (no_paren_local_env local_env) asconst e2 (AExp None) in
      if check_bitwise then begin
        check_logical_operand e1 t1;
        check_logical_operand e2 t2;
      end;
      let tresult, result = doBinOp loc bop' e1' t1 e2' t2 in
      let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
      finishExp (r1 @ r2) ((se0 @@@ se1) @@@ se2) result tresult

    (* assignment operators *)
    | Cabs.BINARY((Cabs.ADD_ASSIGN|Cabs.SUB_ASSIGN|Cabs.MUL_ASSIGN|Cabs.DIV_ASSIGN|
                   Cabs.MOD_ASSIGN|Cabs.BAND_ASSIGN|Cabs.BOR_ASSIGN|Cabs.SHL_ASSIGN|
                   Cabs.SHR_ASSIGN|Cabs.XOR_ASSIGN) as bop, e1, e2) ->
      let se0 = unspecified_chunk empty in
      let action local_env asconst e _what =
        match e.expr_node with
        | Cabs.COMMA _ | Cabs.QUESTION _ | Cabs.PAREN _ ->
          Kernel.fatal ~current:true "normalization of lval in compound assignment failed"
        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) | (* Regular lvalues *)
          Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _ |
          Cabs.GENERIC _ | Cabs.CAST _ (* GCC extension *) -> begin
            if asconst = CConst then
              Kernel.warning ~current:true "op_ASSIGN in constant";
            let bop' = match bop with
              | Cabs.ADD_ASSIGN -> PlusA
              | Cabs.SUB_ASSIGN -> MinusA
              | Cabs.MUL_ASSIGN -> Mult
              | Cabs.DIV_ASSIGN -> Div
              | Cabs.MOD_ASSIGN -> Mod
              | Cabs.BAND_ASSIGN -> BAnd
              | Cabs.BOR_ASSIGN -> BOr
              | Cabs.XOR_ASSIGN -> BXor
              | Cabs.SHL_ASSIGN -> Shiftlt
              | Cabs.SHR_ASSIGN -> Shiftrt
              | _ -> assert false
            in
            let (r1,se1, e1', t1) = doExp local_env CNoConst e (AExp None) in
            let lv1 = get_lval_compound_assigned "assignment with arith" e1' in
            let se1' = remove_reads lv1 se1 in
            let r1' = List.filter (fun x -> not (Lval.equal x lv1)) r1 in
            let local_env =
              { local_env with
                authorized_reads =
                  Lval.Set.add lv1 local_env.authorized_reads }
            in
            let (r2, se2, e2', t2) = doExp local_env CNoConst e2 (AExp None) in
            let se2 = remove_reads lv1 se2 in
            let tresult, result = doBinOp loc bop' e1' t1 e2' t2 in
            (* We must cast the result to the type of the lv1, which may be
             * different than t1 if lv1 was a Cast *)
            let _, result' = castTo tresult (typeOfLval lv1) result in
            (* The type of the result is the type of the left-hand side  *)
            let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
            finishExp []
              (se0 @@@
               (empty @@@ (se1' @@@ se2) +++
                          (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                             (Set(lv1, result', loc)),
                           [lv1],[lv1], r1' @ r2)))
              e1'
              t1
          end
        | _ ->
          abort_context "Unexpected left operand for assignment with arith"
      in
      normalize_binop
        bop action (no_paren_local_env local_env) asconst e1 e2 what
    | Cabs.BINARY((Cabs.AND|Cabs.OR), _, _) | Cabs.UNARY(Cabs.NOT, _) -> begin
        let ce = doCondExp local_env asconst e in
        (* We must normalize the result to 0 or 1 *)
        match ce with
        | CEExp (se, ({enode = Const c;eloc=loc})) ->
          finishExp [] se
            (match isConstTrueFalse c with
             | `CTrue -> one ~loc
             | `CFalse -> zero ~loc)
            intType
        | CEExp (se, ({enode = UnOp(LNot, _, _)} as e)) ->
          (* already normalized to 0 or 1 *)
          finishExp [] se e intType
        | CEExp (se, e) ->
          let e' =
            let te = typeOf e in
            let _, zte = castTo intType te (zero ~loc:e.eloc) in
            new_exp ~loc (BinOp(Ne, e, zte, intType))
          in
          finishExp [] se e' intType
        | _ ->
          let tmp =
            newTempVar ~ghost loc "<boolean expression>" true intType
          in
          let condChunk =
            compileCondExp ~ghost ce
              (empty +++
               (mkStmtOneInstr ~ghost ~valid_sid
                  (Set(var tmp, integer ~loc 1,loc)),[],[],[]))
              (empty +++
               (mkStmtOneInstr ~ghost ~valid_sid
                  (Set(var tmp, integer ~loc 0,loc)),[],[],[]))
          in
          finishExp []
            (local_var_chunk condChunk tmp)
            (new_exp ~loc (Lval (var tmp)))
            intType
      end
    | Cabs.CALL({ expr_node = VARIABLE "__builtin_choose_expr"},
                args, ghost_args)
      when Machine.gccMode() ->
      (* __builtin_choose_expr is supposed to choose at compile time between
         two expressions, hence we have to handle it separately from the
         normal calls. *)
      begin
        match args, ghost_args with
        | [ cond; e1; e2 ], [] ->
          let _, chunk, cond, _ =
            doExp (no_paren_local_env local_env) CConst cond (AExp None)
          in
          if not (isEmpty chunk) then
            abort_context ~loc:cond.eloc
              "first argument of __builtin_choose_expr \
               shouldn't have side effect";
          let cond_is_true =
            match (Cil.constFold true cond).enode with
            | Const (CInt64 (v,_,_)) -> not (Z.equal v Z.zero)
            | Const (CReal(v,_,_)) -> Fc_float.compare v 0. <> 0
            | Const (CChr c) -> Char.code c <> 0
            | Const (CStr _) | Const (CWStr _) -> true
            | _ ->
              abort_context ~loc:cond.eloc
                "first argument of __builtin_choose_expr should be \
                 a compile-time constant"
          in
          if cond_is_true then begin
            doExp (no_paren_local_env local_env) asconst e1 what
          end else begin
            doExp (no_paren_local_env local_env) asconst e2 what
          end
        | _ ->
          abort_context "ill-formed call to __builtin_choose_expr"
      end
    | Cabs.CALL(f, args, ghost_args) ->
      let (rf,sf, f', ft') =
        match (stripParen f).expr_node with
        (* Treat the VARIABLE case separate because we might be calling a
         * function that does not have a prototype. In that case assume it
         * takes INTs as arguments  *)
        | Cabs.VARIABLE n -> begin
            try
              (* First look for polymorphic builtins. The typing rule is
                 luckily always the same one. *)
              let n = match n with
                | "__sync_add_and_fetch" | "__sync_sub_and_fetch"
                | "__sync_or_and_fetch" | "__sync_and_and_fetch"
                | "__sync_xor_and_fetch" | "__sync_nand_and_fetch"
                | "__sync_fetch_and_add" | "__sync_fetch_and_sub"
                | "__sync_fetch_and_or" | "__sync_fetch_and_and"
                | "__sync_fetch_and_xor" | "__sync_fetch_and_nand"
                | "__sync_bool_compare_and_swap"
                | "__sync_val_compare_and_swap"
                | "__sync_lock_release" | "__sync_lock_test_and_set" ->
                  begin
                    match args with
                    | a1::_ ->
                      (* The available prototypes are
                         typ' f(typ* a1,typ a2,typ a3,...);
                         typ' f(typ* a1,typ a2,...);
                         typ' f(typ* a1,...);
                         Hence we just infer the right type
                         looking at the first argument. *)
                      let _,c,_,t =
                        doExp (no_paren_local_env local_env) CNoConst a1 AType
                      in
                      clean_up_chunk_locals c;
                      let t = typeOf_pointed t in
                      Format.sprintf "%s_%sint%d_t"
                        n
                        (if isSignedInteger t then "" else "u")
                        (bitsSizeOf t)
                    | [] ->
                      Kernel.error ~once:true ~current:true
                        "Too few arguments for builtin %s" n;
                      n
                  end

                (* contrarily to the other builtins, __atomic_load and
                   __atomic_exchange generic versions do not share the same
                   signature as their specialized counterparts.
                   Hence, we'd have to change the args list as well.
                *)
                | "__atomic_load" | "__atomic_exchange" ->
                  Kernel.error ~once:true ~current:true
                    "Generic %s is not yet supported" n;
                  n
                (* for store and compare_exchange, the generic version is also
                   able to handle types of arbitrary size, via an external
                   function that takes the size of the type as argument as well.
                   Here too, we'd need to change the args list to support that.
                *)
                | "__atomic_store" | "__atomic_compare_exchange"
                | "__atomic_add_fetch"
                | "__atomic_sub_fetch" | "__atomic_and_fetch"
                | "__atomic_xor_fetch" | "__atomic_or_fetch"
                | "__atomic_nand_fetch" | "__atomic_fetch_add"
                | "__atomic_fetch_sub" | "__atomic_fetch_and"
                | "__atomic_fetch_xor" | "__atomic_fetch_or"
                | "__atomic_fetch_nand" ->
                  begin
                    match args with
                    | a1 :: _ ->
                      let _,c,_,t =
                        doExp (no_paren_local_env local_env) CNoConst a1 AType
                      in
                      clean_up_chunk_locals c;
                      let t = typeOf_pointed t in
                      Format.sprintf "%s_%d" n (bytesSizeOf t)
                    | [] ->
                      Kernel.error ~once:true ~current:true
                        "Too few arguments for builtin %s" n;
                      n
                  end
                | _ -> n
              in
              let vi, _ = lookupVar ghost n in
              let reads =
                if Lval.Set.mem
                    (var vi) local_env.authorized_reads
                   ||
                   (vi.vglob && Cil.isFunctionType vi.vtype)
                then []
                else [ var vi ]
              in
              (reads, unspecified_chunk empty,
               new_exp ~loc:f.expr_loc (Lval(var vi)), vi.vtype)
            (* Found. Do not use finishExp. Simulate what = AExp None  *)
            with Not_found -> begin
                if only_ghost_symbol n then
                  abort_context
                    "Function %s is a ghost symbol. \
                     It cannot be used in non-ghost context. \
                     Did you forget a /*@@ ghost ... /?" n ;
                Kernel.debug ~dkey:Kernel.dkey_typing_global
                  "Calling function %s without prototype." n ;
                let ftype =
                  mk_tfun ~tattr:[("missingproto",[])] intType None false
                in
                (* Add a prototype to the environment *)
                let proto, _ =
                  makeGlobalVarinfo false
                    (makeGlobalVar ~temp:false ~loc:f.expr_loc n ftype) in
                (* Make it EXTERN *)
                proto.vstorage <- Extern;
                proto.vdecl <- f.expr_loc;
                ImplicitPrototypeHook.apply proto;
                (* Add it to the file as well *)
                cabsPushGlobal
                  (GFunDecl (empty_funspec (),proto, f.expr_loc));
                ([var proto],unspecified_chunk empty,
                 new_exp ~loc:f.expr_loc (Lval(var proto)), ftype)
              end
          end
        | _ -> doExp (no_paren_local_env local_env) CNoConst f (AExp None)
      in
      (* Get the result type and the argument types *)
      let (resType, argTypes, isvar, f'', tattr) =
        match unrollType ft' with
        | { tnode = TFun(rt,at,isvar); tattr } -> (rt,at,isvar,f',tattr)
        | { tnode = TPtr t } -> begin
            match unrollType t with
            | { tnode = TFun (rt, at, isvar) } -> (* Make the function pointer
                                                   * explicit  *)
              let f'' =
                match f'.enode with
                | AddrOf lv -> new_exp ~loc:f'.eloc (Lval(lv))
                | _ ->
                  new_exp ~loc:f'.eloc
                    (Lval (mkMem ~addr:f' ~off:NoOffset))
              in
              (rt,at,isvar, f'',[])
            | x ->
              abort_context
                "Unexpected type of the called function %a: %a"
                Cil_printer.pp_exp f' Cil_datatype.Typ.pretty x
          end
        | x ->
          abort_context
            "Unexpected type of the called function %a: %a"
            Cil_printer.pp_exp f' Cil_datatype.Typ.pretty x
      in
      let argTypesList = argsToList argTypes in
      let warn_no_proto f =
        (* Do not punish twice users of completely undeclared functions. *)
        if not (Cil.typeHasAttribute "missingproto" f.vtype) then
          Kernel.warning ~source:(fst loc) ~wkey:Kernel.wkey_no_proto
            "Calling function %a that is declared without prototype.@ \
             Its formals will be inferred from actual arguments.@ \
             Declare it as %a(void) if the function does not take any \
             parameters."
            Cil_printer.pp_varinfo f
            Cil_printer.pp_varinfo f
      in
      (* Drop certain qualifiers from the result type *)
      let resType' = typeRemoveAttributes ["warn_unused_result"] resType in
      (* Before we do the arguments we try to intercept a few builtins. For
       * these we have defined then with a different type, so we do not
       * want to give warnings. We'll just leave the arguments of these
       * functions alone*)
      let isSpecialBuiltin =
        match f''.enode with
        | Lval (Var fv, NoOffset) -> Cil_builtins.is_special_builtin fv.vname
        | _ -> false
      in
      let init_chunk = unspecified_chunk empty in
      (* Do the arguments. In REVERSE order !!! Both GCC and MSVC do this *)
      let rec loopArgs ?(are_ghost=false) = function
        | ([], []) ->
          (match argTypes, f''.enode with
           | None, Lval (Var f,NoOffset) ->
             (* we call a function without prototype with 0 argument.
                Hence, it really has no parameter.
             *)
             if not isSpecialBuiltin && not are_ghost then begin
               warn_no_proto f;
               let typ = mk_tfun ~tattr resType (Some []) false in
               Cil.update_var_type f typ;
             end
           | None, _ (* TODO: treat function pointers. *)
           | Some _, _ -> ()
          );
          (init_chunk, [])

        | _, [] ->
          if not isSpecialBuiltin then
            Kernel.error ~once:true ~current:true
              "Too few%s arguments in call to %a."
              (if are_ghost then " ghost" else "") Cil_printer.pp_exp f' ;
          (init_chunk, [])

        | ((_, at, _) :: atypes, a :: args) ->
          let (ss, args') = loopArgs ~are_ghost (atypes, args) in
          (* Do not cast as part of translating the argument. We let
           * the castTo do this work. This was necessary for
           * test/small1/union5, in which a transparent union is passed
           * as an argument *)
          let (sa, a', att) =
            let local_env = add_ghost_to_local_env local_env are_ghost in
            let (r, c, e, t) =
              doExp (no_paren_local_env local_env) CNoConst a (AExp None)
            in
            (add_reads ~ghost:local_env.is_ghost loc r c, e, t)
          in
          let (texpected, a'') =
            castTo ~context:ContravariantToplevel att at a'
          in
          (* A posteriori check that the argument type was compatible,
             to generate a warning otherwise;
             if a'' = a', no check needs to be done (no cast was introduced).
             Note: this check is conservative (it may not emit warnings when
             it should), and compilers can often detect more errors. *)
          if not (Exp.equal a' a'') &&
             match Cil.isArithmeticType texpected, Cil.isArithmeticType att with
             | true, true -> (* never a problem *) false
             | true, false -> true
             | false, true ->
               (* pointer with no pointer: problematic, except NULL;
                  if expected pointer and got null pointer constant => ok *)
               not (Cil.isPointerType texpected && Ast_info.is_null_expr a')
             | false, false ->
               (* Ghost compatibility is considered 'after_cleanup' *)
               let texpected = Cil.typeRemoveAttributesDeep [ "ghost" ] texpected in
               let att = Cil.typeRemoveAttributesDeep [ "ghost" ] att in
               (* pointers: check compatible modulo void ptr and modulo
                  literal strings (too many warnings otherwise) *)
               let ok1 =
                 (* accept literal strings even when expecting non-const char*;
                    equivalent to GCC's default behavior (-Wno-write-strings) *)
                 (Typ.equal (Cil.unrollType texpected) charPtrType &&
                  Typ.equal (Cil.unrollType att) charConstPtrType) ||
                 (* all pointers are convertible to void* *)
                 (Cil.isVoidPtrType texpected && Cil.isPointerType att) ||
                 (* allow implicit void* -> char* conversion *)
                 (Cil.isAnyCharPtrType texpected && Cil.isVoidPtrType att) ||
                 (* always allow null pointers *)
                 (Cil.isPointerType texpected && Ast_info.is_null_expr a') ||
                 areCompatibleTypes ~context:ContravariantToplevel att texpected
               in
               let ok2 =
                 (* accept pointer to const type as long as the respective
                    argument (formal) is annotated with __fc_initialized_object
                    attribute. *)
                 let arg_is_initialized = Cil.is_initialized a' in
                 arg_is_initialized
                 && Cil.isPointerType texpected
                 && areCompatibleTypes ~context:CovariantToplevel att texpected
               in
               let ok =
                 if ok1 || ok2 then true
                 (* special warning for void* -> any* conversions;
                    this is equivalent to option '-Wc++-compat' in GCC *)
                 else if Cil.isVoidPtrType att && Cil.isPointerType texpected
                 then begin
                   Kernel.warning ~wkey:Kernel.wkey_implicit_conv_void_ptr
                     ~current:true ~once:true
                     "implicit conversion from %a to %a"
                     Cil_datatype.Typ.pretty voidPtrType
                     Cil_datatype.Typ.pretty texpected;
                   true
                 end else
                   false
               in
               not ok
          then
            Kernel.warning ~wkey:Kernel.wkey_incompatible_types_call
              ~current:true ~once:true
              "expected '%a' but got argument of type '%a': %a"
              Cil_datatype.Typ.pretty texpected Cil_datatype.Typ.pretty att
              Cil_printer.pp_exp a';
          (ss @@@ (sa, ghost), a'' :: args')

        | ([], args) -> (* No more types *)
          if not isvar && argTypes != None && not isSpecialBuiltin then
            (* Do not give a warning for functions without a prototype*)
            Kernel.error ~once:true ~current:true
              "Too many%s arguments in call to %a"
              (if are_ghost then " ghost" else "") Cil_printer.pp_exp f';
          let rec loop = function
              [] -> (init_chunk, [])
            | a :: args ->
              let (ss, args') = loop args in
              let (sa, a', _) =
                let (r, c, e, t) =
                  doExp (no_paren_local_env local_env) CNoConst a (AExp None)
                in
                (add_reads ~ghost:local_env.is_ghost loc r c, e, t)
              in
              (ss @@@ (sa, ghost), a' :: args')
          in
          let (chunk,args as res) = loop args in
          (match argTypes, f''.enode with
           | Some _,_ ->
             if isvar then begin
               (* use default argument promotion to infer the type of the
                  variadic actuals, see C11:6.5.2.2:7 *)
               promote_variadic_arguments res
             end else
               res
           | None, Lval (Var f, NoOffset)
             when not isSpecialBuiltin ->
             begin
               (* use default argument promotion to infer the type of the
                  function, see 6.5.2.2.6 *)
               assert (not isvar);
               (* No nullary variadics see C11:6.7.6 *)
               warn_no_proto f;
               let (prm_types,args) =
                 List.split
                   (List.mapi default_argument_promotion args)
               in
               let typ = mk_tfun ~tattr resType (Some prm_types) false in
               begin
                 try
                   (* Nested calls of a function without a prototype : inner
                      calls will update [f] type but the information is not
                      communicated to outer ones, hence [argTypes] is not up to
                      date and we need to check that types are compatibles
                      before updating [f] type (see issue-641-implicit-calls.c
                      test).
                   *)
                   ignore(Cil.compatibleTypes f.vtype typ);
                 with Cannot_combine msg ->
                   abort_context "nested calls of %s without a prototype and \
                                  incompatible arguments : %s" f.vname msg
               end;
               Cil.update_var_type f typ;
               Cil.setFormalsDecl f typ;
               (* We need to check that the update of [f] did not create
                  inconsistencies with call' arguments. It can happen when [f]
                  is used as an lvalue in its own arguments. Updating [f] type
                  will recursively change the type of [f] inside parameters ,
                  therefore [f] type is not up to date anymore, etc (see
                  issue-641-implicit-calls.c test). *)
               let check_arg e (_, at, _) =
                 let typ = Cil.typeOf e in
                 if not @@ Cil_datatype.Typ.equal typ at then
                   abort_context "call to %s with a reference to itself in its \
                                  own parameters" f.vname
               in
               (* args and prm_types should have the same length here, since
                  they both come from the same split.  *)
               List.iter2 check_arg args prm_types;
               (chunk,args)
             end
           | None, _ -> res
           (* TODO: treat function pointers.
              The issue is that their origin is more
              difficult to trace than plain variables (e.g. we'd have
              to take into account possible assignments, or update
              accordingly the signature of current function in case
              of a formal.
           *)
          )
      in
      let (argTypes, ghostArgTypes) =
        List.partition (fun d -> not (isGhostFormalVarDecl d) || ghost) argTypesList
      in
      let args = if ghost then args @ ghost_args else args in

      (* Again, we process arguments in REVERSE order. *)
      let (sghost, ghosts') = loopArgs ~are_ghost:true (ghostArgTypes, ghost_args) in
      let (sargs, args') = loopArgs (argTypes, args) in

      let sargs = sghost @@@ (sargs, false) in

      let (sargs, args') = (sargs, args' @ ghosts') in
      (* Setup some pointer to the elements of the call. We may change
       * these below *)
      let s0 = unspecified_chunk empty in
      (* there is a sequence point between evaluations of args
         and the call itself, but we have to check that args wo side-effects
         (thus not appearing anywhere in sargs) are not modified by others...
         The call must thus be in the unspecified chunk
      *)
      let sargs = if isEmpty sargs then empty else sargs in
      let prechunk = ref ((s0 @@@ (sf, ghost)) @@@ (sargs, ghost)) in
      (* Do we actually have a call, or an expression? *)
      let piscall: bool ref = ref true in

      let pf: exp ref = ref f'' in (* function to call *)
      let pargs: exp list ref = ref args' in (* arguments *)
      let pis__builtin_va_arg: bool ref = ref false in
      let pwhat: expAction ref = ref what in (* what to do with result *)
      let locals = ref [] in

      (* If we do not have a call, this is the result *)
      let pres: exp ref = ref (zero ~loc:e.expr_loc) in

      let prestype: typ ref = ref intType in

      let rec dropCasts e = match e.enode with
        | CastE (_, e) -> dropCasts e
        | _ -> e
      in
      (* Get the name of the last formal *)
      let getNameLastNonGhostFormal () : string =
        match !currentFunctionFDEC.svar.vtype.tnode with
        | TFun(_, Some args, true) -> begin
            let args, _ = Cil.argsToPairOfLists (Some args) in
            match List.rev args with
            | (last_par_name, _, _) :: _ -> last_par_name
            | _ -> ""
          end
        | _ -> ""
      in
      (* Try to intercept some builtins *)
      (match (!pf).enode with
       | Lval(Var fv, NoOffset) -> begin
           match fv.vname with
           | "__builtin_va_arg" ->
             begin
               match !pargs with
               | marker :: ({enode = SizeOf resTyp} as size) :: _ -> begin
                   (* Make a variable of the desired type *)
                   let is_real, destlv, r, destlvtyp =
                     match !pwhat with
                     | ASet (is_real,lv, r, lvt) -> is_real, lv, r, lvt
                     | _ ->
                       let v = newTempVar ~ghost loc "vararg" true resTyp in
                       locals := v::!locals;
                       false, var v, [], resTyp
                   in
                   pwhat := (ASet (is_real, destlv, r, destlvtyp));
                   pargs := [marker; size;
                             new_exp ~loc
                               (CastE(voidPtrType,
                                      new_exp ~loc (AddrOf destlv)))];
                   pis__builtin_va_arg := true;
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" fv.vname;
             end

           | "__builtin_va_start" ->
             let variad = match (!currentFunctionFDEC).svar.vtype.tnode with
               | TFun (_, _, t) -> t
               | _ -> assert false
             in
             let name =
               (!currentFunctionFDEC).svar.vname
             in
             begin
               match !pargs with
               | marker :: last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastNonGhostFormal ()
                     | _ -> false
                   in
                   if not isOk && variad then
                     Kernel.error ~current:true
                       "The last argument in call to __builtin_va_start \
                        should be the last formal argument of %s" name;

                   if not isOk && not variad then
                     Kernel.error ~current:true
                       "Invalid call to __builtin_va_start \
                        in non-variadic function %s"
                       name;

                   (* Check that "lastv" is indeed the last variable in the
                    * prototype and then drop it *)
                   pargs := [ marker ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" name;

                 (* We have to turn uses of __builtin_varargs_start into uses
                  * of __builtin_stdarg_start (because we have dropped the
                  * __builtin_va_alist argument from this function) *)
             end

           | "__builtin_stdarg_start" ->
             let name =
               (!currentFunctionFDEC).svar.vname
             in
             begin
               match !pargs with
               | marker :: last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastNonGhostFormal ()
                     | _ -> false
                   in
                   if not isOk then
                     Kernel.warning ~current:true
                       "The last argument in call to __builtin_stdarg_start \
                        should be the last formal argument of %s" name;

                   (* Check that "lastv" is indeed the last variable in the
                    * prototype and then drop it *)
                   pargs := [ marker ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" name;

                 (* We have to turn uses of __builtin_varargs_start into uses
                  * of __builtin_stdarg_start (because we have dropped the
                  * __builtin_va_alist argument from this function) *)
             end

           | "__builtin_varargs_start" ->
             begin
               (* Lookup the prototype for the replacement *)
               let v, _  =
                 (* builtin is not ghost *)
                 try lookupGlobalVar false "__builtin_stdarg_start"
                 with Not_found ->
                   abort_context
                     "Cannot find __builtin_stdarg_start to replace %s"
                     fv.vname
               in
               pf := new_exp ~loc (Lval (var v))
             end
           |  "__builtin_next_arg" ->
             begin
               match !pargs with
               | last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastNonGhostFormal ()
                     | _ -> false
                   in
                   if not isOk then
                     Kernel.warning ~current:true
                       "The argument in call to %s should be \
                        the last formal argument\n" fv.vname;

                   pargs := [ ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %s\n" fv.vname;
             end
           | "__builtin_va_arg_pack" ->
             begin
               (match !pargs with
                | [ ] -> begin
                    piscall := false;
                    pres := new_exp ~loc:e.expr_loc (SizeOfE !pf);
                    prestype := (Machine.sizeof_type ())
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Invalid call to builtin_va_arg_pack");
             end
           | "__builtin_constant_p" ->
             begin
               (* Before emptying the chunk, we remove the corresponding
                  generated labels from the tables. Otherwise, they will
                  be dangling when we iterate over the tables to fix
                  forward gotos, leading to errors. *)
               let remove_label s =
                 let vis = object
                   inherit Cil.nopCilVisitor
                   method! vstmt { labels } =
                     List.iter
                       (function
                         | Label (l, _, _) ->
                           H.remove labelStmt l;
                           H.remove backPatchGotos l
                         | _ -> ())
                       labels;
                     DoChildren
                 end
                 in
                 ignore (Cil.visitCilStmt vis s)
               in
               List.iter
                 (fun (stmt, _, _, _, _) ->
                    remove_label stmt
                 ) !prechunk.stmts;
               clean_up_chunk_locals !prechunk;
               (* Drop the side-effects *)
               prechunk := empty;
               (* Constant-fold the argument and see if it is a constant *)
               (match !pargs with
                | [ arg ] -> begin
                    match (constFold true arg).enode with
                    | Const _ -> piscall := false;
                      pres := integer ~loc:e.expr_loc 1 ;
                      prestype := intType

                    | _ -> piscall := false;
                      pres := integer ~loc:e.expr_loc 0;
                      prestype := intType
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Invalid call to builtin_constant_p")
             end
           | "__builtin_offsetof" ->
             begin
               match !pargs with
               | [{ enode = CastE (_, {enode = AddrOf (host, offset)}) } as e] ->
                 begin
                   piscall := false;
                   prestype := Machine.sizeof_type ();
                   let typ = Cil.typeOfLhost host in
                   try
                     let start, _width = Cil.bitsOffset typ offset in
                     if start mod 8 <> 0 then
                       Kernel.error ~current:true "Using offset of bitfield";
                     let kind = Machine.sizeof_kind () in
                     pres := Cil.kinteger ~loc:e.eloc kind (start / 8);
                   with SizeOfError (s, _) ->
                     pres := e;
                     Kernel.error ~once:true ~current:true
                       "Unable to compute offset %a in type %a: %s"
                       Cil_datatype.Offset.pretty offset
                       Cil_datatype.Typ.pretty typ
                       s;
                 end
               | _ ->
                 abort_context "Invalid call to builtin_offsetof"
             end
           | "__builtin_types_compatible_p" ->
             begin
               (* Constant-fold the argument and see if it is a constant *)
               (match !pargs with
                | [ {enode = SizeOf t1}; {enode = SizeOf t2}] -> begin
                    (* Drop the side-effects *)
                    prechunk := empty;
                    piscall := false;
                    let compatible =
                      try ignore(combineTypes CombineOther t1 t2); true
                      with Cannot_combine _ -> false
                    in if compatible then
                      pres := integer ~loc 1
                    else
                      pres := integer ~loc 0;
                    prestype := intType
                  end
                | _ ->
                  Kernel.warning
                    ~once:true
                    ~current:true
                    "Invalid call to builtin_types_compatible_p");
             end
           | "__builtin_expect" ->
             begin
               match !pargs with
               | [ arg;_ ] ->
                 (* Keep all side-effects, including those stemming
                    from the second argument. This is quite strange but
                    compliant with GCC's behavior. *)
                 piscall := false;
                 pres := arg
               | _ ->
                 Kernel.warning ~once:true ~current:true
                   "Invalid call to builtin_expect"
             end

           | "__fc_infinity" -> begin
               piscall := false;
               let cst = CReal (infinity, FFloat, Some "INFINITY") in
               pres := Cil.new_exp ~loc (Const cst);
               prestype := floatType;
             end
           | "__fc_nan" -> begin
               piscall := false;
               let cst = CReal (nan, FFloat, Some "NAN") in
               pres := Cil.new_exp ~loc (Const cst);
               prestype := floatType;
             end

           (* TODO: Only keep the side effects of the 1st or 2nd argument
              | "__builtin_choose_expr" ->
              begin match !pargs with
              | [ arg; e1; e2 ] ->
                begin
                  let constfolded = constFold true arg in
                  match constfolded.enode with
                  | Const _ ->
                    piscall := false;
                    if isZero constfolded then begin
                    (* Keep only 3rd arg side effects *)
                      (*TODO: prechunk := sf @@@ (List.nth sargsl 2);*)
                      pres := e2;
                      prestype := typeOf e2
                    end else begin
                    (* Keep only 2nd arg side effects *)
                      (*TODO prechunk := sf @@@ (List.nth sargsl 1);*)
                      pres := e1;
                      prestype := typeOf e1
                    end
                  | _ -> Kernel.warning ~once:true ~current:true
                    "builtin_choose_expr expects a constant first argument"
                end
              | _ ->
              Kernel.warning ~once:true ~current:true
                "Invalid call to builtin_choose_expr: 3 arguments are \
                 expected but %d are provided."
                (List.length !pargs)
              end*)
           | _ ->
             if asconst = CConst then
               (* last special case: we cannot allow a function call
                  at this point.*)
               begin
                 piscall := false;
                 abort_context
                   "Call to %a in constant." Cil_printer.pp_varinfo fv;
               end
         end
       | _ -> ());

      (* Now we must finish the call *)
      if !piscall then begin
        let addCall ?(is_real_var=true) calldest res t =
          let my_write =
            match calldest with
            | None -> []
            | Some c when is_real_var -> [c]
            | Some _ -> []
          in
          prechunk :=
            (empty @@@ (!prechunk, ghost)) +++
            (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
               (Call(calldest,!pf,!pargs,loc)),
             [],my_write, rf);
          pres := res;
          prestype := t
        in
        match !pwhat with
        | ADrop -> addCall None (zero ~loc:e.expr_loc) intType
        | AType -> prestype := resType'
        | ASet(is_real_var, lv, _, vtype) when !pis__builtin_va_arg ->
          (* Make an exception here for __builtin_va_arg *)
          addCall
            ~is_real_var
            None
            (new_exp ~loc:e.expr_loc (Lval(lv)))
            vtype

        | ASet(is_real_var, lv, _, vtype)
          when (allow_return_collapse ~tf:resType' ~tlv:vtype)
          ->
          (* We can assign the result directly to lv *)
          addCall
            ~is_real_var
            (Some lv)
            (new_exp ~loc:e.expr_loc (Lval(lv)))
            vtype

        | _ -> begin
            let restype'' = match !pwhat with
              | AExp (Some t)
                when allow_return_collapse ~tf:resType' ~tlv:t -> t
              | _ -> resType'
            in
            let descr =
              Format.asprintf "%a(%a)"
                Cil_descriptive_printer.pp_exp !pf
                (Pretty_utils.pp_list ~sep:", "
                   Cil_descriptive_printer.pp_exp)
                !pargs
            in
            let tmp = newTempVar ~ghost loc descr false restype'' in
            locals:=tmp::!locals;
            (* Remember that this variable has been created for this
             * specific call. We will use this in collapseCallCast. *)
            IH.add callTempVars tmp.vid ();
            addCall
              ~is_real_var:false
              (Some (var tmp))
              (new_exp ~loc:e.expr_loc (Lval(var tmp)))
              restype'';
          end
      end;
      List.iter
        (fun v -> prechunk:= local_var_chunk !prechunk v) !locals;
      finishExp [] !prechunk !pres !prestype

    | Cabs.COMMA el ->
      if asconst = CConst then Kernel.warning ~current:true "COMMA in constant";
      (* We must ignore AExpLeaveArrayFun (a.k.a. 'do not decay pointers')
         if the expression at hand is a sequence with strictly more than
         one expression, because the exception for sizeof and typeof only
         apply when the expression is directly the argument of the operators.
         See C99 and C11 6.3.2.1§3.)
      *)
      let what =
        if what <> AExpLeaveArrayFun || List.length el = 1
        then what
        else (AExp None)
      in
      let rec loop sofar = function
        | [e] ->
          let (r, se, e', t') =
            doExp (no_paren_local_env local_env) CNoConst e what
          in
          (* Pass on the action *)
          (r, sofar @@@ (se, ghost), e', t')
        | e :: rest ->
          let (_, se, _, _) =
            doExp (no_paren_local_env local_env) CNoConst e ADrop
          in
          loop (sofar @@@ (se, ghost)) rest
        | [] -> Kernel.fatal ~current:true "empty COMMA expression"
      in
      loop empty el

    | Cabs.QUESTION (e1, e2, e3) -> begin
        (* Compile the conditional expression *)
        let ghost = local_env.is_ghost in
        let ce1 = doCondExp (no_paren_local_env local_env) asconst e1 in
        let what' = match what with
          | ADrop -> ADrop
          | _ -> AExp None
        in
        let is_true_cond = evaluate_cond_exp ce1 in
        (* Now we must find the type of both branches, in order to compute
         * the type of the result *)
        let r2, se2, e2'o (* is an option. None means use e1 *), t2 =
          (* A GCC extension. [x ? : y;] is equivalent to [x ? x : y;] *)
          match e2.expr_node with
          | Cabs.NOTHING -> begin (* The same as the type of e1 *)
              match ce1 with
              | CEExp (_, e1') ->
                [], unspecified_chunk empty, None, typeOf e1'
              (* Do not promote to bool *)
              | _ -> [], unspecified_chunk empty, None, intType
            end
          | _ ->
            (* if e1 is false, e2 is only interesting for its type, but
               we won't evaluate it. Hence, it can contain
               non-const constructions *)
            let asconst =
              if is_true_cond = `CFalse then CMayConst else asconst
            in
            let r2, se2, e2', t2 =
              doExp (no_paren_local_env local_env) asconst e2 what'
            in
            r2, se2, Some e2', t2
        in
        (* Do e3 for real. See above for the value of asconst *)
        let asconst' = if is_true_cond = `CTrue then CMayConst else asconst in
        let r3, se3, e3', t3 =
          doExp (no_paren_local_env local_env) asconst' e3 what'
        in
        let tresult = conditionalConversion t2 t3 in
        if asconst <> CNoConst && is_true_cond = `CTrue then begin
          clean_up_chunk_locals se2;
          clean_up_chunk_locals se3;
          let loc = e2.expr_loc in
          let e2' = match e2'o with None -> Cil.one ~loc | Some e -> e in
          let _,e2' = castTo t2 tresult e2' in
          finishExp [] empty e2' tresult;
        end else if asconst <> CNoConst && is_true_cond = `CFalse then begin
          clean_up_chunk_locals se2;
          clean_up_chunk_locals se3;
          let _,e3' = castTo t3 tresult e3' in
          finishExp [] empty e3' tresult
        end else begin
          if not (isEmpty se2) then
            ConditionalSideEffectHook.apply (e,e2);
          if not (isEmpty se3) then
            ConditionalSideEffectHook.apply (e,e3);
          match ce1 with
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CFalse && canDrop se2 ->
            clean_up_chunk_locals se2;
            finishExp r3 ((empty @@@ (se1, ghost)) @@@ (se3, ghost))
              (snd (castTo t3 tresult e3')) tresult
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CTrue && canDrop se3 ->
            begin
              clean_up_chunk_locals se3;
              match e2'o with
              | None -> (* use e1' *)
                finishExp r2
                  ((empty @@@ (se1, ghost)) @@@ (se2, ghost))
                  (snd (castTo t2 tresult e1')) tresult
              | Some e2' ->
                finishExp r2
                  ((empty @@@ (se1, ghost)) @@@ (se2, ghost))
                  (snd (castTo t2 tresult e2')) tresult
            end
          | _ when what = ADrop ->
            (* We are not interested by the result, but might want to
               evaluate e2 and e3 if they are dangerous expressions. *)
            (* dummy result, that will be ultimately be dropped *)
            let res = Cil.zero ~loc in
            (match e2'o with
             | None when is_dangerous e3' || not (isEmpty se3) ->
               let descr =
                 Format.asprintf "%a" Cprint.print_expression e1
               in
               let tmp = newTempVar ~ghost loc descr true tresult in
               let tmp_var = var tmp in
               let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
               let (r1, se1, _, _) =
                 doExp
                   (no_paren_local_env local_env) asconst e1
                   (ASet(false, tmp_var, [], tresult))
               in
               let se1 = local_var_chunk se1 tmp in
               let dangerous =
                 if is_dangerous e3' then
                   keepPureExpr ~ghost e3' loc
                 else skipChunk
               in
               finishExp (r1@r3)
                 ((empty @@@ (se1, ghost)) @@@
                  (ifChunk ~ghost tmp_lval loc skipChunk
                     (se3 @@@ (dangerous, ghost)), ghost))
                 res
                 tresult
             | None ->
               (* we can drop e3, just keep e1 in case it is dangerous *)
               let (r1,se1,e1,_) =
                 doExp (no_paren_local_env local_env) asconst e1 ADrop
               in
               let dangerous =
                 if is_dangerous e1 then
                   keepPureExpr ~ghost e1 loc
                 else skipChunk
               in
               finishExp
                 (r1@r3) (se1 @@@ (dangerous, ghost)) res tresult
             | Some e2'
               when is_dangerous e2' || is_dangerous e3'
                    || not (isEmpty se2) || not (isEmpty se3) ->
               (* we have to keep e1 in order to know which
                  dangerous expression is to be evaluated *)
               let se2 =
                 if is_dangerous e2' then
                   se2 @@@
                   (keepPureExpr ~ghost e2' loc, ghost)
                 else se2
               in
               let se3 =
                 if is_dangerous e3' then
                   se3 @@@ (keepPureExpr ~ghost e3' loc, ghost)
                 else se3
               in
               let cond = compileCondExp ~ghost ce1 se2 se3 in
               finishExp (r2@r3) cond res tresult
             | Some _ ->
               (* we just keep e1 in case it is dangerous. everything
                  else can be dropped *)
               let (r1,se1,e1,_) =
                 doExp (no_paren_local_env local_env) asconst e1 ADrop
               in
               let dangerous =
                 if is_dangerous e1 then
                   keepPureExpr ~ghost e1 loc
                 else skipChunk
               in
               finishExp
                 (r1@r2@r3) (se1 @@@ (dangerous, ghost)) res tresult)
          | _ -> (* Use a conditional *) begin
              match e2'o with
              | None -> (* has form "e1 ? : e3"  *)
                let descr =
                  Format.asprintf "%a" Cprint.print_expression e1
                in
                let tmp = newTempVar ~ghost loc descr true tresult in
                let tmp_var = var tmp in
                let tmp_lval = new_exp ~loc:e.expr_loc (Lval (tmp_var)) in
                let (r1,se1, _, _) =
                  doExp
                    (no_paren_local_env local_env)
                    asconst e1 (ASet(false, tmp_var, [], tresult))
                in
                let se1 = local_var_chunk se1 tmp in
                let newWhat = ASet(false,tmp_var, [], tresult) in
                let r3,se3,_,_ = finishExp ~newWhat r3 se3 e3' t3 in
                finishExp
                  (r1@r3)
                  ((empty @@@ (se1, ghost)) @@@
                   (ifChunk ~ghost tmp_lval loc skipChunk se3, ghost))
                  tmp_lval
                  tresult
              | Some e2' ->
                let is_real, lv, r, lvt, scope_chunk =
                  match what with
                  | ASet (is_real, lv, r, lvt) ->
                    is_real, lv, r, lvt, empty
                  | _ ->
                    let descr =
                      Format.asprintf "%a?%a:%a"
                        Cprint.print_expression e1
                        Cil_descriptive_printer.pp_exp e2'
                        Cil_descriptive_printer.pp_exp e3'
                    in
                    let tmp = newTempVar ~ghost loc descr true tresult in
                    false, var tmp, [], tresult,
                    local_var_chunk empty tmp
                in
                (* Now do e2 and e3 for real *)
                let (r2,se2, _, _) =
                  finishExp ~newWhat:(ASet(is_real,lv,r,lvt))
                    r2 se2 e2' t2
                in
                let (r3, se3, _, _) =
                  finishExp ~newWhat:(ASet(is_real,lv, r, lvt))
                    r3 se3 e3' t3
                in
                let cond = compileCondExp ~ghost ce1 se2 se3 in
                finishExp
                  (r2@r3)
                  (scope_chunk @@@ (cond, ghost))
                  (new_exp ~loc (Lval lv)) tresult
            end
        end
      end

    | Cabs.GNU_BODY _ when !currentFunctionFDEC == dummy_function ->
      abort_context
        "statement expression forbidden outside function definition"

    | Cabs.GNU_BODY b -> begin
        (* Find the last Cabs.COMPUTATION and remember it. This one is invoked
         * on the reversed list of statements. *)
        let findLastComputation = function
            s :: _  ->
            let rec findLast st = match st.stmt_node with
              | CASE (_, s, _) -> findLast s
              | CASERANGE (_, _, s, _) -> findLast s
              | LABEL (_, s, _) -> findLast s
              | Cabs.COMPUTATION _ ->
                begin
                  match local_env.is_ghost,st.stmt_ghost with
                  | true,true | false, false -> st
                  | true, false -> assert false
                  | false, true -> raise Not_found
                end
              | _ -> raise Not_found
            in
            findLast s
          | [] -> raise Not_found
        in
        (* Save the previous data *)
        let old_gnu = ! gnu_body_result in
        let lastComp, isvoidbody =
          match what with
          | ADrop -> (* We are dropping the result *)
            {stmt_ghost = local_env.is_ghost; stmt_node = Cabs.NOP (None, loc)}, true
          | _ ->
            try findLastComputation (List.rev b.Cabs.bstmts), false
            with Not_found ->
              abort_context "void value not ignored as it ought to be"
              (*                Cabs.NOP cabslu, true *)
        in
        let loc = Cabshelper.get_statementloc lastComp in
        (* Prepare some data to be filled by doExp ghost *)
        let data : (exp * typ) option ref = ref None in
        gnu_body_result := (lastComp, data);

        let se = doBodyScope local_env b in

        (*Kernel.debug "Body inside expression: %a@." d_chunk se;*)

        gnu_body_result := old_gnu;
        match !data with
        | None when isvoidbody ->
          finishExp [] se (zero ~loc:e.expr_loc) voidType
        | None ->
          Kernel.fatal ~current:true
            "statement expression without COMPUTATION, which should be caught by findLastComputation"
        | Some (e, t) ->
          let se, e =
            match se.stmts with
            | [ { skind = Block b},_, _, _, _ ] ->
              let vi = newTempVar ~ghost loc "GNU.body" true t in
              b.bstmts <-
                b.bstmts @
                [Cil.mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                   (Set (Cil.var vi, e,loc))];
              (local_var_chunk se vi,Cil.new_exp ~loc (Lval (Cil.var vi)))
            | _ -> se,e
          in
          finishExp [] se e t
      end

    | Cabs.LABELADDR l -> begin (* GCC's taking the address of a label *)
        let l = lookupLabel ghost l in (* To support locally declared labels *)
        let addrval =
          try H.find gotoTargetHash l
          with Not_found -> begin
              let res = !gotoTargetNextAddr in
              incr gotoTargetNextAddr;
              H.add gotoTargetHash l res;
              res
            end
        in
        finishExp [] (unspecified_chunk empty)
          (mkCast ~newt:voidPtrType (integer ~loc addrval)) voidPtrType
      end

    | Cabs.GENERIC (ce, assocs) ->
      let (_, _, control_exp, control_t) = doExp local_env asconst ce AType in
      match Cil.lvalue_conversion control_t with
      | Error msg -> abort_context "%s" msg
      | Ok control_t ->
        let has_default, assocs =
          List.fold_left (fun (has_default, acc) (type_name, expr) ->
              let loc = expr.expr_loc in
              match type_name with
              | None -> (* default *)
                if has_default then
                  abort_context
                    "multiple default clauses in _Generic selection";
                true, ((None, expr) :: acc)
              | Some (spec, dt) ->
                let t = doOnlyType loc ghost spec dt in
                if not (Cil.isCompleteType t) then
                  abort_context
                    "generic association with incomplete type '%a'"
                    Cil_datatype.Typ.pretty t
                else if (Cil.isFunctionType t) then
                  abort_context
                    "generic association with function type '%a'"
                    Cil_datatype.Typ.pretty t
                else if (Cil.is_variably_modified_type t) then
                  abort_context
                    "generic association with variably modified type '%a'"
                    Cil_datatype.Typ.pretty t
                else begin
                  (* Check if current type is compatible with one of the
                     previous associations. Note: this is quadratic in terms of
                     list size.
                  *)
                  List.iter (fun (tn, _) ->
                      match tn with
                      | None -> ()
                      | Some t' ->
                        if areCompatibleTypes ~strictReturnTypes:true t t' then
                          abort_context
                            "multiple compatible types in _Generic selection:@ \
                             '%a' and '%a'"
                            Cil_printer.pp_typ t'
                            Cil_printer.pp_typ t
                    ) acc;
                  has_default, (Some t, expr) :: acc
                end
            ) (false, []) assocs
        in
        let candidates = (* note: assocs only includes non-default candidates *)
          List.filter (fun (type_name, _) ->
              Option.fold
                ~none:false
                ~some:(areCompatibleTypes ~strictReturnTypes:true control_t)
                type_name
            ) assocs
        in
        if List.length candidates > 1 then
          abort_context
            "controlling expression compatible with more than one association \
             type in _Generic selection:@ \
             controlling expression: '%a' (type: %a);@ \
             compatible types: %a"
            Cil_printer.pp_exp control_exp
            Cil_printer.pp_typ control_t
            (Pretty_utils.pp_list ~sep:", " Cil_printer.pp_typ)
            (List.map (fun (tn, _) -> Option.get tn) candidates)
        else if List.length candidates == 1 then
          doExp local_env asconst (snd (List.hd candidates)) what
        else if not has_default then
          let types =
            List.map (fun (type_name, _) -> Option.get type_name) assocs
          in
          abort_context
            "no compatible types and no default type in _Generic selection:@ \
             controlling expression: '%a' (type: %a);@ \
             candidate types: %a"
            Cil_printer.pp_exp control_exp
            Cil_printer.pp_typ control_t
            (Pretty_utils.pp_list ~sep:", " Cil_printer.pp_typ) types;
        else
          let default_type =
            (* This list is guaranteed non-empty, since has_default is 'true' *)
            snd List.(hd (filter (fun (typ_name, _) -> typ_name = None) assocs))
          in
          doExp local_env asconst default_type what
  in
  (*let (_a,b,_c,_d) = result in
    Format.eprintf "doExp ~const:%b ~e:" asconst ;
    Cprint.print_expression e;
    Format.eprintf "@.";
    Format.eprintf "Got: chunk:'%a'@." d_chunk b;*)
  result

and normalize_unop unop action asconst local_env e what =
  match e.expr_node with
  | Cabs.COMMA el -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      { e with
        expr_node =
          Cabs.COMMA
            (replaceLastInList el
               (fun e -> { e with expr_node = Cabs.UNARY(unop, e)})) }
      what
  | Cabs.QUESTION (e1, e2, e3) -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      { e with
        expr_node =
          Cabs.QUESTION
            (e1,
             { e2 with expr_node = Cabs.UNARY(unop, e2)},
             { e3 with expr_node = Cabs.UNARY(unop, e3)})}
      what
  | Cabs.PAREN e1 ->
    doExp (inner_paren local_env) asconst
      { e with expr_node = Cabs.UNARY(unop, e1)} what
  | _ ->
    action
      { local_env with
        is_paren = local_env.inner_paren; inner_paren = false }
      e
      what

and normalize_binop binop action local_env asconst le re what =
  match le.expr_node with
  | Cabs.COMMA el -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (Cabs.COMMA
            (replaceLastInList el
               (fun e -> cabs_exp e.expr_loc (Cabs.BINARY(binop, e, re))))))
      what
  | Cabs.QUESTION (e1, e2q, e3q) -> (* GCC extension *)
    (*TODO: prevent duplication of e2: this is incorrect
      if it contains labels *)
    (* let r2,se2,e2,t2 = doExp authorized_reads ghost asconst e2 in*)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (Cabs.QUESTION
            (e1,
             cabs_exp e2q.expr_loc (Cabs.BINARY(binop, e2q, re)),
             cabs_exp e3q.expr_loc (Cabs.BINARY(binop, e3q, re)))))
      what
  | Cabs.CAST (t, Cabs.SINGLE_INIT e) when binop = Cabs.ASSIGN -> (* GCC extension *)
    doExp (no_inner_paren local_env) asconst
      (cabs_exp le.expr_loc
         (Cabs.CAST
            (t,
             Cabs.SINGLE_INIT
               (cabs_exp e.expr_loc
                  (Cabs.BINARY
                     (binop, e,
                      (cabs_exp re.expr_loc
                         (Cabs.CAST (t, Cabs.SINGLE_INIT re)))))))))
      what
  | Cabs.PAREN e1 ->
    doExp (inner_paren local_env) asconst
      (cabs_exp le.expr_loc (Cabs.BINARY(binop,e1,re))) what
  | _ ->
    action
      { local_env with is_paren = local_env.inner_paren; inner_paren = false }
      asconst le what

(* bop is always the arithmetic version. Change it to the appropriate pointer
 * version if necessary *)
and doBinOp loc (bop: binop) (e1: exp) (t1: typ) (e2: exp) (t2: typ) =
  let doArithmetic () =
    if isArithmeticType t1 && isArithmeticType t2 then begin
      let tres = arithmeticConversion t1 t2 in
      (* Keep the operator since it is arithmetic *)
      tres,
      optConstFoldBinOp loc false bop
        (mkCastT ~oldt:t1 ~newt:tres e1)
        (mkCastT ~oldt:t2 ~newt:tres e2)
        tres
    end else
      abort_context
        "%a operator on non-arithmetic type(s) %a and %a"
        Cil_printer.pp_binop bop Cil_printer.pp_typ t1 Cil_printer.pp_typ t2
  in
  let doArithmeticComp () =
    let tres = arithmeticConversion t1 t2 in
    (* Keep the operator since it is arithmetic *)
    intType,
    optConstFoldBinOp loc false bop
      (mkCastT ~oldt:t1 ~newt:tres e1)
      (mkCastT ~oldt:t2 ~newt:tres e2)
      intType
  in
  let doIntegralArithmetic () =
    if isIntegralType t1 && isIntegralType t2 then begin
      let tres = unrollType (arithmeticConversion t1 t2) in
      match tres.tnode with
      | TInt _ ->
        tres,
        optConstFoldBinOp loc false bop
          (mkCastT ~oldt:t1 ~newt:tres e1)
          (mkCastT ~oldt:t2 ~newt:tres e2)
          tres
      | _ ->
        Kernel.fatal
          "conversion of integer types %a and %a returned a non-integer type %a"
          Cil_printer.pp_typ t1 Cil_printer.pp_typ t2 Cil_printer.pp_typ tres
    end else
      abort_context "%a operator on non-integer type(s) %a and %a"
        Cil_printer.pp_binop bop Cil_printer.pp_typ t1 Cil_printer.pp_typ t2
  in
  (* Invariant: t1 and t2 are pointers types *)
  let pointerComparison e1 t1 e2 t2 =
    if false then Kernel.debug "%a %a %a %a"
        Cil_printer.pp_exp e1 Cil_datatype.Typ.pretty t1
        Cil_printer.pp_exp e2 Cil_datatype.Typ.pretty t2;
    let t1p = Cil.(unrollType (typeOf_pointed t1)) in
    let t2p = Cil.(unrollType (typeOf_pointed t2)) in
    (* We are more lenient than the norm here (C99 6.5.8, 6.5.9), and cast
       arguments with incompatible types to a common type *)
    let e1', e2' =
      if not (areCompatibleTypes t1p t2p) then
        mkCastT ~oldt:t1 ~newt:voidPtrType e1,
        mkCastT ~oldt:t2 ~newt:voidPtrType e2
      else e1, e2
    in
    intType,
    optConstFoldBinOp loc false bop e1' e2' intType
  in
  let do_shift e1 t1 e2 t2 =
    match e1.enode with
    | StartOf lv ->
      { e1 with enode = AddrOf (addOffsetLval (Index (e2,NoOffset)) lv) }
    | _ ->
      optConstFoldBinOp loc false PlusPI e1
        (mkCastT ~oldt:t2 ~newt:(integralPromotion t2) e2) t1
  in
  match bop with
  | (Mult|Div) -> doArithmetic ()
  | (Mod|BAnd|BOr|BXor) -> doIntegralArithmetic ()
  | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result
                          * has the same type as the left hand side *)
    if Machine.msvcMode () then
      (* MSVC has a bug. We duplicate it here *)
      doIntegralArithmetic ()
    else
      let t1' = integralPromotion t1 in
      let t2' = integralPromotion t2 in
      t1',
      optConstFoldBinOp loc false bop
        (mkCastT ~oldt:t1 ~newt:t1' e1)
        (mkCastT ~oldt:t2 ~newt:t2' e2)
        t1'
  | (PlusA|MinusA)
    when isArithmeticType t1 && isArithmeticType t2 -> doArithmetic ()
  | (Eq|Ne|Lt|Le|Ge|Gt)
    when isArithmeticType t1 && isArithmeticType t2 ->
    doArithmeticComp ()
  | PlusA when isPointerType t1 && isIntegralType t2 ->
    t1, do_shift e1 t1 e2 t2
  | PlusA when isIntegralType t1 && isPointerType t2 ->
    t2, do_shift e2 t2 e1 t1
  | MinusA when isPointerType t1 && isIntegralType t2 ->
    t1,
    optConstFoldBinOp loc false MinusPI e1
      (mkCastT ~oldt:t2 ~newt:(integralPromotion t2) e2) t1
  | MinusA when isPointerType t1 && isPointerType t2 ->
    if areCompatibleTypes (* C99 6.5.6:3 *)
        (Cil.type_remove_qualifier_attributes_deep t1)
        (Cil.type_remove_qualifier_attributes_deep t2)
    then
      Machine.ptrdiff_type (),
      optConstFoldBinOp loc false MinusPP e1 e2 (Machine.ptrdiff_type ())
    else abort_context "incompatible types in pointer subtraction"

  (* Two special cases for comparisons with the NULL pointer. We are a bit
     more permissive. *)
  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isZero e2 ->
    pointerComparison e1 t1 (mkCast ~newt:t1 e2) t1
  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t2 && isZero e1 ->
    pointerComparison (mkCast ~newt:t2 e1) t2 e2 t2

  | (Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
    pointerComparison e1 t1 e2 t2

  | (Eq|Ne|Le|Lt|Ge|Gt) when (isPointerType t1 && isArithmeticType t2 ||
                              isArithmeticType t1 && isPointerType t2 ) ->
    abort_context
      "comparison between pointer and non-pointer: %a"
      Cil_printer.pp_exp (dummy_exp(BinOp(bop,e1,e2,intType)))

  | _ ->
    abort_context
      "cannot perform the following comparison %a"
      Cil_printer.pp_exp (dummy_exp(BinOp(bop,e1,e2,intType)))

(* Constant fold a conditional. This is because we want to avoid having
 * conditionals in the initializers. So, we try very hard to avoid creating
 * new statements.
*)
and doCondExp local_env asconst
    (* Try to evaluate the conditional expression
     * to TRUE or FALSE, because it occurs in a constant *)
    ?ctxt (* ctxt is used internally to determine if we should apply
             the conditional side effects hook (see above)
             and should not appear (i.e. be None) in toplevel calls. *)
    (e: Cabs.expression) : condExpRes =
  let ghost = local_env.is_ghost in
  let rec addChunkBeforeCE (c0: chunk) ce =
    let c0 = remove_effects c0 in
    match ce with
    | CEExp (c, e) -> CEExp ((empty @@@ (c0, ghost)) @@@ (c, ghost), e)
    | CEAnd (ce1, ce2) -> CEAnd (addChunkBeforeCE c0 ce1, ce2)
    | CEOr (ce1, ce2) -> CEOr (addChunkBeforeCE c0 ce1, ce2)
    | CENot ce1 -> CENot (addChunkBeforeCE c0 ce1)
  in
  let rec canDropCE = function
      CEExp (c, _e) -> canDrop c
    | CEAnd (ce1, ce2) | CEOr (ce1, ce2) -> canDropCE ce1 && canDropCE ce2
    | CENot (ce1) -> canDropCE ce1
  in
  let rec remove_effects_ce = function
    | CEExp(c,e) -> CEExp(remove_effects c,e)
    | CEAnd(ce1,ce2) -> CEAnd(remove_effects_ce ce1, remove_effects_ce ce2)
    | CEOr(ce1,ce2) -> CEOr(remove_effects_ce ce1, remove_effects_ce ce2)
    | CENot(ce) -> CENot(remove_effects_ce ce)
  in
  let loc = e.expr_loc in
  let result = match e.expr_node with
    | Cabs.BINARY (Cabs.AND, e1, e2) -> begin
        let ce1 = doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 in
        let ce2 = doCondExp (no_paren_local_env local_env) asconst ~ctxt:e e2 in
        let ce1 = remove_effects_ce ce1 in
        match ce1, ce2 with
        | CEExp (se1, ({enode = Const ci1})), _ ->
          (match isConstTrueFalse ci1 with
           | `CTrue -> addChunkBeforeCE se1 ce2
           | `CFalse ->
             (* se2 might contain labels so we cannot always drop it *)
             if canDropCE ce2 then begin
               clean_up_cond_locals ce2; ce1
             end else CEAnd (ce1, ce2))
        | CEExp(se1, e1'), CEExp (se2, e2') when
            Machine.use_logical_operators () && isEmpty se1 && isEmpty se2 ->
          CEExp (empty, new_exp ~loc (BinOp(LAnd, e1', e2', intType)))
        | _ -> CEAnd (ce1, ce2)
      end

    | Cabs.BINARY (Cabs.OR, e1, e2) -> begin
        let ce1 = doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 in
        let ce2 = doCondExp (no_paren_local_env local_env) asconst ~ctxt:e e2 in
        let ce1 = remove_effects_ce ce1 in
        match ce1, ce2 with
        | CEExp (se1, ({enode = Const ci1})), _ ->
          (match isConstTrueFalse ci1 with
           | `CFalse -> addChunkBeforeCE se1 ce2
           | `CTrue ->
             (* se2 might contain labels so we cannot drop it *)
             if canDropCE ce2 then begin
               clean_up_cond_locals ce2; ce1
             end else CEOr (ce1, ce2))
        | CEExp (se1, e1'), CEExp (se2, e2') when
            Machine.use_logical_operators () && isEmpty se1 && isEmpty se2 ->
          CEExp (empty, new_exp ~loc (BinOp(LOr, e1', e2', intType)))
        | _ -> CEOr (ce1, ce2)
      end

    | Cabs.UNARY(Cabs.NOT, e1) -> begin
        match doCondExp (no_paren_local_env local_env) asconst ?ctxt e1 with
        | CEExp (se1, e) when isEmpty se1 ->
          let t = typeOf e in
          if not ((isPointerType t) || (isArithmeticType t))then
            Kernel.error ~once:true ~current:true "Bad operand to !";
          CEExp (empty, new_exp ~loc (UnOp(LNot, e, intType)))
        | ce1 -> CENot ce1
      end

    | Cabs.PAREN e ->
      doCondExp (paren_local_env local_env) asconst ?ctxt e

    | _ ->
      let (r, se, e', t) =
        doExp local_env asconst e (AExp None)
      in
      (* No need to add reads here: we'll always have a sequence point,
         either because the expression is complete, or because of a logic
         operator. *)
      (match ctxt with
       | None -> ()
       | Some _ when isEmpty se -> ()
       | Some orig ->
         ConditionalSideEffectHook.apply (orig,e));
      ignore (checkBool t e');
      CEExp (add_reads ~ghost e.expr_loc r se,
             if asconst <> CNoConst || Machine.lower_constants () then
               constFold true e'
             else e')
  in
  result

and compileCondExp ?(hide=false) ~ghost ce st sf =
  match ce with
  | CEAnd (ce1, ce2) ->
    let loc = Current_loc.get () in
    let (duplicable, sf1, sf2) =
      (* If sf is small then will copy it *)
      try (true, sf, duplicateChunk sf)
      with Failure _ ->
        let lab = newLabelName ghost "_LAND" in
        (false, gotoChunk ~ghost lab loc, consLabel ~ghost lab sf loc false)
    in
    let st' = compileCondExp ~hide ~ghost ce2 st sf1 in
    if not duplicable && !doAlternateConditional then
      let st_fall_through = chunkFallsThrough st' in
      (* if st does not fall through, we do not need to add a goto
         after the else part. This prevents spurious falls-through warning
         afterwards. *)
      let sf' = duplicateChunk sf1 in
      let lab = newLabelName ghost "_LAND" in
      let gotostmt =
        if st_fall_through then gotoChunk ~ghost lab loc else skipChunk
      in
      let labstmt =
        if st_fall_through then
          consLabel ~ghost lab empty loc false
        else skipChunk
      in
      let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
      (compileCondExp ~hide ~ghost ce1 st' sf')
      @@@ gotostmt @@@ sf2 @@@ labstmt
    else
      let sf' = sf2 in
      compileCondExp ~hide ~ghost ce1 st' sf'

  | CEOr (ce1, ce2) ->
    let loc = Current_loc.get () in
    let (duplicable, st1, st2) =
      (* If st is small then will copy it *)
      try (true, st, duplicateChunk st)
      with Failure _ ->
        let lab = newLabelName ghost "_LOR" in
        (false, gotoChunk ~ghost lab loc, consLabel ~ghost lab st loc false)
    in
    if not duplicable && !doAlternateConditional then
      let st' = duplicateChunk st1 in
      let sf' = compileCondExp ~hide ~ghost ce2 st1 sf in
      let sf_fall_through = chunkFallsThrough sf' in
      let lab = newLabelName ghost "_LOR" in
      let gotostmt =
        if sf_fall_through then
          gotoChunk ~ghost lab loc
        else skipChunk
      in
      let labstmt =
        if sf_fall_through then
          consLabel ~ghost lab empty (Current_loc.get ()) false
        else skipChunk
      in
      let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
      (compileCondExp ~hide ~ghost ce1 st' sf')
      @@@ gotostmt @@@ st2 @@@ labstmt
    else
      let st' = st1 in
      let sf' = compileCondExp ~hide ~ghost ce2 st2 sf in
      compileCondExp ~hide ~ghost ce1 st' sf'

  | CENot ce1 -> compileCondExp ~hide ~ghost ce1 sf st

  | CEExp (se, e) -> begin
      match e.enode with
      | Const(CInt64(i,_,_))
        when (not (Integer.equal i Integer.zero)) && canDrop sf ->
        full_clean_up_chunk_locals sf;
        se @@@ (st, ghost)
      | Const(CInt64(z,_,_))
        when (Integer.equal z Integer.zero) && canDrop st ->
        full_clean_up_chunk_locals st;
        se @@@ (sf, ghost)
      | _ ->
        let se', e' =
          if hide then hide_chunk ~ghost ~loc:e.eloc [] se e (typeOf e)
          else se, e
        in
        (empty @@@ (se', ghost)) @@@ (ifChunk ~ghost e' e'.eloc st sf, ghost)
    end


(* A special case for conditionals *)
and doCondition ~is_loop local_env asconst
    (* If we are in constants, we do our best to eliminate the conditional *)
    (e: Cabs.expression)
    (st: chunk)
    (sf: chunk) : chunk =
  let ghost = local_env.is_ghost in
  contains_temp_subarray := false;
  if isEmpty st && isEmpty sf (*TODO: ignore attribute FRAMA_C_KEEP_BLOCK*) then
    begin
      let (_, se, e, _) = doExp local_env CNoConst e ADrop in
      let se' =
        if is_dangerous e then begin
          se @@@ (keepPureExpr ~ghost e e.eloc, ghost)
        end else begin
          if (isEmpty se) then begin
            let name = !currentFunctionFDEC.svar.vorig_name in
            IgnorePureExpHook.apply (name, e)
          end;
          se
        end
      in
      if !contains_temp_subarray then begin
        contains_temp_subarray := false;
        enclose_chunk ~ghost se'
      end
      else se'
    end
  else begin
    let ce = doCondExp (no_paren_local_env local_env) asconst e in
    if !contains_temp_subarray then begin
      contains_temp_subarray := false;
      if is_loop then
        (* Enclose everything (problematic chunk and break loop condition). *)
        enclose_chunk ~ghost (compileCondExp ~ghost ce st sf)
      else
        (* The condition chunk will be hidden, and a tmp variable will be used
           instead in the condition. *)
        compileCondExp ~hide:true ~ghost ce st sf
    end
    else compileCondExp ~ghost ce st sf
  end

and doPureExp local_env (e : Cabs.expression) : exp =
  let (_,se, e', _) = doExp local_env CConst e (AExp None) in
  if isNotEmpty se then
    Kernel.error
      ~once:true ~current:true
      "%a has side-effects. Side-effects are not yet supported here."
      Cprint.print_expression e;
  e'

and doFullExp local_env const e what =
  contains_temp_subarray := false;
  let (r, se, e, t) = doExp local_env const e what in
  let ghost = local_env.is_ghost in
  let loc = e.eloc in
  let se', e' =
    if !contains_temp_subarray then begin
      contains_temp_subarray := false;
      if what = ADrop
      then enclose_chunk ~ghost (add_reads ~ghost loc r se), e
      else hide_chunk ~ghost ~loc r se e t
    end
    else add_reads ~ghost loc r se, e
  in
  (* there is a sequence point after a full exp *)
  empty @@@ (se', ghost), e',t

and doInitializer loc local_env (vi: varinfo) (inite: Cabs.init_expression)
  (* Return the accumulated chunk, the initializer and the new type (might be
   * different for arrays), together with the lvals read during evaluation of
   * the initializer (for local intialization)
  *)
  : chunk * init * typ * Cil_datatype.Lval.Set.t =
  let open Current_loc.Operators in

  let checkArrayInit ty init =
    let init_ok =
      if Cil.isArrayType ty then
        match init with
        | COMPOUND_INIT _ -> true
        | SINGLE_INIT e ->
          (match stripParen e with
             { expr_node =
                 CONSTANT (CONST_STRING _ | CONST_WSTRING _)} -> true
           | _ -> false)
        | _ -> false
      else true
    in
    if not init_ok then
      Kernel.error ~current:true ~once:true
        "Array initializer must be an initializer list or string literal";
  in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "@\nStarting a new initializer for %s : %a@\n"
    vi.vname Cil_datatype.Typ.pretty vi.vtype;
  checkArrayInit vi.vtype inite;
  let acc, preinit, restl =
    let so = makeSubobj vi vi.vtype NoOffset in
    let asconst = if vi.vglob then CConst else CNoConst in
    let<> UpdatedCurrentLoc = loc in
    doInit local_env asconst NoInitPre so
      (unspecified_chunk empty) [ (Cabs.NEXT_INIT, inite) ]
  in
  if restl <> [] then
    Kernel.warning ~current:true "Ignoring some initializers";
  (* sm: we used to do array-size fixups here, but they only worked
   * for toplevel array types; now, collectInitializer does the job,
   * including for nested array types *)
  let typ' = vi.vtype in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "Collecting the initializer for %s@\n" vi.vname;
  let (init, typ'', reads) =
    collectInitializer Cil_datatype.Lval.Set.empty preinit typ'
      ~parenttype:typ'
  in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "Finished the initializer for %s@\n  init=%a@\n  typ=%a@\n  acc=%a@\n"
    vi.vname Cil_printer.pp_init init Cil_datatype.Typ.pretty typ' d_chunk acc;
  empty @@@ (acc, local_env.is_ghost), init, typ'', reads

(* Consume some initializers. This is used by both global and local variables
   initialization.
   – local_env is the current environment
   – asconst is used to indicate that expressions must be compile-time constant
     (i.e. we are in a global initializer)
   – preinit corresponds to the initializers seen previously (for globals)
   – so contains the information about the current subobject currently being
     initialized
   – acc is the chunk corresponding to initializations seen previously
     (for locals)
   – initl is the current list of initializers to be processed
   doInit returns a triple:
   – chunk performing initialization
   – preinit corresponding to the complete initialization
   – the list of unused initializers if any (should be empty most of the time)
*)
and doInit local_env asconst preinit so acc initl =
  let ghost = local_env.is_ghost in
  let whoami fmt = Cil_printer.pp_lval fmt (Var so.host, so.soOff) in
  let initl =
    match initl with
    | (initwhat, SINGLE_INIT e)::tl ->
      (initwhat, SINGLE_INIT (stripParen e)) ::tl
    | _ -> initl
  in
  let initl1 =
    match initl with
    | (Cabs.NEXT_INIT,
       Cabs.SINGLE_INIT
         ({ expr_node = Cabs.CAST ((s, dt), ie); expr_loc} as e)) :: rest ->
      let s', dt', ie' = preprocessCast expr_loc ghost s dt ie in
      (Cabs.NEXT_INIT,
       Cabs.SINGLE_INIT
         ({expr_node = Cabs.CAST ((s', dt'), ie'); expr_loc = e.expr_loc}))
      :: rest
    | _ -> initl
  in
  (* Sometimes we have a cast in front of a compound (in GCC). This
   * appears as a single initializer. Ignore the cast  *)
  let initl2 =
    match initl1 with
    | (what,
       Cabs.SINGLE_INIT
         ({expr_node = Cabs.CAST ((specs, dt), Cabs.COMPOUND_INIT ci);
           expr_loc})) :: rest ->
      let s', dt', _ie' =
        preprocessCast expr_loc ghost specs dt (Cabs.COMPOUND_INIT ci)
      in
      let typ = doOnlyType expr_loc ghost s' dt' in
      if Typ.equal
          (Cil.typeDeepDropAllAttributes typ)
          (Cil.typeDeepDropAllAttributes so.soTyp)
      then
        (* Drop the cast *)
        (what, Cabs.COMPOUND_INIT ci) :: rest
      else
        (* Keep the cast.  A new var will be created to hold
           the intermediate value.  *)
        initl1
    | _ -> initl1
  in
  let allinitl = initl2 in
  Kernel.debug ~dkey:Kernel.dkey_typing_init
    "doInit for %t %s (current %a). Looking at: %t" whoami
    (if so.eof then "(eof)" else "")
    Cil_printer.pp_lval (Var so.host, so.curOff)
    (fun fmt ->
       match allinitl with
       | [] -> Format.fprintf fmt "[]@."
       | (what, ie) :: _ ->
         Cprint.print_init_expression fmt (Cabs.COMPOUND_INIT [(what, ie)])
    );
  let soTyp' = unrollType so.soTyp in
  match soTyp'.tnode, allinitl with
  (* No more initializers return *)
  | _, [] -> acc, preinit, []
  (* No more subobjects to initialize *)
  | _, (Cabs.NEXT_INIT, _) :: _ when so.eof -> acc, preinit, allinitl
  (* If we are at an array of characters and the initializer is a
   * string literal (optionally enclosed in braces) then explode the
   * string into characters *)
  | TArray (bt, leno),
    (Cabs.NEXT_INIT,
     (Cabs.SINGLE_INIT({ expr_node = Cabs.CONSTANT (Cabs.CONST_STRING s)} as e)|
      Cabs.COMPOUND_INIT
        [(Cabs.NEXT_INIT,
          Cabs.SINGLE_INIT(
            { expr_node =
                Cabs.CONSTANT
                  (Cabs.CONST_STRING s)} as e))]))
    :: restil
    when (match unrollTypeNode bt with
        | TInt (IChar|IUChar|ISChar) -> true
        | TInt _ ->
          (*Base type is a scalar other than char. Maybe a wchar_t?*)
          abort_context
            "Using a string literal to initialize something other than \
             a character array"
        | _ ->  false (* OK, this is probably an array of strings. Handle *)
      )              (* it with the other arrays below.*)
    ->
    let charinits =
      let init c =
        Cabs.NEXT_INIT,
        Cabs.SINGLE_INIT
          { expr_node = Cabs.CONSTANT (Cabs.CONST_CHAR [c]);
            expr_loc = e.expr_loc }
      in
      let collector =
        (* ISO 6.7.8 para 14: final NUL added only if no size specified, or
         * if there is room for it; btw, we can't rely on zero-init of
         * globals, since this array might be a local variable *)
        if (Option.is_none leno ||
            ((String.length s) < (integerArrayLength leno)))
        then ref [init Int64.zero]
        else ref []
      in
      for pos = String.length s - 1 downto 0 do
        collector := init (Int64.of_int (Char.code (s.[pos]))) :: !collector
      done;
      !collector
    in
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let leno = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, leno, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      doInit local_env asconst preinit so' acc charinits in
    if initl' <> [] then
      Kernel.warning ~source:(fst e.expr_loc)
        "Too many initializers for character array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env asconst preinit' so acc' restil
  (* If we are at an array of WIDE characters and the initializer is a
   * WIDE string literal (optionally enclosed in braces) then explore
   * the WIDE string into characters *)
  (* [weimer] Wed Jan 30 15:38:05 PST 2002
   * Despite what the compiler says, this match case is used and it is
   * important. *)
  | TArray (bt, leno),
    (Cabs.NEXT_INIT,
     (Cabs.SINGLE_INIT({expr_node = Cabs.CONSTANT (Cabs.CONST_WSTRING s)} as e)|
      Cabs.COMPOUND_INIT
        [(Cabs.NEXT_INIT,
          Cabs.SINGLE_INIT(
            {expr_node =
               Cabs.CONSTANT
                 (Cabs.CONST_WSTRING s)} as e))]))
    :: restil
    when
      (let bt' = unrollType bt in
       match bt'.tnode with
       (* compare bt to wchar_t, ignoring signed vs. unsigned *)
       | TInt _ when (bitsSizeOf bt') =
                     (bitsSizeOf (Machine.wchar_type ())) ->
         true
       | TInt _ ->
         (*Base type is a scalar other than wchar_t.
           Maybe a char?*)
         abort_context
           "Using a wide string literal to initialize \
            something other than a wchar_t array"
       | _ -> false
       (* OK, this is probably an array of strings. Handle
          it with the other arrays below.*)
      )
    ->
    let maxWChar =  (*  (2**(bitsSizeOf !wcharType)) - 1  *)
      Int64.(sub (shift_left one (bitsSizeOf (Machine.wchar_type ()))) one)
    in
    let charinits =
      let init c =
        if Int64.compare c maxWChar > 0 then (* if c > maxWChar *)
          Kernel.error ~once:true ~current:true
            "cab2cil:doInit:character 0x%Lx too big." c;
        Cabs.NEXT_INIT,
        Cabs.SINGLE_INIT
          { expr_node = Cabs.CONSTANT (Cabs.CONST_INT (Int64.to_string c));
            expr_loc = e.expr_loc
          }
      in
      (List.map init s) @
      (
        (* ISO 6.7.8 para 14: final NUL added only if no size specified, or
         * if there is room for it; btw, we can't rely on zero-init of
         * globals, since this array might be a local variable *)
        if (Option.is_none leno
            || ((List.length s) < (integerArrayLength leno)))
        then [init Int64.zero]
        else [])
    in
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let leno = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, leno, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      doInit local_env asconst preinit so' acc charinits
    in
    if initl' <> [] then
      (* sm: see above regarding ISO 6.7.8 para 14, which is not implemented
       * for wchar_t because, as far as I can tell, we don't even put in
       * the automatic NUL (!) *)
      Kernel.warning ~current:true
        "Too many initializers for wchar_t array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env asconst preinit' so acc' restil
  (* If we are at an array and we see a single initializer then it must
   * be one for the first element *)
  | TArray(bt, leno), (Cabs.NEXT_INIT, Cabs.SINGLE_INIT _oneinit) :: _restil  ->
    (* Grab the length if there is one *)
    let leno = integerArrayLength leno in
    so.stack <- InArray(so.soOff, bt, leno, ref 0) :: so.stack;
    normalSubobj so;
    (* Start over with the fields *)
    doInit local_env asconst preinit so acc allinitl
  (* An incomplete structure with any initializer is an error. *)
  | TComp comp, _ :: restil when comp.cfields = None ->
    Kernel.error ~current:true ~once:true
      "variable `%s' has initializer but incomplete type" so.host.vname;
    doInit local_env asconst preinit so acc restil
  (* If we are at a composite and we see a single initializer of the same
   * type as the composite then grab it all. If the type is not the same
   * then we must go on and try to initialize the fields *)
  | TComp comp, (Cabs.NEXT_INIT, Cabs.SINGLE_INIT oneinit) :: restil ->
    let r,se, oneinit', t' =
      doExp (no_paren_local_env local_env) asconst oneinit (AExp None)
    in
    let r = Cil_datatype.Lval.Set.of_list r in
    if (match unrollTypeNode t' with
        | TComp comp' when comp'.ckey = comp.ckey -> true
        | _ -> false)
    then begin
      (* Initialize the whole struct *)
      let preinit = setOneInit preinit so.soOff (SinglePre (oneinit', r)) in
      (* Advance to the next subobject *)
      advanceSubobj so;
      let se = acc @@@ (se, ghost) in
      doInit local_env asconst preinit so se restil
    end else begin (* Try to initialize fields *)
      let toinit = fieldsToInit comp None in
      so.stack <- InComp(so.soOff, comp, toinit) :: so.stack;
      normalSubobj so;
      doInit local_env asconst preinit so acc allinitl
    end

  (* A scalar with a single initializer *)
  | _, (Cabs.NEXT_INIT, Cabs.SINGLE_INIT oneinit) :: restil ->
    let r, se, oneinit', t' =
      doExp (no_paren_local_env local_env) asconst oneinit (AExp(Some so.soTyp))
    in
    let r = Cil_datatype.Lval.Set.of_list r in
    Kernel.debug ~dkey:Kernel.dkey_typing_init "oneinit'=%a, t'=%a, so.soTyp=%a"
      Cil_printer.pp_exp oneinit' Cil_datatype.Typ.pretty t'
      Cil_datatype.Typ.pretty so.soTyp;
    let init_expr =
      if Machine.insert_implicit_casts () then snd (castTo t' so.soTyp oneinit')
      else oneinit'
    in
    let preinit' = setOneInit preinit so.soOff (SinglePre (init_expr,r)) in
    (* Move on *)
    advanceSubobj so;
    let se = acc @@@ (se,ghost) in
    doInit local_env asconst preinit' so se restil
  (* An array with a compound initializer. The initializer is for the
   * array elements *)
  | TArray (bt, leno), (Cabs.NEXT_INIT, Cabs.COMPOUND_INIT initl) :: restil ->
    (* Create a separate object for the array *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the array *)
    let len = integerArrayLength leno in
    so'.stack <- [InArray(so'.curOff, bt, len, ref 0)];
    normalSubobj so';
    let acc', preinit', initl' =
      match initl with
      | [] ->
        (* we must actually indicate that there is some initializer, albeit
           empty, to our parent. This is in particular important if said
           parent is an array of indeterminate size, as the number of
           initializers of its children matters. *)
        let preinit' = setOneInit preinit so'.curOff (empty_preinit()) in
        (* zero initialization will be done anyway,
           no need to change the chunk.*)
        acc, preinit', []
      | _ ->
        doInit local_env asconst preinit so' acc initl
    in
    if initl' <> [] then
      Kernel.warning ~current:true
        "Too many initializers for array %t" whoami;
    (* Advance past the array *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env asconst preinit' so acc' restil
  (* We have a designator that tells us to select the matching union field.
   * This is to support a GCC extension *)
  | TComp ci,
    [(Cabs.NEXT_INIT,
      Cabs.COMPOUND_INIT
        [(Cabs.INFIELD_INIT ("___matching_field", Cabs.NEXT_INIT),
          Cabs.SINGLE_INIT oneinit)])]
    when not ci.cstruct ->
    (* Do the expression to find its type *)
    let _, c, _, t' =
      doExp (no_paren_local_env local_env) asconst oneinit (AExp None)
    in
    clean_up_chunk_locals c;
    let t'noattr = Cil.typeDeepDropAllAttributes t' in
    let rec findField = function
      | [] ->
        abort_context "Cannot find matching union field in cast"
      | fi :: _rest when
          Typ.equal (Cil.typeDeepDropAllAttributes fi.ftype) t'noattr -> fi
      | _ :: rest -> findField rest
    in
    (* If this is a cast from union X to union X *)
    if Typ.equal t'noattr (Cil.typeDeepDropAllAttributes soTyp') then
      doInit local_env asconst preinit so acc
        [(Cabs.NEXT_INIT, Cabs.SINGLE_INIT oneinit)]
    else
      (* If this is a GNU extension with field-to-union cast find the field *)
      let fi = findField (Option.value ~default:[] ci.cfields) in
      (* Change the designator and redo *)
      doInit local_env asconst preinit so acc
        [Cabs.INFIELD_INIT (fi.fname, Cabs.NEXT_INIT), Cabs.SINGLE_INIT oneinit]

  (* A structure with a composite initializer. We initialize the fields*)
  | TComp comp, (Cabs.NEXT_INIT, Cabs.COMPOUND_INIT initl) :: restil ->
    (* Create a separate subobject iterator *)
    let so' = makeSubobj so.host so.soTyp so.soOff in
    (* Go inside the comp *)
    so'.stack <- [InComp(so'.curOff, comp, fieldsToInit comp None)];
    normalSubobj so';
    let acc', preinit', initl' =
      match initl with
      | [] -> (* empty initializer, a GNU extension to indicate
                 0-initialization. We must indicate to our parent that we are
                 here, though. *)
        let preinit' = setOneInit preinit so'.curOff (empty_preinit()) in
        acc, preinit', []
      | _ ->
        doInit local_env asconst preinit so' acc initl
    in
    if initl' <> [] then
      Kernel.warning ~current:true "Too many initializers for structure";
    (* Advance past the structure *)
    advanceSubobj so;
    (* Continue *)
    doInit local_env asconst preinit' so acc' restil
  (* A scalar with a initializer surrounded by a number of braces *)
  | _, (Cabs.NEXT_INIT, next) :: restil ->
    begin
      let rec find_one_init c =
        match c with
        | Cabs.COMPOUND_INIT [Cabs.NEXT_INIT,next] -> find_one_init next
        | Cabs.SINGLE_INIT oneinit -> oneinit
        | _ -> raise Not_found
      in
      try
        let oneinit = find_one_init next in
        let r,se, oneinit', t' =
          doExp (no_paren_local_env local_env)
            asconst oneinit (AExp(Some so.soTyp))
        in
        let r = Cil_datatype.Lval.Set.of_list r in
        let init_expr = mkCastT ~oldt:t' ~newt:so.soTyp oneinit' in
        let preinit' = setOneInit preinit so.soOff (SinglePre (init_expr, r)) in
        (* Move on *)
        advanceSubobj so;
        let se = acc @@@ (se, ghost) in
        doInit local_env asconst preinit' so se restil
      with Not_found ->
        abort_context
          "scalar value (of type %a) initialized by compound initializer"
          Cil_datatype.Typ.pretty soTyp'
    end
  (* We have a designator *)
  | _, (what, ie) :: restil when what != Cabs.NEXT_INIT ->
    (* Process a designator and position to the designated subobject *)
    let addressSubobj
        (so: subobj)
        (what: Cabs.initwhat)
        (acc: chunk) : chunk =
      (* Always start from the current element *)
      so.stack <- []; so.eof <- false;
      normalSubobj so;
      let rec address (what: Cabs.initwhat) (acc: chunk)  : chunk =
        match what with
        | Cabs.NEXT_INIT -> acc
        | Cabs.INFIELD_INIT (fn, whatnext) -> begin
            match unrollTypeNode so.soTyp with
            | TComp comp ->
              let toinit = fieldsToInit comp (Some fn) in
              so.stack <- InComp(so.soOff, comp, toinit) :: so.stack;
              normalSubobj so;
              address whatnext acc
            | _ ->
              abort_context "Field designator %s not in a struct " fn
          end

        | Cabs.ATINDEX_INIT(idx, whatnext) -> begin
            let open Current_loc.Operators in
            let<> UpdatedCurrentLoc = idx.expr_loc in
            match unrollTypeNode so.soTyp with
            | TArray (bt, leno) ->
              let ilen = integerArrayLength leno in
              let nextidx', doidx =
                let (r,doidx, idxe', _) =
                  doExp
                    (no_paren_local_env local_env)
                    CConst idx (AExp(Some intType))
                in

                let doidx = add_reads ~ghost idxe'.eloc r doidx in
                match constFoldToInt idxe', isNotEmpty doidx with
                | Some x, false ->
                  begin
                    match Integer.to_int_opt x with
                    | Some x' -> x', doidx
                    | None ->
                      abort_context
                        "INDEX initialization designator overflows"
                  end
                | _ ->
                  abort_context
                    "INDEX initialization designator is not a constant"
              in
              if nextidx' < 0 || nextidx' >= ilen then
                abort_context "INDEX designator is outside bounds";
              so.stack <-
                InArray(so.soOff, bt, ilen, ref nextidx') :: so.stack;
              normalSubobj so;
              address whatnext (acc @@@ (doidx, ghost))

            | _ -> abort_context "INDEX designator for a non-array"
          end

        | Cabs.ATINDEXRANGE_INIT _ ->
          Kernel.fatal ~current:true "addressSubobj: INDEXRANGE"
      in
      address what acc
    in
    (* First expand the INDEXRANGE by making copies *)
    let rec expandRange (top: Cabs.initwhat -> Cabs.initwhat) = function
      | Cabs.INFIELD_INIT (fn, whatnext) ->
        expandRange (fun what -> top (Cabs.INFIELD_INIT(fn, what))) whatnext
      | Cabs.ATINDEX_INIT (idx, whatnext) ->
        expandRange (fun what -> top (Cabs.ATINDEX_INIT(idx, what))) whatnext
      | Cabs.ATINDEXRANGE_INIT (idxs, idxe) ->
        let open Current_loc.Operators in
        let (rs, doidxs, idxs', _) =
          doExp (no_paren_local_env local_env) CConst idxs (AExp(Some intType))
        in
        let (re, doidxe, idxe', _) =
          doExp (no_paren_local_env local_env) CConst idxe (AExp(Some intType))
        in
        let doidxs = add_reads ~ghost idxs'.eloc rs doidxs in
        let doidxe = add_reads ~ghost idxe'.eloc re doidxe in
        let<> UpdatedCurrentLoc = (fst idxs'.eloc, snd idxe'.eloc) in
        if isNotEmpty doidxs || isNotEmpty doidxe then
          abort_context "Range designators are not constants";
        let first, last =
          match constFoldToInteger idxs', constFoldToInteger idxe' with
          | Some s, Some e -> s, e
          | _ ->
            abort_context
              "INDEX_RANGE initialization designator is not a valid constant"
        in
        if first < 0 || first > last then
          Kernel.error ~once:true ~current:true
            "start index larger than end index in range initializer";
        (* Arbitrary limit to avoid building an impractical AST. *)
        if last - first > 100_000 then
          abort_context "INDEX_RANGE too large";
        let rec loop (i: int) =
          if i > last then restil
          else
            (top (Cabs.ATINDEX_INIT(
                 { expr_node = Cabs.CONSTANT(Cabs.CONST_INT(string_of_int i));
                   expr_loc = fst idxs.expr_loc, snd idxe.expr_loc},
                 Cabs.NEXT_INIT)), ie)
            :: loop (i + 1)
        in
        doInit local_env asconst preinit so acc (loop first)
      | Cabs.NEXT_INIT -> (* We have not found any RANGE *)
        let acc' = addressSubobj so what acc in
        doInit local_env asconst preinit so acc'
          ((Cabs.NEXT_INIT, ie) :: restil)
    in
    expandRange (fun x -> x) what
  | _, (_what, _ie) :: _ ->
    abort_context "doInit: cases for t=%a" Cil_datatype.Typ.pretty soTyp'

(* Create and add to the file (if not already added) a global. Return the
 * varinfo *)
and createGlobal loc ghost logic_spec ((t,s,b,attr_list) : (typ * storage * bool * Cabs.attribute list))
    (((n,ndt,a,cloc), inite) : Cabs.init_name) : varinfo =
  let open Current_loc.Operators in
  Kernel.debug ~dkey:Kernel.dkey_typing_global "createGlobal: %s" n;
  (* If the global is a Frama-C builtin, set the generated flag *)
  if is_stdlib_macro n && get_current_stdheader () = "" then begin
    Kernel.warning ~wkey:Kernel.wkey_cert_msc_38 ~current:true
      "Attempt to declare %s as external identifier outside of the stdlib. \
       It is supposed to be a macro name and cannot be declared. See CERT C \
       coding rule MSC38-C" n
  end;
  let is_fc_builtin {Cabs.expr_node=enode} =
    match enode with Cabs.VARIABLE "FC_BUILTIN" -> true | _ -> false
  in
  let is_fc_stdlib {Cabs.expr_node=enode} =
    match enode with Cabs.VARIABLE v when v = fc_stdlib -> true | _ -> false
  in
  let isgenerated =
    List.exists (fun (_,el) -> List.exists is_fc_builtin el) a
  in
  let islibc =
    List.exists (fun (_,el) -> List.exists is_fc_stdlib el) a
  in
  (* Make a first version of the varinfo *)
  let vi_loc = convLoc cloc in
  let vi =
    makeVarInfoCabs
      ~ghost ~kind:`GlobalDecl ~referenced:islibc ~isgenerated
      vi_loc (t,s,b,attr_list) (n,ndt,a)
  in
  (* Add the variable to the environment before doing the initializer
   * because it might refer to the variable itself *)
  if isFunctionType vi.vtype then begin
    FuncLocs.add_loc ?spec:logic_spec (Current_loc.get ()) vi_loc n;
    if inite != Cabs.NO_INIT  then
      Kernel.error ~once:true ~current:true
        "Function declaration with initializer (%s)\n" vi.vname;
  end else if Option.is_some logic_spec then begin
    Kernel.warning ~wkey:Kernel.wkey_annot_error ~current:true ~once:true
      "Global variable %s is not a function. It cannot have a contract."
      vi.vname
  end;
  let isadef =
    not (isFunctionType vi.vtype) &&
    (inite != Cabs.NO_INIT
     ||
     (* tentative definition, but definition nevertheless. *)
     vi.vstorage = NoStorage || vi.vstorage = Static)
  in
  let vi, alreadyInEnv = makeGlobalVarinfo isadef vi in
  (* Do the initializer and complete the array type if necessary *)
  let init : init option =
    if inite = Cabs.NO_INIT then
      None
    else
      let se, ie', et, _ =
        doInitializer loc (ghost_local_env ghost) vi inite
      in
      (* Maybe we now have a better type?  Use the type of the
       * initializer only if it really differs from the type of
       * the variable. *)
      if unrollType vi.vtype != unrollType et then
        Cil.update_var_type vi et;
      if isNotEmpty se then begin
        Kernel.error ~once:true ~current:true
          "invalid global initializer @[%a@]" d_chunk se;
      end;
      Some ie'
  in

  try
    let oldloc = H.find alreadyDefined vi.vname in
    if init != None then begin
      (* function redefinition is taken care of elsewhere. *)
      Kernel.error ~once:true ~current:true
        "Global %s was already defined at %a" vi.vname Cil_printer.pp_location oldloc;
    end;
    Kernel.debug ~dkey:Kernel.dkey_typing_global
      " global %s was already defined" vi.vname;
    (* Do not declare it again, but update the spec if any *)
    if isFunctionType vi.vtype then
      begin
        match logic_spec with
        | None -> ()
        | Some (spec,_) ->
          let l1 = get_formals vi in
          let l2 = Cil.getFormalsDecl vi in
          List.iter2
            (fun x y ->
               if x != y then
                 Kernel.fatal ~current:true
                   "Function %s: formals are not shared between AST and \
                    FormalDecls table" vi.vname)
            l1 l2;
          try
            let known_behaviors = find_existing_behaviors vi in
            let spec =
              Ltyping.funspec
                known_behaviors vi (Some(get_formals vi)) vi.vtype spec
            in
            update_funspec_in_theFile vi spec
          with LogicTypeError ((source,_),msg) ->
            Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
              "%s. Ignoring specification of function %s" msg vi.vname
      end ;
    vi
  with Not_found -> begin
      (* Not already defined *)
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        " first definition for %s(%d)\n" vi.vname vi.vid;
      if init != None then begin
        (* weimer: Sat Dec  8 17:43:34  2001
         * MSVC NT Kernel headers include this lovely line:
         * extern const GUID __declspec(selectany) \
         *  MOUNTDEV_MOUNTED_DEVICE_GUID = { 0x53f5630d, 0xb6bf, 0x11d0, { \
         *  0x94, 0xf2, 0x00, 0xa0, 0xc9, 0x1e, 0xfb, 0x8b } };
         * So we allow "extern" + "initializer" if "const" is
         * around. *)
        (* sm: As I read the ISO spec, in particular 6.9.2 and 6.7.8,
         * "extern int foo = 3" is exactly equivalent to "int foo = 3";
         * that is, if you put an initializer, then it is a definition,
         * and "extern" is redundantly giving the name external linkage.
         * gcc emits a warning, I guess because it is contrary to
         * usual practice, but I think CIL warnings should be about
         * semantic rather than stylistic issues, so I see no reason to
         * even emit a warning. *)
        if vi.vstorage = Extern then
          vi.vstorage <- NoStorage;     (* equivalent and canonical *)

        IH.remove mustTurnIntoDef vi.vid;
        cabsPushGlobal (GVar(vi, {init = init}, Current_loc.get ()));
        H.add alreadyDefined vi.vname (Current_loc.get ());
        vi
      end else begin
        if not (isFunctionType vi.vtype) &&
           (vi.vstorage = NoStorage || vi.vstorage = Static)
           && not (IH.mem mustTurnIntoDef vi.vid) then
          begin
            IH.add mustTurnIntoDef vi.vid true
          end;
        if not alreadyInEnv then begin (* Only one declaration *)
          (* If it has function type it is a prototype *)
          (* NB: We add the formal prms in the env*)
          if isFunctionType vi.vtype then begin
            if not vi.vdefined then
              setFormalsDecl vi vi.vtype;
            let spec, loc =
              match logic_spec with
              | None -> empty_funspec (), Current_loc.get ()
              | Some (spec,loc) ->
                begin
                  let<> UpdatedCurrentLoc = loc in
                  let loc = Current_loc.get () in
                  let res =
                    try
                      (* it can not have old behavior names, since this is the
                         first time we see the declaration.
                      *)
                      Ltyping.funspec [] vi None vi.vtype spec
                    with LogicTypeError ((source,_),msg) ->
                      Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
                        "%s. Ignoring specification of function %s" msg vi.vname;
                      empty_funspec ()
                  in
                  res, loc
                end
            in
            cabsPushGlobal (GFunDecl (spec, vi, loc));
          end
          else
            cabsPushGlobal (GVarDecl (vi, Current_loc.get ()));
          vi
        end else begin
          Kernel.debug ~dkey:Kernel.dkey_typing_global
            " already in env %s" vi.vname;
          (match logic_spec with
           | None -> ()
           | Some (spec,loc) ->
             let merge_spec = function
               | GFunDecl(old_spec, _, oldloc) ->
                 let behaviors =
                   List.map (fun b -> b.b_name) old_spec.spec_behavior
                 in
                 let spec =
                   try
                     Ltyping.funspec behaviors vi None vi.vtype spec
                   with LogicTypeError ((source,_),msg) ->
                     Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
                       "%s. Ignoring specification of function %s"
                       msg vi.vname;
                     empty_funspec ()
                 in
                 Logic_utils.merge_funspec ~oldloc old_spec spec
               | _ -> assert false
             in
             Current_loc.with_loc loc (update_fundec_in_theFile vi) merge_spec
          );
          vi
        end
      end
    end
    (*
      ignore (E.log "Env after processing global %s is:@\n%t@\n"
      n docEnv);
      ignore (E.log "Alpha after processing global %s is:@\n%t@\n"
      n docAlphaTable)
    *)

(* it can happen that the variable to be initialized appears in the
   auxiliary statements that contribute to its initialization (and thus
   are meant to occur before the corresponding Local_init statement. In
   that case, this function creates an auxiliary variable that is never
   defined as a placeholder.
   Note that in any case, if the execution attempts to evaluate
   the variable (either original or placeholder), the behavior is undefined.
   There are some cases where the evaluation will succeed, though, e.g. with
   size_t x = sizeof(x) > 6 ? sizeof(x): 6;
*)
and cleanup_autoreference vi chunk =
  let temp = ref None in
  let calls = ref [] in
  let extract_calls () =
    let res = !calls in
    calls := [];
    res
  in
  let vis =
    object(self)
      inherit Cil.nopCilVisitor

      method! vinst = function
        | Call _ | Local_init(_,ConsInit _,_) ->
          calls := ref (Option.get self#current_stmt) :: !calls;
          DoChildren
        | _ -> DoChildren

      method! vvrbl v =
        if Cil_datatype.Varinfo.equal v vi then begin
          match !temp with
          | Some v' -> ChangeTo v'
          | None ->
            let ghost = v.vghost in
            let loc = v.vdecl in
            let v' =
              newTempVar ~ghost loc (vi.vname ^ " initialization") true vi.vtype
            in
            temp := Some v';
            ChangeTo v'
        end else SkipChildren
    end
  in
  let transform_lvals l = List.map (visitCilLval vis) l in
  let treat_one (s, m, w, r, _) =
    let s' = visitCilStmt vis s in
    let m' = transform_lvals m in
    let w' = transform_lvals w in
    let r' = transform_lvals r in
    let c' = extract_calls () in
    (s', m', w', r', c')
  in
  let stmts = List.map treat_one chunk.stmts in
  match !temp with
  | None -> chunk
  | Some v -> local_var_chunk { chunk with stmts } v

(* Must catch the Static local variables. Make them global *)
and createLocal ghost ((_, sto, _, _) as specs)
    ((((n, ndt, a, cloc) : Cabs.name),
      (inite: Cabs.init_expression)) as init_name)
  : chunk =
  let loc = convLoc cloc in
  let checkArray init vi =
    if init == Cabs.NO_INIT
    then
      if Cil.isUnsizedArrayType vi.vtype
      then
        Kernel.error ~once:true ~current:true
          "variable %s with array type needs an explicit size or an initializer"
          vi.vorig_name
  in
  (* Check if we are declaring a function *)
  let rec isProto (dt: decl_type) : bool =
    match dt with
    | PROTO (JUSTBASE, _,_, _) -> true
    | PROTO (x, _,_, _) -> isProto x
    | PARENTYPE (_, x, _) -> isProto x
    | ARRAY (x, _, _) -> isProto x
    | PTR (_, x) -> isProto x
    | _ -> false
  in
  match ndt with
  (* Maybe we have a function prototype in local scope. Make it global. We
   * do this even if the storage is Static *)
  | _ when isProto ndt ->
    let vi = createGlobal loc ghost None specs init_name in
    (* Add it to the environment to shadow previous decls *)
    addLocalToEnv ghost n (EnvVar vi);
    LocalFuncHook.apply vi;
    empty

  | _ when sto = Static ->
    Kernel.debug ~dkey:Kernel.dkey_typing_global
      "createGlobal (local static): %s" n;
    (* Now alpha convert it to make sure that it does not conflict with
     * existing globals or locals from this function. *)
    let full_name =
      (* Mangled symbols (that is, starting with '_Z') are unique by
         construction. No need to add current function name as prefix. *)
      if String.starts_with ~prefix:"_Z" n && n <> "_Z"
      then n
      else !currentFunctionFDEC.svar.vname ^ "_" ^ n
    in
    let newname, _  = newAlphaName ghost true "" full_name in
    (* Make it global  *)
    let vi = makeVarInfoCabs ~ghost ~kind:`LocalStaticDecl loc specs (n, ndt, a)
    in
    checkArray inite vi;
    vi.vname <- newname;
    let attrs = Ast_attributes.add (fc_local_static,[]) vi.vattr in
    vi.vattr <- fc_stdlib_attribute attrs;

    (* However, we have a problem if a real global appears later with the
     * name that we have happened to choose for this one. Remember these names
     * for later. *)
    H.add staticLocals vi.vname vi;
    (* Add it to the environment as a local so that the name goes out of
     * scope properly *)
    addLocalToEnv ghost n (EnvVar vi);

    (* Maybe this is an array whose length depends on something with local
       scope, e.g. "static char device[ sizeof(local) ]".
       Const-fold the type to fix this. *)
    Cil.update_var_type vi (constFoldType vi.vtype);

    let init : init option =
      if inite = Cabs.NO_INIT then
        None
      else begin
        let se, ie', et, _ =
          doInitializer loc (ghost_local_env ghost) vi inite
        in
        (* Maybe we now have a better type?  Use the type of the
         * initializer only if it really differs from the type of
         * the variable. *)
        if unrollType vi.vtype != unrollType et then
          Cil.update_var_type vi et;
        if isNotEmpty se then
          Kernel.error ~once:true ~current:true "global static initializer";
        (* Check that no locals are referred by the initializer *)
        check_no_locals_in_initializer ie';
        (* Maybe the initializer refers to the function itself.
           Push a prototype for the function, just in case. *)
        cabsPushGlobal
          (GFunDecl (empty_funspec (), !currentFunctionFDEC.svar,
                     Current_loc.get ()));
        Cil.setFormalsDecl
          !currentFunctionFDEC.svar !currentFunctionFDEC.svar.vtype;
        Some ie'
      end
    in
    cabsPushGlobal (GVar(vi, {init = init}, Current_loc.get ()));
    static_var_chunk empty vi

  (* Maybe we have an extern declaration. Make it a global *)
  | _ when sto = Extern ->
    if inite <> Cabs.NO_INIT
    then
      Kernel.error ~current:true
        "\'extern\' local variable cannot have an initializer";
    let vi = createGlobal loc ghost None specs init_name in
    (* Add it to the local environment to ensure that it shadows previous
     * local variables *)
    addLocalToEnv ghost n (EnvVar vi);
    empty

  | _ ->
    (* Make a variable of potentially variable size. If se0 <> empty then
     * it is a variable size variable *)
    let vi,se0,len,isvarsize =
      makeVarSizeVarInfo ghost loc specs (n, ndt, a) in

    checkArray inite vi;
    let vi = alphaConvertVarAndAddToEnv true vi in        (* Replace vi *)
    if isvarsize then begin
      let free = vla_free_fun () in
      let destructor = AStr free.vname in
      let attr = (frama_c_destructor, [destructor]) in
      vi.vdefined <- true;
      vi.vattr <- Ast_attributes.add attr vi.vattr;
    end;
    let se1 =
      if isvarsize then begin (* Variable-sized array *)
        (* Make a local variable to keep the length *)
        let savelen =
          makeVarInfoCabs
            ~ghost
            ~kind:`LocalDecl
            loc
            (Machine.sizeof_type (), NoStorage, false, [])
            ("__lengthof_" ^ vi.vname,JUSTBASE, [])
        in
        (* Register it *)
        let savelen = alphaConvertVarAndAddToEnv true savelen in
        let se0 = local_var_chunk se0 savelen in
        (* Compute the allocation size *)
        let elt_size = new_exp ~loc (SizeOf (Cil.typeOf_pointed vi.vtype)) in
        let alloca_size =
          new_exp ~loc
            (BinOp(Mult,
                   elt_size,
                   new_exp ~loc (Lval (var savelen)),
                   Machine.sizeof_type ()))
        in
        (* Register the length *)
        IH.add varSizeArrays vi.vid alloca_size;
        (* There can be no initializer for this *)
        if inite != Cabs.NO_INIT then
          Kernel.error ~once:true ~current:true
            "Variable-sized array cannot have initializer";
        let se0 =
          (* add an assertion to ensure the given size is correctly bound:
             assert alloca_bounds: 0 < elt_size * array_size <= max_bounds
          *)
          (se0 +++ (
              let castloc = Current_loc.get () in
              let talloca_size =
                let size = Logic_utils.expr_to_term ~coerce:true elt_size in
                let tlen = Logic_utils.expr_to_term ~coerce:true len in
                Logic_const.term (TBinOp (Mult,size,tlen)) Linteger
              in
              let pos_size =
                let zero =  Logic_const.tinteger ~loc:castloc 0 in
                Logic_const.prel ~loc:castloc (Rlt, zero, talloca_size)
              in
              let max_size =
                let szTo = Cil.bitsSizeOf (Machine.sizeof_type ()) in
                let max_bound =  Logic_const.tint ~loc:castloc (Cil.max_unsigned_number szTo) in
                Logic_const.prel ~loc:castloc (Rle, talloca_size, max_bound)
              in
              let alloca_bounds = Logic_const.pand ~loc:castloc (pos_size, max_size) in
              let alloca_bounds =
                { alloca_bounds with pred_name = ["alloca_bounds"] }
              in
              let alloca_bounds =
                Logic_const.toplevel_predicate alloca_bounds
              in
              let annot =
                Logic_const.new_code_annotation (AAssert ([], alloca_bounds))
              in
              (mkStmtOneInstr ~ghost ~valid_sid
                 (Code_annot (annot, castloc)),
               [],[],[])))
        in
        let setlen =  se0 +++
                      (mkStmtOneInstr ~ghost ~valid_sid
                         (Set(var savelen, mkCast ~newt:savelen.vtype len,
                              Current_loc.get ())),
                       [],[],[])
        in
        (* Initialize the variable *)
        let alloca: varinfo = vla_alloc_fun () in
        if Kernel.DoCollapseCallCast.get () then
          (* do it in one step *)
          setlen +++
          (mkStmtOneInstr ~ghost ~valid_sid
             (Local_init (vi, ConsInit(alloca,[ alloca_size ],Plain_func),loc)),
           [],[var vi],[])
        else begin
          (* do it in two *)
          let rt, _, _, _ = splitFunctionType alloca.vtype in
          let tmp =
            newTempVar ~ghost loc
              (Format.asprintf "alloca(%a)" Cil_printer.pp_exp alloca_size)
              false rt
          in
          tmp.vdefined <- true;
          (local_var_chunk setlen tmp)
          +++ (mkStmtOneInstr ~ghost ~valid_sid
                 (Local_init
                    (tmp,ConsInit(alloca,[alloca_size],Plain_func),loc)),
               [],[],[])
          +++ (mkStmtOneInstr ~ghost ~valid_sid
                 (Local_init
                    (vi,AssignInit
                       (SingleInit
                          (mkCast ~newt:vi.vtype (new_exp ~loc (Lval(var tmp))))),
                     Current_loc.get ())),
               [],[var vi],[var tmp])
        end
      end else empty
    in
    let se1 = local_var_chunk se1 vi in
    if inite = Cabs.NO_INIT then
      se1 (* skipChunk *)
    else begin
      (* TODO: if vi occurs in se4, this is not a real initialization. *)
      vi.vdefined <- true;
      contains_temp_subarray := false;
      let se4, ie', et, r =
        doInitializer loc (ghost_local_env ghost) vi inite
      in
      let se4 = cleanup_autoreference vi se4 in
      (* Fix the length *)
      if Cil.isUnsizedArrayType vi.vtype && Cil.isSizedArrayType et
      then
        (* We have a length now *)
        Cil.update_var_type vi et
      else
        (match vi.vtype.tnode, ie' with
         (* Initializing a local array *)
         | TArray({ tnode = TInt (IChar|IUChar|ISChar) } as bt, None),
           SingleInit({enode = Const(CStr s);eloc=loc}) ->
           let t =
             mk_tarray ~tattr:vi.vtype.tattr bt
               (Some (integer ~loc (String.length s + 1)))
           in
           Cil.update_var_type vi t
         | _, _ -> ());
      (* Now create assignments instead of the initialization *)
      let (@@@) s1 s2 = s1 @@@ (s2, ghost) in
      let read = Cil_datatype.Lval.Set.elements r in
      let normal_init_chunk () =
        let normal_init = Local_init(vi, AssignInit ie', loc) in
        let normal_stmt = Cil.mkStmtOneInstr ~ghost ~valid_sid normal_init in
        i2c (normal_stmt, [ ], [ var vi ], read)
      in
      let se4', chunk_init =
        if not !contains_temp_subarray then se4, normal_init_chunk ()
        else begin
          contains_temp_subarray := false;
          match ie' with
          (* If ie' is already a tmp variable, enclose the chunk and extract tmp
             from inner locals to the new block locals.
          *)
          | SingleInit {enode=Lval(Var tmp, NoOffset)} when tmp.vtemp ->
            let f v = not @@ Cil_datatype.Varinfo.equal v tmp in
            let locals_no_tmp = List.filter f se4.locals in
            let se4_without_tmp = {se4 with locals = locals_no_tmp} in
            let enclosed = enclose_chunk ~ghost ~locals:[tmp] se4_without_tmp in
            enclosed, normal_init_chunk ()
          (* In other cases we hide the chunk and use the new tmp variable for
             the initialization. *)
          | SingleInit old_e ->
            let hidden_chunk, new_e =
              hide_chunk ~ghost ~loc read se4 old_e vi.vtype
            in
            let init_exp = SingleInit new_e in
            let init_instr = Local_init(vi, AssignInit init_exp, loc) in
            let init_stmt = Cil.mkStmtOneInstr ~ghost ~valid_sid init_instr in
            let init_vi_chunk = i2c (init_stmt, [ ], [ var vi ], [ ]) in
            hidden_chunk, init_vi_chunk
          | _ -> assert false
        end
      in
      (se1 @@@ se4') @@@ chunk_init
    end

and doAliasFun ghost vtype (thisname:string) (othername:string)
    (sname:single_name) (loc: cabsloc) : unit =
  (* This prototype declares that name is an alias for
     othername, which must be defined in this file *)
  (*   E.log "%s is alias for %s at %a\n" thisname othername  *)
  (*     Cil_printer.pp_location !currentLoc; *)
  let rt, formals, isva, _ = splitFunctionType vtype in
  if isva then Kernel.error ~once:true ~current:true "alias unsupported with varargs";
  let args = List.map
      (fun (n,_,_) -> { expr_loc = loc; expr_node = Cabs.VARIABLE n})
      (argsToList formals) in
  let call = Cabs.CALL ({expr_loc = loc; expr_node = Cabs.VARIABLE othername}, args,[])
  in
  let stmt = {stmt_ghost = false;
              stmt_node = if isVoidType rt then
                  Cabs.COMPUTATION({expr_loc = loc; expr_node = call}, loc)
                else Cabs.RETURN({expr_loc = loc; expr_node = call}, loc)}
  in
  let body = { Cabs.blabels = []; Cabs.battrs = []; Cabs.bstmts = [stmt] } in
  let fdef = Cabs.FUNDEF (None, sname, body, loc, loc) in
  ignore (doDecl empty_local_env true fdef);
  (* get the new function *)
  let v,_ =
    try lookupGlobalVar ghost thisname
    with Not_found -> Kernel.fatal ~current:true "error in doDecl"
  in
  v.vattr <- Ast_attributes.drop "alias" v.vattr


(* Do one declaration *)
and doDecl local_env (isglobal: bool) (def: Cabs.definition) : chunk =
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = get_definitionloc def in
  match def with
  | Cabs.DECDEF (logic_spec, (s, nl), loc) ->
    (* Do the specifiers exactly once *)
    let sugg =
      match nl with
      | [] -> ""
      | ((n, _, _, _), _) :: _ -> n
    in
    let ghost = local_env.is_ghost in
    let spec_res = doSpecList loc ghost sugg s in
    (* Do all the variables and concatenate the resulting statements *)
    let doOneDeclarator (acc: chunk) (name: init_name) =
      let (n,ndt,a,l),_ = name in
      let<> UpdatedCurrentLoc = l in
      if isglobal then begin
        let bt,_,_,attrs = spec_res in
        let vtype, nattr =
          doType local_env.is_ghost `GlobalDecl
            (AttrName false) bt (Cabs.PARENTYPE(attrs, ndt, a)) in
        (match Ast_attributes.filter "alias" nattr with
         | [] -> (* ordinary prototype. *)
           ignore (createGlobal l local_env.is_ghost logic_spec spec_res name)
         (*  E.log "%s is not aliased\n" name *)
         | [("alias", [AStr othername])] ->
           if not (isFunctionType vtype) || local_env.is_ghost then begin
             Kernel.warning ~current:true
               "%a: CIL only supports attribute((alias)) for C functions."
               Cil_printer.pp_location (Current_loc.get ());
             ignore (createGlobal l ghost logic_spec spec_res name)
           end else
             doAliasFun ghost vtype n othername (s, (n,ndt,a,l)) loc
         | _ ->
           Kernel.error ~once:true ~current:true
             "Bad alias attribute at %a" Cil_printer.pp_location (Current_loc.get()));
        acc
      end else
        acc @@@ (createLocal ghost spec_res name, ghost)
    in
    let res = List.fold_left doOneDeclarator empty nl in
    if isglobal then res
    else begin
      match logic_spec with
      | None -> res
      | Some (spec,loc) ->
        let loc' = convLoc loc in
        begin
          try
            let spec =
              Ltyping.code_annot loc' local_env.known_behaviors
                (Ctype !currentReturnType) (Logic_ptree.AStmtSpec ([],spec))
            in
            append_chunk_to_annot ~ghost
              (s2c
                 (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (spec,loc'))))
              res
          with LogicTypeError ((source,_),msg) ->
            Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
              "%s. Ignoring code annotation" msg;
            res
        end
    end
  | Cabs.TYPEDEF (ng, _) ->
    doTypedef local_env.is_ghost ng; empty

  | Cabs.ONLYTYPEDEF (s, _) ->
    doOnlyTypedef local_env.is_ghost s; empty

  | Cabs.GLOBASM (s, _) when isglobal ->
    cabsPushGlobal (GAsm (s, Current_loc.get ())); empty

  | Cabs.PRAGMA (a, _) when isglobal -> begin
      match doAttr local_env.is_ghost ("dummy", [a]) with
      | [("dummy", [a'])] ->
        let a'' =
          match a' with
          | ACons (s, args) ->
            process_align_pragma s args;
            process_stdlib_pragma s args >>?
            process_pack_pragma
          | _ -> (* Cil.fatal "Unexpected attribute in #pragma" *)
            Kernel.warning ~current:true "Unexpected attribute in #pragma";
            Some ( ("", [a']))
        in
        Option.iter
          (fun a'' ->
             cabsPushGlobal (GPragma (a'', Current_loc.get ())))
          a'';
        empty

      | _ -> abort_context "Too many attributes in pragma"
    end

  | Cabs.STATIC_ASSERT (e, s, _) -> begin
      let (_, _, cond_exp, _) = doExp local_env CConst e ADrop in
      begin
        match Cil.constFoldToInt ~machdep:true cond_exp with
        | Some i ->
          if Integer.(equal i zero) then
            Kernel.error ~current:true "static assertion failed%s%s@."
              (if s <> "" then ": " else "") s
        | None ->
          Kernel.error ~current:true
            "failed to evaluate constant expression in static assertion:@ \
             @[%a@]"
            Cprint.print_expression e
      end;
      (* _Static_assert is not stored in the Cil AST *)
      empty
    end

  | Cabs.FUNDEF (spec,((specs,(n,dt,a, _)) : Cabs.single_name),
                 (body : Cabs.block), loc1, loc2) when isglobal ->
    begin
      let ghost = local_env.is_ghost in
      let idloc = loc1 in
      let funloc = fst loc1, snd loc2 in
      let endloc = loc2 in
      Kernel.debug ~dkey:Kernel.dkey_typing_global
        "Definition of %s at %a\n" n Cil_printer.pp_location idloc;
      FuncLocs.add_loc ?spec loc1 endloc n;
      IH.clear callTempVars;

      (* Make the fundec right away, and we'll populate it later. We
       * need this throughout the code to create temporaries. *)
      currentFunctionFDEC :=
        { svar     = makeGlobalVar ~temp:false ~ghost n voidType;
          slocals  = []; (* For now we'll put here both the locals and
                          * the formals. Then "endFunction" will
                          * separate them *)
          sformals = []; (* Not final yet *)
          smaxid   = 0;
          sbody    = dummy_function.sbody; (* Not final yet *)
          smaxstmtid = None;
          sallstmts = [];
          sspec = empty_funspec ()
        };
      !currentFunctionFDEC.svar.vdecl <- idloc;

      (* Setup the environment. Add the formals to the locals. Maybe
       * they need alpha-conv  *)
      enterScope ();  (* Start the scope *)
      ignore (Cabsvisit.visitCabsBlock (new gatherLabelsClass) body);

      IH.clear varSizeArrays;

      (* Enter all the function's labels into the alpha conversion table *)
      ignore (Cabsvisit.visitCabsBlock (new registerLabelsVisitor) body);

      (* Do not process transparent unions in function definitions.
       * We'll do it later *)
      transparentUnionArgs := [];

      let bt,sto,inl,attrs = doSpecList idloc local_env.is_ghost n specs in
      !currentFunctionFDEC.svar.vinline <- inl;
      let ftyp, funattr =
        doType local_env.is_ghost `GlobalDecl
          (AttrName false) bt (Cabs.PARENTYPE(attrs, dt, a))
      in
      if Ast_attributes.contains "thread" funattr then begin
        let wkey = Kernel.wkey_inconsistent_specifier in
        let source = fst funloc in
        Kernel.warning ~wkey ~source "only objects can be thread-local"
      end;
      (* Format.printf "Attrs are %a@." d_attrlist funattr; *)
      Cil.update_var_type !currentFunctionFDEC.svar ftyp;
      !currentFunctionFDEC.svar.vattr <- funattr;
      !currentFunctionFDEC.svar.vstorage <- sto;
      let vi,has_decl =
        makeGlobalVarinfo true !currentFunctionFDEC.svar in
      (* Add the function itself to the environment. Add it before
       * you do the body because the function might be recursive. Add
       * it also before you add the formals to the environment
       * because there might be a formal with the same name as the
       * function and we want it to take precedence. *)
      (* Make a variable out of it and put it in the environment *)
      !currentFunctionFDEC.svar <- vi;

      (* If it is extern inline then we add it to the global
       * environment for the original name as well. This will ensure
       * that all uses of this function will refer to the renamed
       * function *)
      addGlobalToEnv ghost n (EnvVar !currentFunctionFDEC.svar);
      H.find_opt alreadyDefined !currentFunctionFDEC.svar.vname
      |>
      (Option.iter
         (fun loc ->
            abort_context
              "There is a definition already for %s \
               (previous definition was at %a)."
              n Cil_datatype.Location.pretty loc));
      H.add alreadyDefined !currentFunctionFDEC.svar.vname idloc;


          (*
            ignore (E.log "makefunvar:%s@\n type=%a@\n vattr=%a@\n"
            n Cil_datatype.Typ.pretty thisFunctionVI.vtype
            d_attrlist thisFunctionVI.vattr);
          *)

      (* makeGlobalVarinfo might have changed the type of the function
       * (when combining it with the type of the prototype). So get the
       * type only now. *)

      (**** Process the TYPE and the FORMALS ***)
      let _ =
        let (returnType, formals_t, isvararg, funta) =
          splitFunctionTypeVI !currentFunctionFDEC.svar
        in
        (* Record the returnType for doStatement *)
        currentReturnType   := returnType;


        (* Create the formals and add them to the environment. *)
        (* sfg: extract tsets for the formals from dt *)
        let cnt = ref 0 in
        let doFormal (loc : location) ((fn, ft, fa) as fd) =
          let ghost = ghost || isGhostFormalVarDecl fd in
          let f = makeVarinfo ~ghost ~temp:false ~loc false true fn ft in
          f.vattr <- fa;
          if f.vname = "" then begin
            f.vname <- "__x" ^ (string_of_int !cnt);
            incr cnt;
            f.vattr <- Ast_attributes.(add anonymous_attribute f.vattr);
          end;
          alphaConvertVarAndAddToEnv true f
        in
        let rec doFormals fl' ll' =
          begin
            match (fl', ll') with
            | [], _ -> []

            | fl, [] -> (* no more locs available *)
              List.map (doFormal (Current_loc.get ())) fl

            | f::fl, (_,(_,_,_,l))::ll ->
              (* sfg: these lets seem to be necessary to
               *  force the right order of evaluation *)
              let f' = doFormal (convLoc l) f in
              let fl' = doFormals fl ll in
              f' :: fl'
          end
        in
        let fmlocs = (match dt with PROTO(_, fml, _, _) -> fml | _ -> []) in
        let formals = doFormals (argsToList formals_t) fmlocs in
        (* in case of formals referred to in types of others, doType has
           put dummy varinfos. We need to fix them now that we have proper
           bindings.
           TODO: completely refactor the way formals' typechecking is done.
        *)
        let () = fixFormalsType formals in

        (* Recreate the type based on the formals. *)
        let ftype =
          let args = Some (List.map (fun f -> (f.vname, f.vtype, f.vattr)) formals) in
          mk_tfun ~tattr:funta returnType args isvararg
        in

        (*log "Funtype of %s: %a\n" n Cil_datatype.Typ.pretty ftype;*)

        (* Now fix the names of the formals in the type of the function
         * as well *)
        Cil.update_var_type !currentFunctionFDEC.svar ftype;
        !currentFunctionFDEC.sformals <- formals;
        (* we will revisit the spec for the declaration in order
           to change the formals according to the new variables.
        *)
        if has_decl then begin
          try
            Hashtbl.add alpha_renaming
              vi.vid
              (Cil.create_alpha_renaming
                 (Cil.getFormalsDecl vi) formals)
          with
          | Invalid_argument _ ->
            abort_context "Inconsistent formals"
          | Not_found ->
            (* the declaration comes from an
               implicit prototype. We do not have
               any spec anyway. However, we will have a declaration
               in the resulting AST, to which we must attach some
               formals.
            *)
            Cil.unsafeSetFormalsDecl vi formals
        end;
      in
      (* Now change the type of transparent union args back to what it
       * was so that the body type checks. We must do it this late
       * because makeGlobalVarinfo from above might choke if we give
       * the function a type containing transparent unions  *)
      let _ =
        let rec fixbackFormals (idx: int) (args: varinfo list) : unit=
          match args with
          | [] -> ()
          | a :: args' ->
            (* Fix the type back to a transparent union type *)
            (try
               let origtype = List.assq idx !transparentUnionArgs in
               Cil.update_var_type a origtype;
             with Not_found -> ());
            fixbackFormals (idx + 1) args'
        in
        fixbackFormals 0 !currentFunctionFDEC.sformals;
        transparentUnionArgs := [];
      in
      let behaviors = find_existing_behaviors !currentFunctionFDEC.svar in
      (******* Now do the spec *******)
      let merge_spec () =
        (* Merge pre-existing spec if needed. *)
        if has_decl then begin
          let merge_spec = function
            | GFunDecl(old_spec,_,oldloc) as g ->
              if not (Cil.is_empty_funspec old_spec) then begin
                rename_spec g;
                Logic_utils.merge_funspec ~oldloc
                  !currentFunctionFDEC.sspec old_spec;
                Logic_utils.clear_funspec old_spec;
              end;
            | _ -> assert false
          in
          update_fundec_in_theFile !currentFunctionFDEC.svar merge_spec
        end
      in
      begin
        match spec with
        | Some (spec,loc) ->
          let<> UpdatedCurrentLoc = loc in
          (try
             !currentFunctionFDEC.sspec <-
               Ltyping.funspec behaviors
                 !currentFunctionFDEC.svar
                 (Some !currentFunctionFDEC.sformals)
                 !currentFunctionFDEC.svar.vtype spec
           with LogicTypeError ((source,_),msg) ->
             Kernel.warning ~wkey:Kernel.wkey_annot_error ~source
               "%s. Ignoring logic specification of function %s"
               msg !currentFunctionFDEC.svar.vname);
          merge_spec ()
        | None -> merge_spec ()
      end;
      (********** Now do the BODY *************)
      let _ =
        let stmts =
          doBody
            { empty_local_env with
              known_behaviors =
                (List.map (fun x -> x.b_name)
                   !currentFunctionFDEC.sspec.spec_behavior)
                @ behaviors;
              is_ghost = local_env.is_ghost
            }
            body
        in
        (* Finish everything *)
        exitScope ();
        (* Now fill in the computed goto statement with cases. Do this
         * before mkFunctionbody which resolves the gotos *)
        (match !gotoTargetData with
         | Some (_switchv, switch) ->
           let switche, loc =
             match switch.skind with
             | Switch (switche, _, _, l) -> switche, l
             | _ ->
               Kernel.fatal ~current:true
                 "the computed goto statement not a switch"
           in
           (* Build a default chunk that segfaults *)
           let default =
             defaultChunk ~ghost
               loc
               (i2c (mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                       (Set ((Mem (mkCast ~newt:intPtrType (integer ~loc 0)),
                              NoOffset),
                             integer ~loc 0, loc)),[],[],[]))
           in
           let bodychunk = ref default in
           H.iter
             (fun lname laddr ->
                bodychunk :=
                  caseRangeChunk ~ghost
                    [integer ~loc laddr] loc
                    (gotoChunk ~ghost lname loc @@@ (!bodychunk, ghost)))
             gotoTargetHash;
           (* Now recreate the switch *)
           let newswitch = switchChunk ~ghost switche !bodychunk loc in
           (* We must still share the old switch statement since we
            * have already inserted the goto's *)
           let newswitchkind =
             match newswitch.stmts with
             | [ s, _, _,_,_] when newswitch.cases == []-> s.skind
             | _ ->
               Kernel.fatal ~current:true
                 "Unexpected result from switchChunk"
           in
           switch.skind <- newswitchkind

         | None -> ());
        (* Now finish the body and store it *)
        let body = mkFunctionBody ~ghost stmts in
        !currentFunctionFDEC.sbody <- body;
        (* Reset the global parameters *)
        gotoTargetData := None;
        H.clear gotoTargetHash;
        gotoTargetNextAddr := 0;
      in
      !currentFunctionFDEC.slocals <- (List.rev !currentFunctionFDEC.slocals);
      setMaxId !currentFunctionFDEC;

      (* Now go over the types of the formals and pull out the formals
       * with transparent union type. Replace them with some shadow
       * parameters and then add assignments  *)
      let _ =
        let newformals, newbody =
          List.fold_right (* So that the formals come out in order *)
            (fun f (accform, accbody) ->
               match isTransparentUnion f.vtype with
               | None -> (f :: accform, accbody)
               | Some fstfield ->
                 (* A new shadow to be placed in the formals. Use
                  * makeTempVar to update smaxid and all others but
                    do not insert as a local variable of [f]. *)
                 let loc = Current_loc.get () in
                 let shadow =
                   makeTempVar
                     !currentFunctionFDEC ~insert:false
                     fstfield.ftype
                 in
                 (* Now replace it with the current formal. *)
                 (shadow :: accform,
                  mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
                    (Set ((Var f, Field(fstfield, NoOffset)),
                          new_exp ~loc (Lval (var shadow)), loc))
                  :: accbody))
            !currentFunctionFDEC.sformals
            ([], !currentFunctionFDEC.sbody.bstmts)
        in
        !currentFunctionFDEC.sbody.bstmts <- newbody;
        (* To make sure sharing with the type is proper *)
        setFormals !currentFunctionFDEC newformals;
      in

      (* Now see whether we can fall through to the end of the function *)
      if blockFallsThrough !currentFunctionFDEC.sbody then begin
        let loc = endloc in
        let<> UpdatedCurrentLoc = endloc in
        let protect_return,retval =
          (* Guard the [return] instructions we add with an
             [\assert \false]*)
          let pfalse = Logic_const.unamed ~loc Pfalse in
          let pfalse = { pfalse with pred_name = ["missing_return"] } in
          let pfalse = Logic_const.toplevel_predicate pfalse in
          let assert_false () =
            let annot =
              Logic_const.new_code_annotation (AAssert ([], pfalse))
            in
            Cil.mkStmt ~ghost ~valid_sid (Instr(Code_annot(annot,loc)))
          in
          let rt = unrollType !currentReturnType in
          match rt.tnode with
          | TVoid -> [], None
          | TInt _ | TEnum _ | TFloat _ | TPtr _ ->
            let res = Some (mkCastT ~oldt:intType ~newt:rt (zero ~loc)) in
            if !currentFunctionFDEC.svar.vname = "main" then
              [],res
            else begin
              Kernel.warning ~current:true ~wkey:Kernel.wkey_cert_msc_37
                "Body of function %s falls-through. \
                 Adding a return statement"
                !currentFunctionFDEC.svar.vname;
              [assert_false ()], res
            end
          | _ ->
            (* 0 is not an admissible value for the return type.
               On the other hand, *( T* )0 is. We're not supposed
               to get there anyway. *)
            let null_ptr =
              mkCastT ~oldt:intType ~newt:(mk_tptr rt) (zero ~loc)
            in
            let res =
              Some (new_exp ~loc (Lval (mkMem ~addr:null_ptr ~off:NoOffset)))
            in
            Kernel.warning ~current:true ~wkey:Kernel.wkey_cert_msc_37
              "Body of function %s falls-through. \
               Adding a return statement"
              !currentFunctionFDEC.svar.vname;
            [assert_false ()], res
        in
        if not (typeHasAttribute "noreturn" !currentFunctionFDEC.svar.vtype)
        then
          !currentFunctionFDEC.sbody.bstmts <-
            !currentFunctionFDEC.sbody.bstmts
            @ protect_return @
            [mkStmt ~ghost ~valid_sid (Return(retval, endloc))]
      end;

      (* ignore (E.log "The env after finishing the body of %s:\n%t\n"
         n docEnv); *)
      cabsPushGlobal (GFun (!currentFunctionFDEC, funloc));
      currentFunctionFDEC := dummy_function;
      empty
    end (* FUNDEF *)

  | LINKAGE (n, _, dl) ->
    if n <> "C" then
      Kernel.warning ~current:true
        "Encountered linkage specification \"%s\"" n;
    if not isglobal then
      Kernel.error ~once:true ~current:true
        "Encountered linkage specification in local scope";
    (* For now drop the linkage on the floor !!! *)
    List.iter
      (fun d ->
         let s = doDecl local_env isglobal d in
         if isNotEmpty s then
           abort_context "global initializer with side-effects")
      dl;
    empty

  | Cabs.GLOBANNOT decls when isglobal ->
    List.iter
      (fun decl  ->
         let loc = convLoc decl.Logic_ptree.decl_loc in
         let<> UpdatedCurrentLoc = loc in
         try
           match Ltyping.annot decl with
           | None -> ()
           | Some tdecl ->
             let attr = fc_stdlib_attribute [] in
             let tdecl =
               List.fold_left
                 (Fun.flip Logic_utils.add_attribute_glob_annot) tdecl attr
             in
             cabsPushGlobal (GAnnot(tdecl,Current_loc.get ()))
         with LogicTypeError ((source,_),msg) ->
           Kernel.warning
             ~wkey:Kernel.wkey_annot_error ~source
             "%s. Ignoring global annotation" msg
      )
      decls;
    empty

  | Cabs.GLOBANNOT _ | Cabs.PRAGMA _ | Cabs.GLOBASM _ | Cabs.FUNDEF _ ->
    abort_context "this form of declaration must be global"

and doTypedef ghost ((specs, nl): Cabs.name_group) =
  (* Do the specifiers exactly once *)
  if !scopes <> [] then
    Kernel.warning
      ~once:true ~current:true ~wkey:Kernel.wkey_parser_unsupported
      "block-level typedefs currently unsupported;@ \
       trying to convert it to a global-level typedef.@ \
       Note that this may lead to incoherent error messages.";
  let bt, sto, inl, attrs =
    doSpecList (Current_loc.get()) ghost (suggestAnonName nl) specs
  in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let createTypedef ((n,ndt,a,_) : Cabs.name) =
    (*    E.s (error "doTypeDef") *)
    let newTyp, tattr =
      doType ghost `Typedef AttrType bt (Cabs.PARENTYPE(attrs, ndt, a))  in
    checkTypedefSize n newTyp;
    let tattr = fc_stdlib_attribute tattr in
    let newTyp' = cabsTypeAddAttributes tattr newTyp in
    checkRestrictQualifierDeep newTyp';
    let env = if ghost then ghost_env else env in
    if H.mem typedefs n && Datatype.String.Hashtbl.mem env n then
      (* check if type redefinition is allowed (C11 only);
         in all cases, do not create a new type.
         TODO: if local typedef redefinitions are to be supported, then the new type
         must be created if the definition is syntactically valid. *)
      begin
        if !scopes <> [] then
          Kernel.failure ~current:true
            "redefinition of a typedef in a non-global scope is currently unsupported";
        let typeinfo = H.find typedefs n in
        let _, oldloc = lookupType ghost "type" n in
        if areCompatibleTypes typeinfo.ttype newTyp' then
          begin
            let error_conflicting_types () =
              Kernel.error ~current:true
                "redefinition of type '%s' in the same scope with conflicting type.@ \
                 Previous declaration was at %a"
                n Cil_datatype.Location.pretty oldloc
            in
            let warn_c11_redefinition () =
              Kernel.warning ~wkey:Kernel.wkey_c11 ~current:true
                "redefinition of type '%s' in the same scope is only allowed \
                 in C11.@ Previous declaration was at %a" n
                Cil_datatype.Location.pretty oldloc
            in
            (* Tested with GCC+Clang: redefinition of compatible types in same scope:
               - enums are NOT allowed, except if they refer to the exact same
                 enumerated type
               - composite types are allowed only if the composite type itself is
                 not redefined (complex rules; with some extra tag checking performed
                 in compatibleTypesp, we use tags here to detect redefinitions,
                 which are invalid)
               - redefinition via a typedef of a struct/union/enum IS allowed;
               - other types are allowed. *)
            if declared_in_current_scope ~ghost n then
              begin
                match newTyp'.tnode with (* do NOT unroll type here,
                                            redefinitions of typedefs are ok *)
                | TComp newci ->
                  (* Composite types with different tags may be compatible, but here
                     we use the tags to try and detect if the type is being redefined,
                     which is NOT allowed. *)
                  begin
                    match unrollTypeNode typeinfo.ttype with
                    | TComp ci ->
                      if ci.cname <> newci.cname then
                        (* different tags => we consider that the type is being redefined *)
                        error_conflicting_types ()
                      else
                        (* redeclaration in same scope valid only in C11 *)
                        warn_c11_redefinition ()
                    | _ -> (* because of the compatibility test, this should not happen *)
                      Kernel.fatal ~current:true "typeinfo.ttype (%a) should be TComp"
                        Cil_datatype.Typ.pretty typeinfo.ttype
                  end
                | TEnum newei -> (* GCC/Clang: "conflicting types" *)
                  let t = unrollType typeinfo.ttype in
                  (match t.tnode with
                   | TEnum ei ->
                     if ei.ename <> newei.ename then
                       error_conflicting_types ()
                     else
                       warn_c11_redefinition ()
                   | TInt _ -> error_conflicting_types ()
                   | _ ->
                     Kernel.fatal
                       ~current:true "typeinfo.ttype (%a) should be an Enum"
                       Cil_datatype.Typ.pretty t)
                | TInt _ ->
                  let t = unrollType typeinfo.ttype in
                  (match t.tnode with
                   | TInt _ -> warn_c11_redefinition ()
                   | TEnum _ -> error_conflicting_types ()
                   | _ ->
                     Kernel.fatal
                       ~current:true "typeinfo.ttype (%a) should be an int"
                       Cil_datatype.Typ.pretty t
                  )
                | _ -> (* redeclaration in same scope valid only in C11 *)
                  warn_c11_redefinition ()
              end
          end
        else if declared_in_current_scope ~ghost n then
          Kernel.error ~current:true
            "redefinition of type '%s' in the same scope with incompatible type.@ \
             Previous declaration was at %a" n Cil_datatype.Location.pretty oldloc;
      end
    else (* effectively create new type *) begin
      let n', _  = newAlphaName ghost true "type" n in
      let ti =
        { torig_name = n; tname = n';
          ttype = newTyp'; treferenced = false }
      in
      (* Since we use the same name space, we might later hit a global with
       * the same name and we would want to change the name of the global.
       * It is better to change the name of the type instead. So, remember
       * all types whose names have changed *)
      H.add typedefs n' ti;
      let namedTyp = mk_tnamed ti in
      (* Register the type. register it as local because we might be in a
       * local context  *)
      addLocalToEnv ghost (kindPlusName "type" n) (EnvTyp namedTyp);
      cabsPushGlobal (GType (ti, Current_loc.get ()))
    end
  in
  List.iter createTypedef nl

and doOnlyTypedef ghost (specs: Cabs.spec_elem list) : unit =
  let bt, sto, inl, attrs =
    doSpecList (Current_loc.get()) ghost "" specs
  in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let restyp, nattr =
    doType ghost `Typedef AttrType bt (Cabs.PARENTYPE(attrs, Cabs.JUSTBASE, []))
  in
  if nattr <> [] then
    Kernel.warning ~current:true "Ignoring identifier attribute";
  (* doSpec will register the type. *)
  (* See if we are defining a composite or enumeration type, and in that
   * case move the attributes from the defined type into the composite type
   * *)
  let isadef =
    List.exists
      (function
          Cabs.SpecType(Cabs.Tstruct(_, Some _, _)) -> true
        | Cabs.SpecType(Cabs.Tunion(_, Some _, _)) -> true
        | Cabs.SpecType(Cabs.Tenum(_, Some _, _)) -> true
        | _ -> false) specs
  in
  match restyp.tnode with
  | TComp ci ->
    if isadef then begin
      ci.cattr <- cabsAddAttributes ci.cattr restyp.tattr;
      (* The GCompTag was already added *)
    end else (* Add a GCompTagDecl *)
      cabsPushGlobal (GCompTagDecl(ci, Current_loc.get ()))
  | TEnum ei ->
    if isadef then begin
      ei.eattr <- cabsAddAttributes ei.eattr restyp.tattr;
    end else
      cabsPushGlobal (GEnumTagDecl(ei, Current_loc.get ()))
  | _ ->
    Kernel.warning ~current:true ~wkey:Kernel.wkey_unnamed_typedef
      "Ignoring unnamed typedef that does not introduce a struct or \
       enumeration type"

(* Now define the processors for body and statement *)
and doBody local_env (blk: Cabs.block) : chunk =
  let ghost = local_env.is_ghost in
  (* Rename the labels and add them to the environment *)
  List.iter (fun l -> ignore (genNewLocalLabel ghost l)) blk.blabels;
  (* See if we have some attributes *)
  let battrs = doAttributes ghost blk.Cabs.battrs in

  let bodychunk =
    afterConversion ~ghost
      (snd
         (List.fold_left   (* !!! @ evaluates its arguments backwards *)
            (fun ((new_behaviors,keep_block),prev) s ->
               let local_env =
                 { local_env with
                   known_behaviors =
                     new_behaviors @ local_env.known_behaviors
                 }
               in
               (* Format.eprintf "Considering statement: %a@."
                  Cprint.print_statement s; *)
               let res = doStatement local_env s in
               (* Keeps stmts originating from the same source
                  statement in a single block when the statement
                  follows a code annotation, so that the annotation
                  will be attached to the whole result and
                  not to the first Cil statement. This is only needed
                  for statement contracts and pragmas. Other (non-loop, as
                  they have special treatment) annotations operate purely
                  at current point and do not care about what happens to the
                  next statement.
               *)
               let new_behaviors, keep_next =
                 match s.stmt_node with
                 | CODE_ANNOT(Logic_ptree.AStmtSpec (_,s),_)
                 | CODE_SPEC (s,_) ->
                   List.map
                     (fun x -> x.Logic_ptree.b_name)
                     s.Logic_ptree.spec_behavior,
                   true
                 | CODE_ANNOT
                     (Logic_ptree.AExtended(_,is_loop,ext),loc) ->
                   let source = fst loc in
                   let kind =
                     Logic_env.extension_category ~plugin:ext.ext_plugin ext.ext_name
                   in
                   (match kind, is_loop with
                    | Ext_code_annot Ext_here, false -> [], false
                    | Ext_code_annot Ext_next_stmt, false -> [], true
                    | Ext_code_annot Ext_next_loop, true -> [], false
                    | Ext_code_annot Ext_next_both, _ -> [], not is_loop
                    | Ext_code_annot (Ext_here | Ext_next_stmt), true ->
                      Kernel.(
                        warning
                          ~source ~wkey:wkey_acsl_extension
                          "%s is a code annotation extension, \
                           but used here as a loop annotation" ext.ext_name);
                      [], false
                    | Ext_code_annot Ext_next_loop, false ->
                      Kernel.(
                        warning
                          ~source ~wkey:wkey_acsl_extension
                          "%s is a loop annotation extension, \
                           but used here as a code annotation" ext.ext_name);
                      [], false
                    | (Ext_global | Ext_contract), _ ->
                      Kernel.(
                        warning
                          ~source ~wkey:wkey_acsl_extension
                          "%s is not a code annotation extension" ext.ext_name);
                      [], false)
                 | _ -> [], false
               in
               (*               Format.eprintf "Done statement %a@." d_chunk res; *)
               let chunk =
                 if keep_block then
                   append_chunk_to_annot ~ghost prev res
                 else prev @@@ (res, ghost)
               in ((new_behaviors, keep_next), chunk))
            (([],false),empty)
            blk.Cabs.bstmts))
  in
  if battrs == [] && bodychunk.locals == []
  then begin
    (* keep block marked with FRAMA_C_KEEP_BLOCK or that defines local
          variables as independent blocks whatever happens.
    *)
    bodychunk
  end
  else begin
    let b = c2block ~ghost bodychunk in
    b.battrs <- battrs;
    let res = s2c (mkStmt ~ghost ~valid_sid (Block b)) in
    { res with cases = bodychunk.cases }
  end

and doBodyScope local_env blk =
  enterScope (); let res = doBody local_env blk in exitScope (); res

and doStatement local_env (s : Cabs.statement) : chunk =
  let open Current_loc.Operators in
  let mk_loop_annot a loc =
    try
      List.map
        (Ltyping.code_annot
           loc local_env.known_behaviors (Ctype !currentReturnType)) a
    with LogicTypeError ((source,_),msg) ->
      Kernel.warning
        ~wkey:Kernel.wkey_annot_error ~source
        "%s. Ignoring loop annotation" msg;
      []
  in
  let ghost = s.stmt_ghost in
  let local_env = { local_env with is_ghost = ghost } in
  let<> UpdatedCurrentLoc = convLoc (get_statementloc s) in
  match s.stmt_node with
  | Cabs.NOP (attr, loc) ->
    let sattr = Option.fold ~none:[] ~some:(doAttr local_env.is_ghost) attr in
    let stmt = mkEmptyStmt ~ghost ~valid_sid ~sattr ~loc () in
    { empty with stmts = [stmt,[],[],[],[]] }
  | Cabs.COMPUTATION (e, loc) ->
    let (lasts, data) = !gnu_body_result in
    if lasts == s then begin      (* This is the last in a GNU_BODY *)
      let (s', e', t') = doFullExp local_env CNoConst e (AExp None) in
      data := Some (e', t');      (* Record the result *)
      s'
    end else
      let (s', e', _) = doFullExp local_env CNoConst e ADrop in
      (* drop the side-effect free expression unless the whole computation
         is pure and it contains potential threats (i.e. dereference)
      *)
      if isEmpty s' && is_dangerous e'
      then
        s' @@@ (keepPureExpr ~ghost e' loc, ghost)
      else
        begin
          if (isEmpty s') then begin
            let name = !currentFunctionFDEC.svar.vorig_name in
            IgnorePureExpHook.apply (name, e');
          end;
          s'
        end

  | Cabs.BLOCK (b, _, _) ->
    let c = doBodyScope local_env b in
    let b = c2block ~ghost c in
    b.battrs <- Ast_attributes.add_list [(frama_c_keep_block,[])] b.battrs;
    let res = s2c (mkStmt ~ghost ~valid_sid (Block b)) in
    { res with cases = c.cases }

  | Cabs.IF(e, st, sf, _) ->
    let st' = doStatement local_env st in
    let sf' = doStatement local_env sf in
    doCondition ~is_loop:false local_env CNoConst e st' sf'

  | Cabs.WHILE(a,e,s,loc) ->
    startLoop true;
    let a = mk_loop_annot a loc in
    let s' = doStatement local_env s in
    let s' =
      if !doTransformWhile then
        s' @@@ (consLabContinue ~ghost skipChunk, ghost)
      else s'
    in
    let loc' = convLoc loc in
    let break_cond = breakChunk ~ghost loc' in
    exitLoop ();
    loopChunk ~ghost ~sattr:[("while",[])] a
      ((empty @@@
        (doCondition ~is_loop:true local_env CNoConst e skipChunk break_cond, ghost))
       @@@ (s', ghost))

  | Cabs.DOWHILE(a, e,s,loc) ->
    startLoop false;
    let a = mk_loop_annot a loc in
    let s' = doStatement local_env s in
    let loc' = convLoc loc in
    (* No 'break' instruction can exit the chunk *)
    let no_break chunk =
      List.for_all (fun (s, _, _, _, _) -> not (stmtCanBreak s)) chunk.stmts
    in
    (* Check if we are translating 'do { <s> } while (0)'. If so, translate
       it into '<s>' instead. Only active when -simplify-trivial-loops is
       set (default), as it impact plugins that compare the shape of the
       Cabs and of the Cil files. *)
    if Kernel.SimplifyTrivialLoops.get() &&
       isCabsZeroExp e (* exp is 0 or something equivalent *) &&
       a = [] (* No loop annot *) &&
       not (continueUsed ()) (* no 'continue' inside s *) &&
       no_break s' (* no break that exists s *)
    then (
      exitLoop ();
      s'
    )
    else
      let s'' =
        consLabContinue ~ghost
          (doCondition
             ~is_loop:true
             local_env
             CNoConst e skipChunk (breakChunk ~ghost loc'))
      in
      exitLoop ();
      loopChunk ~ghost ~sattr:[("dowhile",[])] a (s' @@@ (s'', ghost))

  | Cabs.FOR(a,fc1,e2,e3,s,loc) -> begin
      let loc' = convLoc loc in
      enterScope (); (* Just in case we have a declaration *)
      ForLoopHook.apply (fc1,e2,e3,s);
      let (se1, _, _) , has_decl =
        match fc1 with
        | FC_EXP e1 -> doFullExp local_env CNoConst e1 ADrop, false
        | FC_DECL d1 ->
          (doDecl local_env false d1, zero ~loc, voidType), true
      in
      let (se3, _, _) = doFullExp local_env CNoConst e3 ADrop in
      startLoop false;
      let a = mk_loop_annot a loc in
      let s' = doStatement local_env s in
      (*Kernel.debug "Loop body : %a" d_chunk s';*)
      let s'' = consLabContinue ~ghost se3 in
      let break_cond = breakChunk ~ghost loc' in
      exitLoop ();
      let c = s' @@@ (s'', ghost) in
      let c =
        match e2.expr_node with
        | Cabs.NOTHING -> (* This means true *)
          c
        | _ ->
          doCondition ~is_loop:true local_env CNoConst e2 skipChunk break_cond @@@ (c, ghost)
      in
      let res = se1 @@@ (loopChunk ~sattr:[("for",[])] ~ghost a c, ghost) in
      exitScope ();
      if has_decl then begin
        let chunk = s2c (mkStmt ~ghost ~valid_sid (Block (c2block ~ghost res)))
        in
        { chunk with cases = res.cases }
      end else res
    end

  | Cabs.BREAK loc ->
    let loc' = convLoc loc in
    breakChunk ~ghost loc'

  | Cabs.CONTINUE loc ->
    let loc' = convLoc loc in
    continueOrLabelChunk ~ghost loc'

  | Cabs.RETURN ({ expr_node = Cabs.NOTHING}, loc) ->
    let loc' = convLoc loc in
    if not (isVoidType !currentReturnType) then
      Kernel.error ~current:true
        "Return statement without a value in function returning %a\n"
        Cil_datatype.Typ.pretty !currentReturnType;
    returnChunk ~ghost None loc'

  | Cabs.RETURN (e, loc) ->
    let loc' = convLoc loc in
    (* Sometimes we return the result of a void function call *)
    if isVoidType !currentReturnType then begin
      Kernel.error ~current:true
        "Return statement with a value in function returning void";
      let (se, _, _) = doFullExp local_env CNoConst e ADrop in
      se @@@ (returnChunk ~ghost None loc', ghost)
    end else begin
      let rt =
        typeRemoveAttributes ["warn_unused_result"] !currentReturnType
      in
      let (se, e', et) =
        doFullExp local_env CNoConst e (AExp (Some rt)) in
      let (_, e'') = castTo et rt e' in
      se @@@ (returnChunk ~ghost (Some e'') loc', ghost)
    end

  | Cabs.SWITCH (e, s, loc) ->
    let loc' = convLoc loc in
    let (se, e', et) = doFullExp local_env CNoConst e (AExp None) in
    if not (Cil.isIntegralType et) then
      Kernel.abort ~once:true ~current:true "Switch on a non-integer expression.";
    let et' = Cil.integralPromotion et in
    let e' = mkCastT ~oldt:et ~newt:et' e' in
    enter_break_env ();
    let s' = doStatement local_env s in
    exit_break_env ();
    se @@@ (switchChunk ~ghost e' s' loc', ghost)

  | Cabs.CASE (e, s, loc) ->
    let loc' = convLoc loc in
    let (se, e', _) = doFullExp local_env CConst e (AExp None) in
    if isNotEmpty se || not (Cil.isIntegerConstant e') then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let chunk =
      caseRangeChunk ~ghost
        [if Machine.lower_constants () then constFold false e' else e']
        loc' (doStatement local_env s)
    in
    (* se has no statement, but can contain local variables, in
       particular in the case of a sizeof with side-effects. *)
    se @@@ (chunk,ghost)

  | Cabs.CASERANGE (el, eh, s, loc) ->
    let loc' = convLoc loc in
    let (sel, el', _) = doFullExp local_env CNoConst el (AExp None) in
    let (seh, eh', _) = doFullExp local_env CNoConst eh (AExp None) in
    if isNotEmpty sel || isNotEmpty seh then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let il, ih =
      match constFoldToInteger el', constFoldToInteger eh' with
      | Some il, Some ih -> il, ih
      | _ ->
        abort_context "non-constant expression(s) in case-range"
    in
    if il > ih then Kernel.error ~once:true ~current:true "Empty case range";
    (* Arbitrary limit to avoid building an impractical AST. *)
    if ih - il > 100_000 then abort_context "Case range too large";
    let rec mkAll (i: int) =
      if i > ih then [] else integer ~loc i :: mkAll (i + 1)
    in
    (sel @@@ (seh,ghost)) @@@
    (caseRangeChunk ~ghost (mkAll il) loc' (doStatement local_env s),
     ghost)

  | Cabs.DEFAULT (s, loc) ->
    let loc' = convLoc loc in
    defaultChunk ~ghost loc' (doStatement local_env s)
  | Cabs.LABEL (l, s, loc) ->
    let loc' = convLoc loc in
    Option.iter
      begin fun label ->
        let context = match label with
          | Here | Pre | Init -> "annotations"
          | LoopEntry | LoopCurrent -> "loop annotations"
          | Old | Post -> "contracts"
        in
        Kernel.warning ~current:true
          "%s is a builtin ACSL label, this C label is hidden in %s" l context
      end
      (Logic_typing.builtin_label l) ;
    add_label_env l;
    C_logic_env.add_current_label l;
    (* Lookup the label because it might have been locally defined *)
    let chunk =
      consLabel ~ghost (lookupLabel ghost l) (doStatement local_env s) loc' true
    in
    C_logic_env.reset_current_label (); chunk

  | Cabs.GOTO (l, loc) ->
    let loc' = convLoc loc in
    (* Maybe we need to rename this label *)
    gotoChunk ~ghost (lookupLabel ghost l) loc'

  | Cabs.COMPGOTO (e, loc) -> begin
      let loc' = convLoc loc in
      (* Do the expression *)
      let se, e', _ =
        doFullExp local_env CNoConst e (AExp (Some voidPtrType))
      in
      match !gotoTargetData with
      | Some (switchv, switch) -> (* We have already generated this one  *)
        (se
         @@@ (i2c(mkStmtOneInstr ~ghost ~valid_sid
                    (Set (var switchv, mkCast ~newt:intType e', loc')),
                  [],[],[]), ghost))
        @@@ (s2c(mkStmt ~ghost ~valid_sid (Goto (ref switch, loc'))), ghost)

      | None -> begin
          (* Make a temporary variable *)
          let vchunk = createLocal
              local_env.is_ghost
              (intType, NoStorage, false, [])
              (("__compgoto", Cabs.JUSTBASE, [], loc), Cabs.NO_INIT)
          in
          if not (isEmpty vchunk) then
            Kernel.fatal ~current:true
              "Non-empty chunk in creating temporary for goto *";
          let switchv, _ =
            try lookupVar ghost "__compgoto"
            with Not_found -> abort_context "Cannot find temporary for goto *";
          in
          (* Make a switch statement. We'll fill in the statements at the
           * end of the function *)
          let switch =
            mkStmt ~ghost ~valid_sid
              (Switch (new_exp ~loc (Lval(var switchv)),
                       mkBlock [], [], loc'))
          in
          (* And make a label for it since we'll goto it *)
          switch.labels <- [Label ("__docompgoto", loc', false)];
          gotoTargetData := Some (switchv, switch);
          (se @@@
           (i2c
              (mkStmtOneInstr ~ghost ~valid_sid
                 (Set (var switchv, mkCast ~newt:intType e', loc')),
               [],[],[]),
            ghost))
          @@@ (s2c switch, ghost)
        end
    end

  | Cabs.DEFINITION d ->
    doDecl local_env false d

  | Cabs.ASM (asmattr, tmpls, details, loc) ->
    (* Make sure all the outs are variables *)
    let loc' = convLoc loc in
    let attr' = doAttributes local_env.is_ghost asmattr in
    let stmts : chunk ref = ref empty in
    let ext_asm =
      match details with
      | None -> None
      | Some { aoutputs; ainputs; aclobbers; alabels} ->
        let asm_outputs =
          List.map
            (fun (id, c, e) ->
               let (se, e', _) = doFullExp local_env CNoConst e (AExp None) in
               let lv =
                 match e'.enode with
                 | Lval lval
                 | StartOf lval -> lval
                 | _ ->
                   abort_context "Expected lval for ASM outputs"
               in
               if not (isEmpty se) then
                 stmts := !stmts @@@ (se, ghost);
               (id, c, lv)) aoutputs
        in
        (* Get the side-effects out of expressions *)
        let asm_inputs =
          List.map
            (fun (id, c, e) ->
               let (r, se, e', _) =
                 doExp (no_paren_local_env local_env) CNoConst e (AExp None)
               in
               let se = add_reads ~ghost e'.eloc r se in
               if not (isEmpty se) then
                 stmts := !stmts @@@ (se, ghost);
               (id, c, e'))
            ainputs
        in
        let asm_clobbers = aclobbers in
        let asm_gotos =
          List.map
            (fun label ->
               let label = lookupLabel ghost label in
               let gref = ref dummyStmt in
               addGoto label gref;
               gref)
            alabels
        in
        Some { asm_outputs; asm_inputs; asm_clobbers; asm_gotos }
    in
    !stmts @@@
    (i2c(mkStmtOneInstr ~ghost:local_env.is_ghost ~valid_sid
           (Asm(attr', tmpls, ext_asm, loc')),[],[],[]),
     ghost)
  | THROW (e,loc) ->
    let loc' = convLoc loc in
    (match e with
     | None -> s2c (mkStmt ~ghost ~valid_sid (Throw (None,loc')))
     | Some e ->
       let se,e,t = doFullExp local_env CNoConst e (AExp None) in
       se @@@
       (s2c (mkStmt ~ghost ~valid_sid (Throw (Some (e,t),loc'))),ghost))
  | TRY_CATCH(stry,l,loc) ->
    let loc' = convLoc loc in
    let chunk_try = doStatement local_env stry in
    let type_one_catch (var,scatch) =
      enterScope();
      let vi =
        match var with
        | None -> Catch_all
        | Some (t,(n,ndt,a,ldecl)) ->
          let spec = doSpecList ldecl ghost n t in
          let vi =
            makeVarInfoCabs
              ~ghost ~kind:`LocalDecl ldecl spec (n,ndt,a)
          in
          addLocalToEnv ghost n (EnvVar vi);
          !currentFunctionFDEC.slocals <- vi :: !currentFunctionFDEC.slocals;
          Catch_exn(vi,[])
      in
      let chunk_catch = doStatement local_env scatch in
      exitScope();
      (vi,c2block ~ghost chunk_catch)
    in
    let catches = List.map type_one_catch l in
    s2c
      (mkStmt
         ~ghost ~valid_sid (TryCatch(c2block ~ghost chunk_try,catches,loc')))
  | CODE_ANNOT (a, loc) ->
    let loc' = convLoc loc in
    begin
      try
        let typed_annot =
          Ltyping.code_annot
            loc' local_env.known_behaviors (Ctype !currentReturnType) a
        in
        s2c (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (typed_annot,loc')))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning
          ~wkey:Kernel.wkey_annot_error ~source
          "%s. Ignoring code annotation" msg;
        BlockChunk.empty
    end

  | CODE_SPEC (a, loc) ->
    let loc' = convLoc loc in
    begin
      try
        let spec =
          Ltyping.code_annot loc' local_env.known_behaviors
            (Ctype !currentReturnType) (Logic_ptree.AStmtSpec ([],a))
        in
        s2c (mkStmtOneInstr ~ghost ~valid_sid (Code_annot (spec,loc')))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning
          ~wkey:Kernel.wkey_annot_error ~source
          "%s. Ignoring code annotation" msg;
        BlockChunk.empty
    end

let process_inline_def = function
  | GFun ( { svar }, _loc) when svar.vinline && svar.vstorage = NoStorage ->
    (* we have an inline definition, which is also an implicit external
       _declaration_ (see C11 6.7.4§7). Just rename its uses in the current
       translation unit. *)
    svar.vname <- svar.vname ^ "__fc_inline";
    (* inline definition is restricted to this translation unit. *)
    svar.vstorage <- Static;
  | GFun ({ svar },_) when svar.vinline && svar.vstorage = Extern ->
    (* The definition is a real external definition. We may as well remove
       the inline specification. *)
    svar.vinline <- false;
  | _ -> ()

(* Translate a file *)
let convFile (path, f) =
  Errorloc.clear_errors();
  (* Clean up the global types *)
  initGlobals();
  startFile ();
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear staticLocals;
  H.clear typedefs;
  H.clear alpha_renaming;
  Stack.clear packing_pragma_stack;
  current_packing_pragma := None;
  H.clear pragma_align_by_struct;
  current_pragma_align := None;
  Logic_env.prepare_tables ();
  anonCompFieldNameId := 0;
  Kernel.debug ~level:2 "Converting CABS->CIL" ;
  Cil_builtins.Builtin_functions.iter_sorted
    (fun name (resTyp, argTypes, isva) ->
       ignore (setupBuiltin name (resTyp, ArgTypes argTypes, isva)));
  let globalidx = ref 0 in
  let doOneGlobal (ghost,(d: Cabs.definition)) =
    let local_env = ghost_local_env ghost in
    let s = doDecl local_env true d in
    if isNotEmpty s then
      abort_context "global initializer with side-effects";
  in
  List.iter doOneGlobal f;
  let globals = fileGlobals () in
  List.iter process_inline_def globals;
  List.iter rename_spec globals;
  Logic_env.prepare_tables ();
  IH.clear mustTurnIntoDef;
  H.clear alreadyDefined;
  H.clear compInfoNameEnv;
  H.clear enumInfoNameEnv;
  H.clear staticLocals;
  H.clear typedefs;
  Datatype.String.Hashtbl.clear env;
  Datatype.String.Hashtbl.clear global_env;
  Datatype.String.Hashtbl.clear ghost_env;
  Datatype.String.Hashtbl.clear ghost_global_env;
  IH.clear callTempVars;
  H.clear alpha_renaming;
  constrExprId := 0;

  if false then Kernel.debug "Cabs2cil converted %d globals" !globalidx;
  (* We are done *)
  { fileName = path;
    globals;
    globinit = None;
    globinitcalled = false;
  }

(* export function without internal `relaxed' argument. *)
let areCompatibleTypes t1 t2 = areCompatibleTypes t1 t2
