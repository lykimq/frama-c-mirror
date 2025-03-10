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

(* -------------------------------------------------------------------------- *)
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes

open MemoryContext
open Lang
open Lang.F
open Memory

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> MemoryContext.param
  val iter: ?kf:kernel_function -> init:bool -> (varinfo -> unit) -> unit
end

module Raw : VarUsage =
struct
  let datatype = "Raw"
  let param _x = MemoryContext.ByValue
  let iter ?kf ~init f =
    begin
      ignore init ;
      Globals.Vars.iter (fun x _initinfo -> f x) ;
      match kf with
      | None -> ()
      | Some kf -> List.iter f (Kernel_function.get_formals kf) ;
    end
end

module Static : VarUsage =
struct
  let datatype = "Static"
  let param x =
    let open Cil_types in
    if x.vaddrof || Cil.isArrayType x.vtype || Cil.isPointerType x.vtype
    then MemoryContext.ByAddr else MemoryContext.ByValue
  let iter = Raw.iter
end

module Make(V : VarUsage)(M : Memory.Model) =
struct

  (* -------------------------------------------------------------------------- *)
  (* ---  Model                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let datatype = "MemVar." ^ V.datatype ^ M.datatype
  let configure = M.configure
  let no_binder = { bind = fun _ f v -> f v }
  let configure_ia _ = no_binder

  let hypotheses p =
    let kf,init = match WpContext.get_scope () with
      | WpContext.Global -> None,false
      | WpContext.Kf f ->
        Some f, CfgInfos.is_entry_point f in
    let w = ref p in
    V.iter ?kf ~init (fun vi -> w := MemoryContext.set vi (V.param vi) !w) ;
    M.hypotheses !w

  (* -------------------------------------------------------------------------- *)
  (* ---  Chunk                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let is_framed_var x = not x.vglob && not x.vaddrof
  (* Can not use VarUsage info, since (&x) can still be passed
     to the function and be modified by the call (when it assigns everything). *)

  module VAR =
  struct
    type t = varinfo
    let self = "var"
    let hash = Varinfo.hash
    let equal = Varinfo.equal
    let compare = Varinfo.compare
    let pretty = Varinfo.pretty
    let typ_of_chunk x =
      match V.param x with
      | ByRef -> Cil.typeOf_pointed x.vtype
      | _ -> x.vtype
    let tau_of_chunk x = Lang.tau_of_ctype (typ_of_chunk x)
    let is_framed = is_framed_var
    let is_init _ = false
    let is_primary _ = true
    let basename_of_chunk = LogicUsage.basename
  end

  module VALLOC =
  struct
    type t = varinfo
    let self = "alloc"
    let hash = Varinfo.hash
    let compare = Varinfo.compare
    let equal = Varinfo.equal
    let pretty = Varinfo.pretty
    let tau_of_chunk _x = Qed.Logic.Bool
    let basename_of_chunk x =
      match V.param x with
      | ByRef ->
        "ra_" ^ LogicUsage.basename x
      | NotUsed | ByValue | ByShift | ByAddr | InContext _ | InArray _ ->
        "ta_" ^ LogicUsage.basename x
    let is_init _ = false
    let is_primary _ = false
    let is_framed = is_framed_var
  end

  module VINIT =
  struct
    type t = varinfo
    let self = "init"
    let hash = Varinfo.hash
    let compare = Varinfo.compare
    let equal = Varinfo.equal
    let pretty = Varinfo.pretty
    let typ_of_chunk x =
      match V.param x with
      | ByRef -> Cil.typeOf_pointed x.vtype
      | _ -> x.vtype
    let tau_of_chunk x = Lang.init_of_ctype (typ_of_chunk x)
    let is_init _ = true
    let is_primary _ = false
    let is_framed = is_framed_var
    let basename_of_chunk x = "Init_" ^ (LogicUsage.basename x)
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Sigma                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module SVAR = Sigma.Make(VAR)
  module SALLOC = Sigma.Make(VALLOC)
  module SINIT = Sigma.Make(VINIT)

  (* -------------------------------------------------------------------------- *)
  (* ---  State Pretty Printer                                              --- *)
  (* -------------------------------------------------------------------------- *)

  type ichunk = Iref of varinfo | Ivar of varinfo | Iinit of varinfo

  module IChunk =
  struct

    let compare_var x y =
      let rank x =
        if x.vformal then 0 else
        if x.vglob then 1 else
        if x.vtemp then 3 else 2 in
      let cmp = rank x - rank y in
      if cmp <> 0 then cmp else Varinfo.compare x y

    type t = ichunk
    let hash = function
      | Iref x | Ivar x -> Varinfo.hash x
      | Iinit x -> 13 * Varinfo.hash x
    let compare x y =
      match x,y with
      | Iref x , Iref y -> Varinfo.compare x y
      | Ivar x , Ivar y -> compare_var x y
      | Iinit x, Iinit y -> compare_var x y
      | Iref _ , _ -> (-1)
      | Iinit _, _ -> (-1)
      | _ , Iref _ -> 1
      | _ , Iinit _ -> 1

    let equal x y =
      match x,y with
      | Iref x, Iref y | Ivar x, Ivar y | Iinit x, Iinit y -> Varinfo.equal x y
      |  _, _ -> false

  end

  module Icmap = Qed.Mergemap.Make(IChunk)

  let ilval = function
    | Iref x -> (Mvar x,[Mindex e_zero])
    | Ivar x | Iinit x -> (Mvar x,[])

  let imval c = match c with
    | Iref _ | Ivar _ -> Memory.Mlval (ilval c)
    | Iinit _ -> Memory.Minit (ilval c)

  let is_ref x =
    match V.param x with
    | InContext _ | InArray _ | ByRef -> true
    | _ -> false

  let iref x = if is_ref x then Iref x else Ivar x

  let lookup (s : Sigma.state) e =
    try
      match Sigma.ckind @@ Tmap.find e s with
      | SVAR.Mu x -> imval (iref x)
      | SINIT.Mu x -> imval (Iinit x)
      | SALLOC.Mu _ -> raise Not_found
      | _ -> raise Not_found
    with Not_found ->
      M.lookup s e

  let icmap domain istate =
    Tmap.fold
      (fun m c w ->
         if Vars.intersect (F.vars m) domain
         then
           match Sigma.ckind c with
           | SVAR.Mu x -> Icmap.add (iref x) m w
           | SINIT.Mu x -> Icmap.add (Iinit x) m w
           | _ -> w
         else w
      ) istate Icmap.empty

  let rec diff lv v1 v2 =
    if v1 == v2 then Bag.empty else
      match F.repr v2 with
      | Qed.Logic.Aset(m , k , vk) ->
        let upd = diff (Mstate.index lv k) (F.e_get m k) vk in
        Bag.concat (diff lv v1 m) upd
      | Qed.Logic.Rdef fvs ->
        rdiff lv v1 v2 fvs
      | _ ->
        Bag.elt (Mstore(lv,v2))

  and rdiff lv v1 v2 = function
    | (Lang.Cfield (fi, _k) as fd ,f2) :: fvs ->
      let f1 = F.e_getfield v1 fd in
      if f1 == f2 then rdiff lv v1 v2 fvs else
        let upd = diff (Mstate.field lv fi) f1 f2 in
        let m = F.e_setfield v2 fd f1 in
        Bag.concat upd (diff lv v1 m)
    | (Lang.Mfield _,_)::_ -> Bag.elt (Mstore(lv,v2))
    | [] -> Bag.empty

  let updates (seq : Sigma.state sequence) domain =
    let pre = icmap domain seq.pre in
    let post = icmap domain seq.post in
    let pool = ref Bag.empty in
    Icmap.iter2
      (fun c v1 v2 ->
         match v1 , v2 with
         | _ , None -> ()
         | None , Some v -> pool := Bag.add (Mstore(ilval c,v)) !pool
         | Some v1 , Some v2 -> pool := Bag.concat (diff (ilval c) v1 v2) !pool
      ) pre post ;
    Bag.concat !pool (M.updates seq domain)

  (* -------------------------------------------------------------------------- *)
  (* ---  Location                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  type mem =
    | CVAL (* By-Value variable *)
    | CREF (* By-Ref variable *)
    | CTXT of MemoryContext.validity (* In-context pointer *)
    | CARR of MemoryContext.validity (* In-context array *)
    | HEAP (* In-heap variable *)

  let is_heap_allocated = function
    | CREF | CVAL -> false | HEAP | CTXT _ | CARR _ -> true

  type loc =
    | Ref of varinfo
    | Val of mem * varinfo * ofs list (* The varinfo has {i not} been contextualized yet *)
    | Loc of M.loc (* Generalized In-Heap pointer *)

  and ofs =
    | Field of fieldinfo
    | Shift of c_object * term

  type segment = loc rloc

  let rec ofs_vars xs = function
    | [] -> xs
    | Field _ :: ofs -> ofs_vars xs ofs
    | Shift(_,k) :: ofs -> ofs_vars (Vars.union xs (F.vars k)) ofs

  let vars = function
    | Ref _ -> Vars.empty
    | Loc l -> M.vars l
    | Val(_,_,ofs) -> ofs_vars Vars.empty ofs

  let rec ofs_occurs x = function
    | [] -> false
    | Field _ :: ofs -> ofs_occurs x ofs
    | Shift(_,k) :: ofs -> Vars.mem x (F.vars k) || ofs_occurs x ofs

  let occurs x = function
    | Ref _ -> false
    | Loc l -> M.occurs x l
    | Val(_,_,ofs) -> ofs_occurs x ofs

  let byte_offset n = function
    | Field fd -> F.e_add n (F.e_int (Ctypes.field_offset fd))
    | Shift(obj,k) -> F.e_add n (F.e_fact (Ctypes.sizeof_object obj) k)

  (* -------------------------------------------------------------------------- *)
  (* ---  Variable and Context                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let vtype m x =
    match m with
    | CVAL | HEAP -> x.vtype
    | CTXT _ | CREF -> Cil.typeOf_pointed x.vtype
    | CARR _ -> Cil_const.mk_tarray (Cil.typeOf_pointed x.vtype) None

  let vobject m x = Ctypes.object_of (vtype m x)

  let vbase m x =
    match m with
    | CVAL | HEAP -> x
    | _ -> { x with vglob = true ; vtype = vtype m x }

  (* -------------------------------------------------------------------------- *)
  (* ---  Pretty                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let rec pp_offset ~obj fmt = function
    | [] -> ()
    | Field f :: ofs ->
      Format.fprintf fmt ".%s" f.fname ;
      pp_offset ~obj:(object_of f.ftype) fmt ofs
    | Shift(elt,k) :: ofs ->
      if Ctypes.is_array obj ~elt then
        ( Format.fprintf fmt ".(%a)" F.pp_term k ;
          pp_offset ~obj:elt fmt ofs )
      else
        ( Format.fprintf fmt ".(%a : %a)" F.pp_term k Ctypes.pretty elt ;
          pp_offset ~obj:elt fmt ofs )

  let pp_mem fmt = function
    | CVAL -> Format.pp_print_string fmt "var"
    | CREF -> Format.pp_print_string fmt "ref"
    | CTXT _ -> Format.pp_print_string fmt "ptr"
    | CARR _ -> Format.pp_print_string fmt "arr"
    | HEAP -> Format.pp_print_string fmt "mem"

  (* re-uses strings that are used into the description of -wp-xxx-vars *)
  let pp_var_model fmt = function
    | ByValue | ByShift | NotUsed -> Format.pp_print_string fmt "non-aliased" (* cf.  -wp-unalias-vars *)
    | ByRef -> Format.pp_print_string fmt "by reference" (* cf. -wp-ref-vars *)
    | InContext _ | InArray _ -> Format.pp_print_string fmt "in an isolated context" (* cf. -wp-context-vars *)
    | ByAddr -> Format.pp_print_string fmt "aliased" (* cf. -wp-alias-vars *)

  let pretty fmt = function
    | Ref x -> VAR.pretty fmt x
    | Loc l -> M.pretty fmt l
    | Val(m,x,ofs) ->
      let obj = vobject m x in
      Format.fprintf fmt "@[%a:%a%a@]"
        pp_mem m VAR.pretty x
        (pp_offset ~obj) ofs

  let noref ~op var =
    Warning.error
      "forbidden %s variable '%a' considered %a.@\n\
       Use model 'Typed' instead or specify '-wp-unalias-vars %a'"
      op Varinfo.pretty var
      pp_var_model (V.param var)
      Varinfo.pretty var

  (* -------------------------------------------------------------------------- *)
  (* ---  Basic Constructors                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let null = Loc M.null

  let literal ~eid cst = Loc (M.literal ~eid cst)

  let cvar x = match V.param x with
    | NotUsed | ByValue | ByShift -> Val(CVAL,x,[])
    | ByAddr -> Val(HEAP,x,[])
    | InContext _ | InArray _ | ByRef -> Ref x

  (* -------------------------------------------------------------------------- *)
  (* ---  Nullable locations                                                --- *)
  (* -------------------------------------------------------------------------- *)

  module Nullable = WpContext.Generator(Varinfo)
      (struct
        let name = "MemVar.Nullable"
        type key = varinfo
        type data = lfun

        let compile v =
          let result = t_addr () in
          let lfun = Lang.generated_f ~result "pointer_%s" v.vname in
          let cluster =
            Definitions.cluster ~id:"Globals" ~title:"Context pointers" ()
          in
          Definitions.define_symbol {
            d_lfun = lfun ;
            d_types = 0 ;
            d_params = [] ;
            d_definition = Definitions.Logic result ;
            d_cluster = cluster
          } ;
          lfun
      end)

  let nullable_address v =
    Lang.F.e_fun (Nullable.get v) []

  (* -------------------------------------------------------------------------- *)
  (* ---  Lifting                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let moffset l = function
    | Field f -> M.field l f
    | Shift(e,k) -> M.shift l e k

  let mloc_of_path m x ofs =
    let l = match m with
      | CTXT Nullable | CARR Nullable -> M.pointer_loc @@ nullable_address x
      | _ -> M.cvar (vbase m x)
    in
    List.fold_left moffset l ofs

  let mloc_of_loc = function
    | Loc l -> l
    | Ref x -> M.cvar x
    | Val(m,x,ofs) -> mloc_of_path m x ofs

  let pointer_loc p = Loc (M.pointer_loc p)
  let pointer_val l = M.pointer_val (mloc_of_loc l)

  let field l f =
    match l with
    | Loc l -> Loc (M.field l f)
    | Ref x -> noref ~op:"field access to" x
    | Val(m,x,ofs) ->
      if not @@ is_heap_allocated m then MemMemory.unsupported_union f ;
      Val(m,x,ofs @ [Field f])

  let rec ofs_shift obj k = function
    | [] -> [Shift(obj,k)]
    | [Shift(elt,i)] when Ctypes.equal obj elt -> [Shift(elt,F.e_add i k)]
    | f::ofs -> f :: ofs_shift obj k ofs

  let shift l obj k = match l with
    | Loc l -> Loc (M.shift l obj k)
    | Ref x -> noref ~op:"array access to" x
    | Val(m,x,ofs) -> Val(m,x,ofs_shift obj k ofs)

  let base_addr = function
    | Loc l -> Loc (M.base_addr l)
    | Ref x -> noref ~op:"base address of" x (* ??? ~suggest:ByValue *)
    | Val(m,x,_) -> Val(m,x,[])

  let base_offset = function
    | Loc l -> M.base_offset l
    | Ref x -> noref ~op:"offset address of" x (* ??? ~suggest:ByValue *)
    | Val(_,_,ofs) -> List.fold_left byte_offset e_zero ofs

  let block_length sigma obj = function
    | Loc l -> M.block_length sigma obj l
    | Ref x -> noref ~op:"block-length of" x
    | Val(m,x,_) ->
      begin match Ctypes.object_of (vtype m x) with
        | C_comp ({ cfields = None } as c) ->
          Cvalues.bytes_length_of_opaque_comp c
        | obj ->
          let size =
            if Ctypes.sizeof_defined obj
            then Ctypes.sizeof_object obj
            else if Wp_parameters.ExternArrays.get ()
            then max_int
            else Warning.error ~source:"MemVar" "Unknown array-size"
          in F.e_int size
      end

  let cast obj l = Loc(M.cast obj (mloc_of_loc l))
  let loc_of_int e a = Loc(M.loc_of_int e a)
  let int_of_loc i l = M.int_of_loc i (mloc_of_loc l)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Load                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec access_gen kind a = function
    | [] -> a
    | Field f :: ofs -> access_gen kind (e_getfield a (cfield ~kind f)) ofs
    | Shift(_,k) :: ofs -> access_gen kind (e_get a k) ofs

  let access = access_gen KValue
  let access_init = access_gen KInit

  let rec update_gen kind a ofs v = match ofs with
    | [] -> v
    | Field f :: ofs ->
      let phi = cfield ~kind f in
      let a_f = F.e_getfield a phi in
      let a_f_v = update_gen kind a_f ofs v in
      F.e_setfield a phi a_f_v
    | Shift(_,k) :: ofs ->
      let a_k = F.e_get a k in
      let a_k_v = update_gen kind a_k ofs v in
      F.e_set a k a_k_v

  let update = update_gen KValue
  let update_init = update_gen KInit

  let load sigma obj = function
    | Ref x ->
      begin match V.param x with
        | ByRef       -> Memory.Loc(Val(CREF,x,[]))
        | InContext n -> Memory.Loc(Val(CTXT n,x,[]))
        | InArray n   -> Memory.Loc(Val(CARR n,x,[]))
        | NotUsed | ByAddr | ByValue | ByShift -> assert false
      end
    | Val((CREF|CVAL),x,ofs) ->
      Memory.Val(access (SVAR.value sigma x) ofs)
    | Loc l ->
      Cvalues.map_value
        (fun l -> Loc l)
        (M.load sigma obj l)
    | Val((CTXT _|CARR _|HEAP) as m,x,ofs) ->
      Cvalues.map_value
        (fun l -> Loc l)
        (M.load sigma obj (mloc_of_path m x ofs))

  let load_init sigma obj = function
    | Ref _ ->
      e_true
    | Val((CREF|CVAL),x,_) when Cvalues.always_initialized x ->
      Cvalues.initialized_obj obj
    | Val((CREF|CVAL),x,ofs) ->
      access_init (SINIT.value sigma x) ofs
    | Loc l ->
      M.load_init sigma obj l
    | Val((CTXT _|CARR _|HEAP) as m,x,ofs) ->
      M.load_init sigma obj (mloc_of_path m x ofs)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Store                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let memvar_stored kind seq x ofs v =
    let read, update = match kind with
      | KValue -> SVAR.value, update
      | KInit -> SINIT.value, update_init
    in
    let v1 = read seq.pre x in
    let v2 = read seq.post x in
    Set( v2 , update v1 ofs v )

  let gen_stored kind seq obj l v =
    let mstored = match kind with KValue -> M.stored | KInit -> M.stored_init in
    match l with
    | Ref x -> noref ~op:"write to" x
    | Val((CREF|CVAL),x,ofs) ->
      [memvar_stored kind seq x ofs v]
    | Val((CTXT _|CARR _|HEAP) as m,x,ofs) ->
      mstored seq obj (mloc_of_path m x ofs) v
    | Loc l ->
      mstored seq obj l v

  let stored = gen_stored KValue
  let stored_init = gen_stored KInit

  let copied seq obj l1 l2 =
    let v = match load seq.pre obj l2 with
      | Memory.Val r -> r
      | Memory.Loc l -> pointer_val l
    in stored seq obj l1 v

  let copied_init seq obj l1 l2 =
    stored_init seq obj l1 (load_init seq.pre obj l2)

  (* -------------------------------------------------------------------------- *)
  (* ---  Pointer Comparison                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let is_null = function
    | Loc l -> M.is_null l
    | Val ((CTXT Nullable|CARR Nullable)as m,x,ofs) ->
      M.is_null (mloc_of_path m x ofs)
    | Ref _ | Val _ -> F.p_false

  let rec offset = function
    | [] -> e_zero
    | Field f :: ofs ->
      e_add (e_int (Ctypes.field_offset f)) (offset ofs)
    | Shift(obj,k)::ofs ->
      e_add (e_fact (Ctypes.sizeof_object obj) k) (offset ofs)

  let loc_diff obj a b =
    match a , b with
    | Loc l1 , Loc l2 -> M.loc_diff obj l1 l2
    | Ref x , Ref y when Varinfo.equal x y -> e_zero
    | Val(_,x,p) , Val(_,y,q) when Varinfo.equal x y ->
      e_div (e_sub (offset p) (offset q)) (e_int (Ctypes.sizeof_object obj))
    | _ ->
      Warning.error ~source:"Reference Variable Model"
        "Uncomparable locations %a and %a" pretty a pretty b

  let loc_compare lcmp icmp same a b =
    match a , b with
    | Loc l1 , Loc l2 -> lcmp l1 l2
    | Ref x , Ref y ->
      if Varinfo.equal x y then same else p_not same
    | Val(_,x,p) , Val(_,y,q) ->
      if Varinfo.equal x y then icmp (offset p) (offset q) else p_not same
    | (Val _ | Loc _) , (Val _ | Loc _) -> lcmp (mloc_of_loc a) (mloc_of_loc b)
    | Ref _ , (Val _ | Loc _) | (Val _ | Loc _) , Ref _ -> p_not same

  let loc_eq = loc_compare M.loc_eq F.p_equal F.p_true
  let loc_lt = loc_compare M.loc_lt F.p_lt F.p_false
  let loc_leq = loc_compare M.loc_leq F.p_leq F.p_true
  let loc_neq = loc_compare M.loc_neq F.p_neq F.p_false

  (* -------------------------------------------------------------------------- *)
  (* ---  Range & Offset Fits                                               --- *)
  (* -------------------------------------------------------------------------- *)

  exception ShiftMismatch

  let shift_mismatch l =
    Wp_parameters.fatal "Invalid shift : %a" pretty l

  let unsized_array () = Warning.error ~severe:false
      "Validity of unsized-array not implemented yet"

  let fits_inside cond a b n =
    p_leq e_zero a :: p_lt b (e_int n) :: cond

  let fits_off_by_one cond a b n =
    p_leq e_zero a :: p_leq b (e_int n) :: cond

  let stay_outside cond a b n =
    p_lt b e_zero :: p_leq (e_int n) a :: cond

  (* Append conditions to [cond] for [range=(elt,a,b)],
     consisting of [a..b] elements with type [elt] to fits inside the block,
     provided [a<=b]. *)
  let rec block_check fitting cond (block,size) ((elt,a,b) as range) =
    if Ctypes.equal block elt then
      fitting cond a b size
    else
      match Ctypes.get_array block with
      | Some( e , Some n ) -> block_check fitting cond (e , n * size) range
      | Some( _ , None ) -> unsized_array ()
      | None -> raise ShiftMismatch

  (* Append conditions for and array offset (te,k) to fits in obj *)
  let array_check fitting cond te k obj =
    match Ctypes.get_array obj with
    | Some( e , Some n ) when Ctypes.equal e te -> fitting cond k k n
    | Some( _ , None ) -> unsized_array ()
    | _ -> block_check fitting cond (obj,1) (te,k,k)

  (* Append conditions for [offset] to fits [object], provided [a<=b]. *)
  let rec offset_fits cond obj offset =
    match offset with
    | [] -> cond
    | Field fd :: ofs ->
      offset_fits cond (Ctypes.object_of fd.ftype) ofs
    | Shift(te,k) :: ofs ->
      let cond = array_check fits_inside cond te k obj in
      offset_fits cond te ofs

  (* Append conditions to [cond] for [range=(elt,a,b)], starting at [offset],
     consisting of [a..b] elements with type [elt] to fits inside the block,
     of stay outside valid paths, provided [a<=b]. *)
  let rec range_check fitting cond alloc offset ((elt,a,b) as range) =
    match offset with
    | [] -> block_check fitting cond alloc range
    | Field fd :: ofs ->
      range_check fitting cond (Ctypes.object_of fd.ftype,1) ofs range
    | Shift(te,k) :: ofs ->
      if Ctypes.equal te elt then
        range_check fitting cond alloc ofs (elt,e_add a k,e_add b k)
      else
        match Ctypes.get_array (fst alloc) with
        | Some( e , Some n ) when Ctypes.equal e te ->
          let cond = fitting cond k k n in
          range_check fitting cond (e,n) ofs range
        | Some( _ , None ) ->
          unsized_array ()
        | _ ->
          let cond = block_check fitting cond alloc (te,k,k) in
          range_check fitting cond (te,1) ofs range

  (* -------------------------------------------------------------------------- *)
  (* ---  Validity                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let rec last_field_shift acs obj ofs =
    match acs , obj , ofs with
    | OBJ , _ , [Shift(te,k)] -> Some(te,k,obj)
    | OBJ , C_comp c , (Field fd :: ofs) ->
      begin
        match Option.map List.rev c.cfields with
        | Some (fd0::_) when Fieldinfo.equal fd fd0 ->
          last_field_shift acs (Ctypes.object_of fd.ftype) ofs
        | _ -> None
      end
    | _ -> None

  let valid_offset acs obj ofs =
    match last_field_shift acs obj ofs with
    | Some(te,k,obj) ->
      F.p_conj (array_check fits_off_by_one [] te k obj)
    | None ->
      F.p_conj (offset_fits [] obj ofs)

  let valid_range acs obj ofs range =
    match last_field_shift acs obj ofs with
    | Some _ ->
      F.p_conj (range_check fits_off_by_one [] (obj,1) ofs range)
    | _ ->
      F.p_conj (range_check fits_inside [] (obj,1) ofs range)

  (* varinfo *)

  let valid_base sigma acs mem x =
    if x.vglob then
      match acs with
      | RW -> if Cil.typeHasQualifier "const" x.vtype then p_false else p_true
      | RD | OBJ -> p_true
    else
      match mem with
      | CVAL | HEAP -> p_bool (SALLOC.value sigma x)
      | CREF | CTXT Valid | CARR Valid -> p_true
      | CTXT Nullable | CARR Nullable ->
        p_not @@ M.is_null (mloc_of_path mem x [])

  (* segment *)

  let valid_offset_path sigma acs mem x ofs =
    p_and
      (valid_base sigma acs mem x)
      (valid_offset acs (vobject mem x) ofs)

  let valid_range_path sigma acs mem x ofs rg =
    p_and
      (valid_base sigma acs mem x)
      (valid_range acs (vobject mem x) ofs rg)

  (* in-model validation *)

  let valid sigma acs = function
    | Rloc(obj,l) ->
      begin match l with
        | Ref _ -> p_true
        | Loc l -> M.valid sigma acs (Rloc(obj,l))
        | Val(m,x,p) ->
          try valid_offset_path sigma acs m x p
          with ShiftMismatch ->
            if is_heap_allocated m then
              M.valid sigma acs (Rloc(obj,mloc_of_loc l))
            else
              shift_mismatch l
      end
    | Rrange(l,elt,a,b) ->
      begin match l with
        | Ref x -> noref ~op:"valid sub-range of" x
        | Loc l -> M.valid sigma acs (Rrange(l,elt,a,b))
        | Val(m,x,p) ->
          match a,b with
          | Some ka,Some kb ->
            begin
              try
                F.p_imply (F.p_leq ka kb)
                  (valid_range_path sigma acs m x p (elt,ka,kb))
              with ShiftMismatch ->
                if is_heap_allocated m then
                  let l = mloc_of_loc l in
                  M.valid sigma acs (Rrange(l,elt,a,b))
                else shift_mismatch l
            end
          | _ ->
            Warning.error "Validity of infinite range @[%a.(%a..%a)@]"
              pretty l Vset.pp_bound a Vset.pp_bound b
      end

  (* -------------------------------------------------------------------------- *)
  (* ---  Invalidity                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let invalid_range obj ofs range =
    F.p_disj (range_check stay_outside [] (obj,1) ofs range)

  let invalid_offset_path sigma m x p =
    p_not (valid_offset_path sigma RD m x p)

  let invalid_range_path sigma m x p rg =
    p_imply
      (valid_base sigma RD m x)
      (invalid_range (vobject m x) p rg)

  let invalid sigma = function
    | Rloc(obj,l) ->
      begin match l with
        | Ref _ -> p_false
        | Loc l -> M.invalid sigma (Rloc(obj,l))
        | Val(m,x,p) ->
          try invalid_offset_path sigma m x p
          with ShiftMismatch ->
            if is_heap_allocated m then
              M.invalid sigma (Rloc(obj,mloc_of_loc l))
            else
              shift_mismatch l
      end
    | Rrange(l,elt,a,b) ->
      begin match l with
        | Ref x -> noref ~op:"invalid sub-range of" x
        | Loc l -> M.invalid sigma (Rrange(l,elt,a,b))
        | Val(m,x,p) ->
          match a,b with
          | Some ka,Some kb ->
            begin
              try
                F.p_imply (F.p_leq ka kb)
                  (invalid_range_path sigma m x p (elt,ka,kb))
              with ShiftMismatch ->
                if is_heap_allocated m then
                  let l = mloc_of_loc l in
                  M.invalid sigma (Rrange(l,elt,a,b))
                else shift_mismatch l
            end
          | _ ->
            Warning.error "Invalidity of infinite range @[%a.(%a..%a)@]"
              pretty l Vset.pp_bound a Vset.pp_bound b
      end

  (* -------------------------------------------------------------------------- *)
  (* ---  Initialized                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec initialized_loc sigma obj x ofs =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      p_bool (access_init (SINIT.value sigma x) ofs)
    | C_array { arr_flat=flat ; arr_element = te } ->
      let size = match flat with
        | None -> unsized_array ()
        | Some { arr_size } -> arr_size in
      let elt = Ctypes.object_of te in
      initialized_range sigma elt x ofs (e_int 0) (e_int (size-1))
    | C_comp { cfields = None } ->
      Lang.F.p_equal
        (access_init (SINIT.value sigma x) ofs)
        (Cvalues.initialized_obj obj)
    | C_comp { cfields = Some fields } ->
      let init_field fd =
        let obj_fd = Ctypes.object_of fd.ftype in
        let ofs_fd = ofs @ [Field fd] in
        initialized_loc sigma obj_fd x ofs_fd
      in
      Lang.F.p_conj (List.map init_field fields)

  and initialized_range sigma elt x ofs lo up =
    let i = Lang.freshvar ~basename:"i" Lang.t_int in
    let vi = e_var i in
    let ofs = ofs @ [ Shift(elt, vi) ] in
    let range_i = p_and (p_leq lo vi) (p_leq vi up) in
    let init_i = initialized_loc sigma elt x ofs in
    Lang.F.p_forall [i] (p_imply range_i init_i)

  let initialized sigma l =
    match l with
    | Rloc(obj,l) ->
      begin match l with
        | Ref _ -> p_true
        | Loc l -> M.initialized sigma (Rloc(obj,l))
        | Val(m,x,p) ->
          if is_heap_allocated m then
            M.initialized sigma (Rloc(obj,mloc_of_loc l))
          else if Cvalues.always_initialized x then
            try valid_offset RD (vobject m x) p
            with ShiftMismatch -> shift_mismatch l
          else
            initialized_loc sigma obj x p
      end
    | Rrange(l,elt, Some a, Some b) ->
      begin match l with
        | Ref _ -> p_true
        | Loc l -> M.initialized sigma (Rrange(l,elt,Some a, Some b))
        | Val(m,x,p) ->
          try
            if is_heap_allocated m then
              let l = mloc_of_loc l in
              M.initialized sigma (Rrange(l,elt,Some a, Some b))
            else
              let rec normalize obj = function
                | [] -> [], a, b
                | [Shift(elt, i)] when Ctypes.equal obj elt ->
                  [], F.e_add a i, F.e_add b i
                | f :: ofs ->
                  let l, a, b = normalize obj ofs in f :: l, a, b
              in
              let p, a, b = normalize elt p in
              let in_array = valid_range RD (vobject m x) p (elt, a, b) in
              let initialized =
                if Cvalues.always_initialized x then p_true
                else initialized_range sigma elt x p a b
              in
              F.p_imply (F.p_leq a b) (p_and in_array initialized)
          with ShiftMismatch ->
            shift_mismatch l
      end
    | Rrange(l, _,a,b) ->
      Warning.error
        "Initialization of infinite range @[%a.(%a..%a)@]"
        pretty l Vset.pp_bound a Vset.pp_bound b


  (* -------------------------------------------------------------------------- *)
  (* ---  Framing                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let rec forall_pointers phi v t =
    match Cil.unrollTypeNode t with
    | TInt _ | TFloat _ | TVoid | TEnum _ | TNamed _ | TBuiltin_va_list
      -> F.p_true
    | TPtr _ | TFun _ -> phi v
    | TComp { cfields = None } ->
      F.p_true
    | TComp { cfields = Some fields } ->
      F.p_all
        (fun fd ->
           forall_pointers phi (e_getfield v (cfield fd)) fd.ftype)
        fields
    | TArray (elt, _) ->
      let k = Lang.freshvar Qed.Logic.Int in
      F.p_forall [k] (forall_pointers phi (e_get v (e_var k)) elt)

  let frame sigma =
    let hs = ref [] in
    Sigma.iter
      begin fun chunk value ->
        match Sigma.ckind chunk with
        | SVAR.Mu x when not @@ is_ref x ->
          (if (x.vglob || x.vformal) then
             let t = VAR.typ_of_chunk x in
             let v = e_var value in
             let h = forall_pointers (M.global sigma) v t in
             if not (F.eqp h F.p_true) then hs := h :: !hs ) ;
          (if x.vglob then
             let v = e_var value in
             hs := Cvalues.has_ctype x.vtype v :: !hs ) ;
        | _ -> ()
      end sigma ;
    !hs @ M.frame sigma

  (* -------------------------------------------------------------------------- *)
  (* ---  Scope                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let is_mem x = match V.param x with
    | ByAddr -> true
    | _ -> false

  let is_mvar_alloc x =
    match V.param x with
    | ByRef | InContext _ | InArray _ | NotUsed -> false
    | ByValue | ByShift | ByAddr -> true

  let alloc sigma xs =
    let xm = List.filter is_mem xs in
    let sigma = M.alloc sigma xm in
    let xv = List.filter is_mvar_alloc xs in
    let domain = Sigma.Domain.of_list @@ List.map SALLOC.chunk xv in
    Sigma.havoc sigma domain

  let scope_vars seq scope xs =
    let xs = List.filter is_mvar_alloc xs in
    if xs = [] then []
    else
      let t_in = seq.pre in
      let t_out = seq.post in
      let v_in  = match scope with Enter -> e_false | Leave -> e_true in
      let v_out = match scope with Enter -> e_true | Leave -> e_false in
      List.map
        (fun x ->
           F.p_and
             (F.p_equal (SALLOC.value t_in x) v_in)
             (F.p_equal (SALLOC.value t_out x) v_out)
        ) xs

  let scope_init seq scope xs =
    match scope with
    | Leave -> []
    | Enter ->
      let init_status v =
        if v.vdefined || Cvalues.always_initialized v || not@@ is_mvar_alloc v
        then None
        else
          let i = Cvalues.uninitialized_obj (Ctypes.object_of v.vtype) in
          Some (Lang.F.p_equal (access_init (SINIT.value seq.post v) []) i)
      in
      List.filter_map init_status xs

  let is_nullable m v =
    let addr = nullable_address v in
    p_or
      (M.is_null @@ M.pointer_loc addr)
      (p_equal (M.pointer_val @@ M.cvar (vbase m v)) addr)

  let nullable_scope v =
    match V.param v with
    | InContext Nullable -> Some (is_nullable (CTXT Nullable) v)
    | InArray Nullable -> Some (is_nullable (CARR Nullable) v)
    | _ -> None

  let scope seq scope xs =
    let xm = List.filter is_mem xs in
    let hmem = M.scope seq scope xm in
    let hvars = scope_vars seq scope xs in
    let hinit = scope_init seq scope xs in
    let hcontext = List.filter_map nullable_scope xs in
    hvars @ hinit @ hmem @ hcontext

  let global = M.global

  (* -------------------------------------------------------------------------- *)
  (* ---  Havoc along a ranged-path                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec assigned_path
      kind
      (hs : pred list) (* collector of properties *)
      (xs : var list)  (* variable quantifying the assigned location *)
      (ys : var list)  (* variable quantifying others locations *)
      (a : term)  (* pre-term for root + current offset *)
      (b : term)  (* post-term for root + current offset *)
    = function
      | [] -> hs

      (*TODO: optimized version for terminal [Field _] and [Index _] *)

      | Field f :: ofs ->
        let cf = cfield ~kind f in
        let af = e_getfield a cf in
        let bf = e_getfield b cf in
        let hs = assigned_path kind hs xs ys af bf ofs in
        List.fold_left
          (fun hs g ->
             if Fieldinfo.equal f g then hs else
               let cg = cfield ~kind g in
               let ag = e_getfield a cg in
               let bg = e_getfield b cg in
               let eqg = p_forall ys (p_equal ag bg) in
               eqg :: hs
          ) hs
          (* Note: we have field accesses, everything here is thus complete *)
          (Option.get f.fcomp.cfields)

      | Shift(_,e) :: ofs ->
        let y = Lang.freshvar ~basename:"k" Qed.Logic.Int in
        let k = e_var y in
        let ak = e_get a k in
        let bk = e_get b k in
        if List.exists (fun x -> F.occurs x e) xs then
          (* index [e] is covered by [xs]:
             must explore deeper the remaining path. *)
          assigned_path kind hs xs (y::ys) ak bk ofs
        else
          (* index [e] is not covered by [xs]:
             any index different from e is disjoint.
             explore also deeply with index [e]. *)
          let ae = e_get a e in
          let be = e_get b e in
          let ek = p_neq e k in
          let eqk = p_forall (y::ys) (p_imply ek (p_equal ak bk)) in
          assigned_path kind (eqk :: hs) xs ys ae be ofs

  let assigned_genset s xs mem x ofs p =
    let valid = valid_offset_path s.post Memory.RW mem x ofs in
    let a = SVAR.value s.pre x in
    let b = SVAR.value s.post x in
    let a_ofs = access a ofs in
    let b_ofs = access b ofs in
    let p_sloc = p_forall xs (p_hyps [valid;p_not p] (p_equal a_ofs b_ofs)) in
    let conds = assigned_path KValue [p_sloc] xs [] a b ofs in
    List.map (fun p -> Assert p) conds

  (*
  let monotonic_initialized_genset s xs mem x ofs p =
    if always_init x then [Assert p_true]
    else
      let valid = valid_offset_path s.post Memory.RW mem x ofs in
      let a = SINIT.value s.pre x in
      let b = SINIT.value s.post x in
      let a_ofs = access_init a ofs in
      let b_ofs = access_init b ofs in
      let not_p = p_forall xs (p_hyps [valid;p_not p] (p_equal a_ofs b_ofs)) in
      let exact_p =
        p_forall xs (p_hyps [valid; p] (p_imply (p_bool a_ofs) (p_bool b_ofs)))
      in
      let conds = assigned_path KInit [not_p; exact_p] xs [] a b ofs in
      List.map (fun p -> Assert p) conds
  *)

  (* -------------------------------------------------------------------------- *)
  (* ---  Assigned                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let rec monotonic_initialized seq obj x ofs =
    if Cvalues.always_initialized x then p_true
    else
      match obj with
      (* Structure initialization is not monotonic *)
      | C_comp _ -> p_true
      (* Neither is initialization of arrays of structures *)
      | C_array { arr_element=t } when Cil.isStructOrUnionType t -> p_true

      | C_int _ | C_float _ | C_pointer _ ->
        p_imply
          (p_bool (access_init (SINIT.value seq.pre x) ofs))
          (p_bool (access_init (SINIT.value seq.post x) ofs))

      | C_array { arr_flat=flat ; arr_element=t } ->
        let size = match flat with
          | None -> unsized_array ()
          | Some { arr_size } -> arr_size
        in
        let v = Lang.freshvar ~basename:"i" Lang.t_int in
        let obj = Ctypes.object_of t in
        let ofs = ofs @ [ Shift(obj, e_var v) ] in
        let low = Some (e_int 0) in
        let up = Some (e_int (size-1)) in
        let hyp = Vset.in_range (e_var v) low up in
        let in_range = monotonic_initialized seq obj x ofs in
        Lang.F.p_forall [v] (p_imply hyp in_range)

  let assigned_loc seq obj = function
    | Ref x -> noref ~op:"assigns to" x
    | Val((CVAL|CREF),x,[]) ->
      [ Assert (monotonic_initialized seq obj x []) ]
    | Val((CVAL|CREF),x,ofs) ->
      let value = Lang.freshvar ~basename:"v" (tau_of_object obj) in
      memvar_stored KValue seq x ofs (e_var value) ::
      if Cil.isStructOrUnionType x.vtype then []
      else begin
        let init = Lang.freshvar ~basename:"v" (init_of_object obj) in
        Assert(monotonic_initialized seq obj x ofs) ::
        [ memvar_stored KInit seq x ofs (e_var init) ]
      end
    | Val((HEAP|CTXT _|CARR _) as m,x,ofs) ->
      M.assigned seq obj (Sloc (mloc_of_path m x ofs))
    | Loc l ->
      M.assigned seq obj (Sloc l)

  let assigned_array seq obj l elt n =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Val((CVAL|CREF),x,[]) ->
      if Ctypes.is_compound elt then []
      else
        (* Note that 'obj' above corresponds to the elements *)
        let obj = Ctypes.object_of x.vtype in
        [ Assert (monotonic_initialized seq obj x []) ]

    | Val((CVAL|CREF),x,ofs) ->
      let f_array t =
        e_var (Lang.freshvar ~basename:"v" Qed.Logic.(Array(Int, t))) in
      [ memvar_stored KValue seq x ofs (f_array @@ Lang.tau_of_object elt) ;
        memvar_stored KInit  seq x ofs (f_array @@ Lang.init_of_object elt) ]

    | Val((HEAP|CTXT _|CARR _) as m,x,ofs) ->
      let l = mloc_of_path m x ofs in
      M.assigned seq obj (Sarray(l,elt,n))
    | Loc l ->
      M.assigned seq obj (Sarray(l,elt,n))

  let assigned_range seq obj l elt a b =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Loc l ->
      M.assigned seq obj (Srange(l,elt,a,b))
    | Val((HEAP|CTXT _|CARR _) as m,x,ofs) ->
      M.assigned seq obj (Srange(mloc_of_path m x ofs,elt,a,b))
    | Val((CVAL|CREF) as m,x,ofs) ->
      let k = Lang.freshvar ~basename:"k" Qed.Logic.Int in
      let p = Vset.in_range (e_var k) a b in
      let ofs = ofs_shift elt (e_var k) ofs in
      (* (monotonic_initialized_genset seq [k] m x ofs p) @*)
      (assigned_genset seq [k] m x ofs p)

  let assigned_descr seq obj xs l p =
    match l with
    | Ref x -> noref ~op:"assigns to" x
    | Loc l ->
      M.assigned seq obj (Sdescr(xs,l,p))
    | Val((HEAP|CTXT _|CARR _) as m,x,ofs) ->
      M.assigned seq obj (Sdescr(xs,mloc_of_path m x ofs,p))
    | Val((CVAL|CREF) as m,x,ofs) ->
      (* (monotonic_initialized_genset seq xs m x ofs p) @ *)
      (assigned_genset seq xs m x ofs p)

  let assigned seq obj = function
    | Sloc l -> assigned_loc seq obj l
    | Sarray(l,elt,n) -> assigned_array seq obj l elt n
    | Srange(l,elt,a,b) -> assigned_range seq obj l elt a b
    | Sdescr(xs,l,p) -> assigned_descr seq obj xs l p

  (* -------------------------------------------------------------------------- *)
  (* --- Segments                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  type seq =
    | Rseg of varinfo
    | Fseg of varinfo * delta list
    | Mseg of M.loc rloc * varinfo * delta list
    | Lseg of M.loc rloc
  and delta =
    | Dfield of fieldinfo
    | Drange of term option * term option

  let dofs = function
    | Field f -> Dfield f
    | Shift(_,k) -> let u = Some k in Drange(u,u)

  let tofs = function
    | Field d -> Ctypes.object_of d.ftype
    | Shift(elt,_) -> elt

  let rec dstartof dim = function
    | C_array arr ->
      let n = match arr.arr_flat with None -> 1 | Some a -> a.arr_dim in
      if n > dim then
        let u = Some e_zero in
        let elt = Ctypes.object_of arr.arr_element in
        Drange(u,u) :: dstartof dim elt
      else []
    | _ -> []

  let rec doffset obj host = function
    | d::ds -> dofs d :: (doffset obj (tofs d) ds)
    | [] -> dstartof (Ctypes.get_array_dim obj) host

  let delta obj x ofs = doffset obj (Ctypes.object_of x.vtype) ofs

  let rec range ofs obj a b =
    match ofs with
    | [] -> [ Drange(a,b) ]
    | [Shift(elt,k)] when Ctypes.equal elt obj ->
      [ Drange( Vset.bound_shift a k , Vset.bound_shift b k ) ]
    | d :: ofs -> dofs d :: range ofs obj a b

  let locseg = function

    | Rloc(_,Ref x) -> Rseg x
    | Rrange(Ref x,_,_,_) -> noref ~op:"sub-range of" x

    | Rloc(obj,Loc l) -> Lseg (Rloc(obj,l))
    | Rloc(obj,Val((CVAL|CREF),x,ofs)) ->
      Fseg(x,delta obj x ofs)

    | Rrange(Loc l,obj,a,b) -> Lseg (Rrange(l,obj,a,b))
    | Rrange(Val((CVAL|CREF),x,ofs),obj,a,b) ->
      Fseg(x,range ofs obj a b)

    (* Nullable: force M without symbolic access *)
    | Rloc(obj,Val((CTXT Nullable|CARR Nullable) as m,x,ofs)) ->
      Lseg(Rloc(obj, mloc_of_path m x ofs))
    | Rrange(Val((CTXT Nullable|CARR Nullable) as m,x,ofs),obj,a,b) ->
      Lseg(Rrange(mloc_of_path m x ofs, obj, a, b))

    (* Otherwise: use M with symbolic access *)
    | Rloc(obj,Val((CTXT Valid|CARR Valid|HEAP) as m,x,ofs)) ->
      Mseg(Rloc(obj,mloc_of_path m x ofs),x,delta obj x ofs)
    | Rrange(Val((CTXT Valid|CARR Valid|HEAP) as m,x,ofs),obj,a,b) ->
      Mseg(Rrange(mloc_of_path m x ofs,obj,a,b),x,range ofs obj a b)

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Inclusion                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let rec included_delta d1 d2 =
    match d1 , d2 with
    | _ , [] -> p_true
    | [] , _ -> p_false
    | u :: d1 , v :: d2 ->
      match u , v with
      | Dfield f , Dfield g when Fieldinfo.equal f g ->
        included_delta d1 d2
      | Dfield _ , _ | _ , Dfield _ -> p_false
      | Drange(a1,b1) , Drange(a2,b2) ->
        p_conj [ Vset.ordered ~strict:false ~limit:true a2 a1 ;
                 Vset.ordered ~strict:false ~limit:true b1 b2 ;
                 included_delta d1 d2 ]

  let included s1 s2 =
    match locseg s1 , locseg s2 with
    | Rseg x , Rseg y -> if Varinfo.equal x y then p_true else p_false
    | Rseg _ , _ | _ , Rseg _ -> p_false

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
      if Varinfo.equal x1 x2 then included_delta d1 d2 else p_false

    | Fseg _ , _ | _ , Fseg _ -> p_false

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.included s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Separation                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let rec separated_delta d1 d2 =
    match d1 , d2 with
    | [] , _ | _ , [] -> p_false
    | u :: d1 , v :: d2 ->
      match u , v with
      | Dfield f , Dfield g when Fieldinfo.equal f g
        -> separated_delta d1 d2
      | Dfield _ , _ | _ , Dfield _ -> p_true
      | Drange(a1,b1) , Drange(a2,b2) ->
        p_disj [ Vset.ordered ~strict:true ~limit:false b1 a2 ;
                 Vset.ordered ~strict:true ~limit:false b2 a1 ;
                 separated_delta d1 d2 ]

  let separated r1 r2 =
    match locseg r1 , locseg r2 with
    | Rseg x , Rseg y -> if Varinfo.equal x y then p_false else p_true
    | Rseg _ , _ | _ , Rseg _ -> p_true

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
      if Varinfo.equal x1 x2 then separated_delta d1 d2 else p_true
    | Fseg _ , _ | _ , Fseg _ -> p_true

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.separated s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Domain                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let domain obj l =
    match l with
    | Ref x | Val((CVAL|CREF),x,_) ->
      let init =
        if not @@ Cvalues.always_initialized x then [ SINIT.chunk x ] else []
      in
      Sigma.Domain.of_list (SVAR.chunk x :: init)
    | Loc _ | Val((CTXT _|CARR _|HEAP),_,_) ->
      M.domain obj (mloc_of_loc l)

  let is_well_formed sigma =
    let cstrs = ref [] in
    Sigma.iter
      (fun chunk value ->
         match Sigma.ckind chunk with
         | SVAR.Mu x when not @@ is_ref x ->
           cstrs := Cvalues.has_ctype x.vtype (e_var value) :: !cstrs
         | _ -> ())
      sigma ;
    p_conj (M.is_well_formed sigma :: !cstrs)

  (* -------------------------------------------------------------------------- *)

end
