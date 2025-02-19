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
(* --- Memory Model                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Definitions
open Ctypes
open Lang
open Lang.F
open Memory
open Sigma

(* -------------------------------------------------------------------------- *)
(* --- Compound Loader                                                    --- *)
(* -------------------------------------------------------------------------- *)

let cluster () =
  Definitions.cluster ~id:"Compound" ~title:"Memory Compound Loader" ()

module type Model =
sig

  val name : string

  type loc
  val sizeof : c_object -> term
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc

  val to_addr : loc -> term
  val to_region_pointer : loc -> int * term
  val of_region_pointer : int -> c_object -> term -> loc

  val value_footprint: c_object -> loc -> domain
  val init_footprint: c_object -> loc -> domain

  val frames : c_object -> loc -> Chunk.t -> frame list

  val last : sigma -> c_object -> loc -> term

  val memcpy : c_object -> mtgt:term -> msrc:term -> ltgt:loc -> lsrc:loc ->
    length:term -> Chunk.t -> term

  val eqmem_forall :
    c_object -> loc -> Chunk.t -> term -> term -> var list * pred * pred

  val load_int : sigma -> c_int -> loc -> term
  val load_float : sigma -> c_float -> loc -> term
  val load_pointer : sigma -> typ -> loc -> loc

  val store_int : sigma -> c_int -> loc -> term -> Chunk.t * term
  val store_float : sigma -> c_float -> loc -> term -> Chunk.t * term
  val store_pointer : sigma -> typ -> loc -> term -> Chunk.t * term

  val is_init_atom : sigma -> c_object -> loc -> term
  val is_init_range : sigma -> c_object -> loc -> term -> pred
  val set_init_atom : sigma -> c_object -> loc -> term -> Chunk.t * term
  val set_init : c_object -> loc -> length:term ->
    Chunk.t -> current:term -> term

end

module Make (M : Model) =
struct

  let signature ft =
    let s = Sigma.create () in
    let xs = ref [] in
    let cs = ref [] in
    Domain.iter
      (fun c ->
         cs := c :: !cs ;
         xs := (Sigma.get s c) :: !xs ;
      ) ft ;
    List.rev !xs , List.rev !cs , s

  let domain obj loc =
    Domain.union
      (M.value_footprint obj loc)
      (M.init_footprint obj loc)

  let pp_rid fmt r = if r <> 0 then Format.fprintf fmt "_R%03d" r

  (* -------------------------------------------------------------------------- *)
  (* --- Frame Lemmas for Compound Access                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let memories sigma chunks = List.map (Sigma.value sigma) chunks
  let assigned sigma c m chunks =
    List.map
      (fun c0 -> if Chunk.equal c0 c then m else Sigma.value sigma c0)
      chunks

  let frame_lemmas phi obj loc params chunks =
    begin
      let prefix = Fun.debug phi in
      let sigma = Sigma.create () in
      List.iteri
        (fun i chunk ->
           List.iter
             (fun (name,triggers,conditions,m1,m2) ->
                let mem1 = assigned sigma chunk m1 chunks in
                let mem2 = assigned sigma chunk m2 chunks in
                let value1 = e_fun phi (params @ mem1) in
                let value2 = e_fun phi (params @ mem2) in
                let vars1 = F.vars value1 in
                let vars2 = F.vars value2 in
                let l_triggers =
                  if Vars.subset vars1 vars2 then
                    [ (Trigger.of_term value2 :: triggers ) ]
                  else
                  if Vars.subset vars2 vars1 then
                    [ (Trigger.of_term value1 :: triggers ) ]
                  else
                    [ (Trigger.of_term value1 :: triggers );
                      (Trigger.of_term value2 :: triggers ) ]
                in
                let l_name = Format.asprintf "%s_%s_%s%d"
                    prefix name (Chunk.basename_of_chunk chunk) i in
                let l_lemma = F.p_hyps conditions (p_equal value1 value2) in
                Definitions.define_lemma {
                  l_kind = Admit ;
                  l_name ;
                  l_triggers ;
                  l_forall = F.p_vars l_lemma ;
                  l_lemma = l_lemma ;
                  l_cluster = cluster () ;
                }
             ) (M.frames obj loc chunk)
        ) chunks
    end

  (* -------------------------------------------------------------------------- *)
  (* ---  Loader utils                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  module AKEY =
  struct
    type t = int * base * Matrix.t
    and base = I of c_int | F of c_float | P | C of compinfo
    let make r elt ds =
      let base = match elt with
        | C_int i -> I i
        | C_float f -> F f
        | C_pointer _ -> P
        | C_comp c -> C c
        | C_array _ -> raise (Invalid_argument "Wp.EqArray")
      in r, base , ds
    let key = function
      | I i -> Ctypes.i_name i
      | F f -> Ctypes.f_name f
      | P -> "ptr"
      | C c -> Lang.comp_id c
    let key_init = function
      | (I _ | F _ | P) as b -> key b ^ "_init"
      | C c -> Lang.comp_init_id c
    let obj = function
      | I i -> C_int i
      | F f -> C_float f
      | P -> C_pointer Cil_const.voidPtrType
      | C c -> C_comp c
    let tau = function
      | I _ -> Lang.t_int
      | F f -> Lang.t_float f
      | P -> Lang.t_addr ()
      | C c -> Lang.t_comp c
    let tau_init = function
      | I _ | F _ | P -> Lang.t_bool
      | C c -> Lang.t_init c
    let compare (r,a,p) (s,b,q) =
      if r = s then
        let cmp = String.compare (key a) (key b) in
        if cmp <> 0 then cmp else Matrix.compare p q
      else r - s
    let pretty fmt (r,a,ds) =
      Format.fprintf fmt "%s%a%a" (key a) pp_rid r Matrix.pp_suffix_id ds
  end

  module type LOAD_INFO = sig
    val kind : Lang.datakind
    val footprint : c_object -> M.loc -> domain
    val t_comp : compinfo -> Lang.tau
    val t_array : AKEY.base -> Lang.tau
    val comp_id : compinfo -> string
    val array_id : AKEY.base -> string
    val load : sigma -> c_object -> M.loc -> term
  end

  module VALUE_LOAD_INFO = struct
    let kind = KValue
    let footprint = M.value_footprint
    let t_comp = Lang.t_comp
    let t_array = AKEY.tau
    let comp_id = Lang.comp_id
    let array_id = AKEY.key
    let load_rec = ref (fun _ _ _ -> assert false)
    let load sigma = !load_rec sigma
  end

  module INIT_LOAD_INFO = struct
    let kind = KInit
    let footprint = M.init_footprint
    let t_comp = Lang.t_init
    let t_array = AKEY.tau_init
    let comp_id = Lang.comp_init_id
    let array_id = AKEY.key_init
    let load_rec = ref (fun _ _ _ -> assert false)
    let load sigma = !load_rec sigma
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Compound Loader                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  module COMP_KEY =
  struct
    type t = int * compinfo
    let compare (r,c) (r',c') = if r=r' then Compinfo.compare c c' else r-r'
    let pretty fmt (r,c) = Format.fprintf fmt "%d:%a" r Compinfo.pretty c
  end

  module COMP_GEN (Info : LOAD_INFO) = WpContext.Generator(COMP_KEY)
      (struct
        let name = M.name ^ ".COMP" ^ (if Info.kind = KInit then "INIT" else "")
        type key = int * compinfo
        type data = lfun * chunk list

        let generate (r,c) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = C_comp c in
          let loc = M.of_region_pointer r obj v in (* t_pointer -> loc *)
          let domain = Info.footprint obj loc in
          let result = Info.t_comp c in
          let lfun =
            Lang.generated_f ~context:true ~result "Load%a_%s"
              pp_rid r (Info.comp_id c) in
          let xms,chunks,sigma = signature domain in
          let prms = x :: xms in
          let dfun =
            match c.cfields with
            | None -> Definitions.Logic result
            | Some fields ->
              let def = List.map
                  (fun f ->
                     let fd = cfield ~kind:Info.kind f in
                     let ft = object_of f.ftype in
                     let fv = Info.load sigma ft (M.field loc f) in
                     let pr = F.e_apply (F.e_lambda prms fv) in
                     F.set_builtin_field lfun fd pr ;
                     fd,fv
                  ) fields
              in Definitions.Function( result , Def , e_record def )
          in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = prms ;
            d_definition = dfun ;
            d_cluster = cluster () ;
          } ;
          frame_lemmas lfun obj loc [v] chunks ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module COMP = COMP_GEN(VALUE_LOAD_INFO)
  module COMP_INIT = COMP_GEN(INIT_LOAD_INFO)

  (* -------------------------------------------------------------------------- *)
  (* ---  Array Loader                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  module ARRAY_GEN(Info: LOAD_INFO) = WpContext.Generator(AKEY)
      (struct
        open Matrix
        let name = M.name ^ ".ARRAY" ^ (if Info.kind=KInit then "INIT" else "")
        type key = AKEY.t
        type data = lfun * chunk list

        let generate (r,a,ds) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = AKEY.obj a in
          let loc = M.of_region_pointer r obj v in (* t_pointer -> loc *)
          let domain = Info.footprint obj loc in
          let result = Matrix.cc_tau (Info.t_array a) ds in
          let lfun =
            Lang.generated_f ~result ~context:true "Array%a_%s%a"
              pp_rid r (Info.array_id a) Matrix.pp_suffix_id ds in
          let prefix = Lang.Fun.debug lfun in
          let name = prefix ^ "_access" in
          let xms,chunks,sigma = signature domain in
          let env = Matrix.cc_env ds in
          let prms = x :: env.size_var @ xms in
          let phi = e_fun lfun (v :: env.size_val @ List.map e_var xms) in
          let va = List.fold_left e_get phi env.index_val in
          let ofs = e_sum env.index_offset in
          let vm = Info.load sigma obj (M.shift loc obj ofs) in
          let lemma = p_hyps env.index_range (p_equal va vm) in
          let cluster = cluster () in
          Definitions.define_symbol {
            d_lfun = lfun ;
            d_types = 0 ;
            d_params = prms ;
            d_definition = Logic result ;
            d_cluster = cluster ;
          } ;
          Definitions.define_lemma {
            l_kind = Admit ;
            l_name = name ;
            l_forall = F.p_vars lemma ;
            l_triggers = [[Trigger.of_term va]] ;
            l_lemma = lemma ;
            l_cluster = cluster ;
          } ;
          let pr = F.e_lambda (prms @ env.index_var) vm in
          let nk = List.length env.index_var in
          Lang.F.set_builtin_get lfun
            (fun es ks ->
               if List.length ks = nk then
                 F.e_apply pr (es @ ks)
               else
                 raise Not_found
            ) ;
          if env.length <> None then
            begin
              let ns = List.map F.e_var env.size_var in
              frame_lemmas lfun obj loc (v::ns) chunks
            end ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module ARRAY = ARRAY_GEN(VALUE_LOAD_INFO)
  module ARRAY_INIT = ARRAY_GEN(INIT_LOAD_INFO)

  (* -------------------------------------------------------------------------- *)
  (* --- Loaders                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  module LOADER_GEN
      (ATOM: sig
         val load_int : sigma -> c_int -> M.loc -> term
         val load_float : sigma -> c_float -> M.loc -> term
         val load_pointer : sigma -> typ -> M.loc -> term
       end)
      (COMP: sig val get : (int*compinfo) -> (lfun * chunk list) end)
      (ARRAY: sig val get : (int*AKEY.base*Matrix.t) -> (lfun * chunk list) end)
  = struct
    let load_comp sigma comp loc =
      let r , p = M.to_region_pointer loc in
      let f , m = COMP.get (r,comp) in
      F.e_fun f (p :: memories sigma m)

    let load_array sigma a loc =
      let r , p = M.to_region_pointer loc in
      let e , ns = Ctypes.array_dimensions a in
      let ds = Matrix.of_dims ns in
      let f , m = ARRAY.get @@ AKEY.make r e ds in
      F.e_fun f (p :: Matrix.cc_dims ns @ memories sigma m)

    let load sigma obj loc =
      match obj with
      | C_int i -> ATOM.load_int sigma i loc
      | C_float f -> ATOM.load_float sigma f loc
      | C_pointer t -> ATOM.load_pointer sigma t loc
      | C_comp c -> load_comp sigma c loc
      | C_array a -> load_array sigma a loc
  end

  module VALUE_LOADER =
    LOADER_GEN
      (struct
        let load_int = M.load_int
        let load_float = M.load_float
        let load_pointer sigma t loc =
          snd @@ M.to_region_pointer @@ M.load_pointer sigma t loc
      end)
      (COMP)(ARRAY)

  let load_comp = VALUE_LOADER.load_comp
  let load_array = VALUE_LOADER.load_array
  let load_value = VALUE_LOADER.load

  let () = VALUE_LOAD_INFO.load_rec := load_value

  let load sigma obj loc =
    let open Memory in
    match obj with
    | C_int i -> Val (M.load_int sigma i loc)
    | C_float f -> Val (M.load_float sigma f loc)
    | C_pointer t -> Loc (M.load_pointer sigma t loc)
    | C_comp c -> Val (load_comp sigma c loc)
    | C_array a -> Val (load_array sigma a loc)

  (* -------------------------------------------------------------------------- *)
  (* --- Initialized                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let isinitrec = ref (fun _ _ _ -> assert false)

  module IS_INIT_COMP = WpContext.Generator(COMP_KEY)
      (struct
        let name = M.name ^ ".IS_INIT_COMP"
        type key = int * compinfo
        type data = lfun * chunk list

        let generate (r,c) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let obj = C_comp c in
          let loc = M.of_region_pointer r obj (e_var x) in
          let domain = M.init_footprint obj loc in
          let cluster = cluster () in
          (* Is_init: structural definition *)
          let name =
            Format.asprintf "Is%s%a" (Lang.comp_init_id c) pp_rid r
          in
          let lfun = Lang.generated_p name in
          let xms,chunks,sigma = signature domain in
          let params = x :: xms in
          let def = match c.cfields with
            | None -> Logic Lang.t_prop
            | Some fields ->
              let def = p_all
                  (fun f -> !isinitrec sigma (object_of f.ftype) (M.field loc f))
                  fields
              in
              Predicate(Def, def)
          in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = params ;
            d_definition = def ;
            d_cluster = cluster ;
          } ;
          (* Is_init: full-range definition *)
          let is_init_p = p_call lfun (List.map e_var (x :: xms)) in
          let is_init_r = M.is_init_range sigma obj loc e_one in
          let lemma = p_equiv is_init_p is_init_r in
          Definitions.define_lemma {
            l_kind = Admit ;
            l_name = name ^ "_range" ;
            l_forall = params ;
            l_triggers = [] ;
            l_lemma = lemma ;
            l_cluster = cluster ;
          } ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module IS_ARRAY_INIT = WpContext.Generator(AKEY)
      (struct
        open Matrix
        let name = M.name ^ ".IS_ARRAY_INIT"
        type key = AKEY.t
        type data = lfun * chunk list

        let generate (r,a,ds) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = AKEY.obj a in
          let loc = M.of_region_pointer r obj v in
          let domain = M.init_footprint obj loc in
          let name = Format.asprintf "IsInitArray%a_%s%a"
              pp_rid r (AKEY.key a) Matrix.pp_suffix_id ds in
          let lfun = Lang.generated_p name in
          let xmem,chunks,sigma = signature domain in
          let env = Matrix.cc_env ds in
          let params = x :: env.size_var @ xmem in
          let ofs = e_sum env.index_offset in
          let vm = !isinitrec sigma obj (M.shift loc obj ofs) in
          let def = p_forall env.index_var (p_hyps env.index_range vm) in
          let cluster = cluster () in
          (* Is_init: structural definition *)
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = params ;
            d_definition = Predicate (Def, def) ;
            d_cluster = cluster ;
          } ;
          (* Is_init: range definition *)
          begin match env.length with None -> () | Some len ->
            let is_init_p = p_call lfun (List.map e_var params) in
            let is_init_r = M.is_init_range sigma obj loc len in
            let lemma = p_equiv is_init_p is_init_r in
            Definitions.define_lemma {
              l_kind = Admit ;
              l_name = name ^ "_range" ;
              l_forall = params ;
              l_triggers = [] ;
              l_lemma = lemma ;
              l_cluster = cluster ;
            }
          end ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  let initialized_comp sigma comp loc =
    let r , p = M.to_region_pointer loc in
    let f , m = IS_INIT_COMP.get (r,comp) in
    F.p_call f (p :: memories sigma m)

  let initialized_array sigma ainfo loc =
    let r , p = M.to_region_pointer loc in
    let e , ns = Ctypes.array_dimensions ainfo in
    let ds = Matrix.of_dims ns in
    let f , m = IS_ARRAY_INIT.get @@ AKEY.make r e ds in
    F.p_call f (p :: Matrix.cc_dims ns @ memories sigma m)

  let initialized_loc sigma obj loc =
    match obj with
    | C_int _ | C_float _ | C_pointer _ -> p_bool (M.is_init_atom sigma obj loc)
    | C_comp ci -> initialized_comp sigma ci loc
    | C_array a -> initialized_array sigma a loc

  let () = isinitrec := initialized_loc

  let initialized sigma = function
    | Rloc(obj, loc) -> initialized_loc sigma obj loc
    | Rrange(loc, obj, Some low, Some up) ->
      let x = Lang.freshvar ~basename:"i" Lang.t_int in
      let v = e_var x in
      let hyps = [ p_leq low v ; p_leq v up] in
      let loc = M.shift loc obj v in
      p_forall [x] (p_hyps hyps (initialized_loc sigma obj loc))
    | Rrange(_l, _, low, up) ->
      Wp_parameters.abort ~current:true
        "Invalid infinite range @[<hov 2>+@,(%a@,..%a)@]"
        Vset.pp_bound low Vset.pp_bound up

  module INIT_LOADER =
    LOADER_GEN
      (struct
        let load_int sigma ikind = M.is_init_atom sigma (C_int ikind)
        let load_float sigma fkind = M.is_init_atom sigma (C_float fkind)
        let load_pointer sigma typ = M.is_init_atom sigma (C_pointer typ)
      end)(COMP_INIT)(ARRAY_INIT)

  let load_init = INIT_LOADER.load
  let () = INIT_LOAD_INFO.load_rec := load_init

  (* -------------------------------------------------------------------------- *)
  (* --- Mem Copies \ Havocs                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let gen_memcpy_length get_domain s obj ?lsrc loc length =
    let lsrc = if Wp_parameters.Havoc.get () then None else lsrc in
    let ps = ref [] in
    Domain.iter
      (fun chunk ->
         let pre = Sigma.value s.pre chunk in
         let post = Sigma.value s.post chunk in
         let tau = Chunk.tau_of_chunk chunk in
         let updated =
           match lsrc with
           | None ->
             let basename = Chunk.basename_of_chunk chunk ^ "_undef" in
             let fresh = F.e_var (Lang.freshvar ~basename tau) in
             M.memcpy obj ~mtgt:pre ~msrc:fresh ~ltgt:loc ~lsrc:loc ~length chunk
           | Some lsrc ->
             M.memcpy obj ~mtgt:pre ~msrc:pre ~ltgt:loc ~lsrc ~length chunk
         in ps := Set(post,updated) :: !ps
      ) (get_domain obj loc) ; !ps

  let memcpy_length = gen_memcpy_length M.value_footprint
  let memcpy seq obj ?lsrc ltgt = memcpy_length seq obj ltgt ?lsrc F.e_one

  let memcpy_init_length = gen_memcpy_length M.init_footprint
  let memcpy_init seq obj ?lsrc ltgt = memcpy_init_length seq obj ltgt ?lsrc F.e_one

  (* -------------------------------------------------------------------------- *)
  (* --- Stored & Copied                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let updated_init_atom seq obj loc value =
    let chunk_init,mem_init = M.set_init_atom seq.pre obj loc value in
    Set(Sigma.value seq.post chunk_init,mem_init)

  let updated_atom seq obj loc value =
    let phi_store sigma = match obj with
      | C_int i -> M.store_int sigma i
      | C_float f -> M.store_float sigma f
      | C_pointer ty -> M.store_pointer sigma ty
      | _ -> failwith "MemLoader updated_atom called on a non atomic type"
    in
    let chunk_store,mem_store = phi_store seq.pre loc value in
    Set(Sigma.value seq.post chunk_store,mem_store)

  let stored seq obj loc value =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      [ updated_atom seq obj loc value ]
    | C_comp _ | C_array _ ->
      Set(load_value seq.post obj loc, value) :: memcpy seq obj loc

  let stored_init seq obj loc value =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      [ updated_init_atom seq obj loc value ]
    | C_comp _ | C_array _ ->
      Set(load_init seq.post obj loc, value) :: memcpy_init seq obj loc

  let copied s obj p q =
    if Wp_parameters.Havoc.get () then
      stored s obj p (load_value s.pre obj q)
    else match obj with
      | C_int _ | C_float _ | C_pointer _ ->
        stored s obj p (load_value s.pre obj q)
      | C_comp _ | C_array _ -> memcpy s obj ~lsrc:q p


  let copied_init s obj p q =
    if Wp_parameters.Havoc.get () then
      stored_init s obj p (load_init s.pre obj q)
    else match obj with
      | C_int _ | C_float _ | C_pointer _ ->
        stored_init s obj p (load_init s.pre obj q)
      | C_comp _ | C_array _ -> memcpy_init s obj ~lsrc:q p

  (* -------------------------------------------------------------------------- *)
  (* --- Assigned                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let assigned_loc seq obj loc =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      let value = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
      let init = Lang.freshvar ~basename:"i" (Lang.init_of_object obj) in
      [ updated_init_atom seq obj loc (e_var init) ;
        updated_atom seq obj loc (e_var value) ]
    | C_comp _ | C_array _ ->
      if Wp_parameters.Havoc.get () then
        memcpy seq obj loc @ memcpy_init seq obj loc
      else memcpy seq obj ~lsrc:loc loc @ memcpy_init seq obj loc

  let assigned_range s obj l a b =
    let loc = M.shift l obj a in
    memcpy_length s obj loc (e_range a b) @
    memcpy_init_length s obj loc (e_range a b)

  let assigned seq obj sloc =
    (* Assert (M.monotonic_init seq.pre seq.post) :: *)
    match sloc with
    | Sloc loc -> assigned_loc seq obj loc
    | Sdescr(xs,loc,condition) ->
      let ps = ref [] in
      Domain.iter
        (fun c ->
           let m1 = Sigma.value seq.pre c in
           let m2 = Sigma.value seq.post c in
           let p,separated,equal = M.eqmem_forall obj loc c m1 m2 in
           let sep_from_all = F.p_forall xs (F.p_imply condition separated) in
           let phi = F.p_forall p (F.p_imply sep_from_all equal) in
           ps := Assert phi :: !ps
        ) (domain obj loc) ;
      !ps
    | Sarray(loc,obj,n) ->
      assigned_range seq obj loc e_zero (e_int (n-1))
    | Srange(loc,obj,u,v) ->
      let a = match u with Some a -> a | None -> e_zero in
      let b = match v with Some b -> b | None -> M.last seq.pre obj loc in
      assigned_range seq obj loc a b

  (* -------------------------------------------------------------------------- *)

end
