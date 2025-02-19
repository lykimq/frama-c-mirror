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

(* ************************************************************************** *)
(* Misc functions *)
(* ************************************************************************** *)

(* return true if the string s starts with prefix p and false otherwise *)
let startswith p s =
  let lp = String.length p in
  if lp <= String.length s then
    p = String.sub s 0 lp
  else
    false

(* if string s is prefixed with string p, then return s without p, otherwise
 * return s as is *)
let strip_prefix p s =
  let lp = String.length p in
  if startswith p s then
    String.sub s lp (String.length s - lp)
  else
    s

(* True if a named function has a definition and false otherwise *)
let has_fundef exp = match exp.enode with
  | Lval(Var vi, _) ->
    let kf =
      try Globals.Functions.get vi
      with Not_found -> Options.fatal "[has_fundef] not a function"
    in
    Kernel_function.is_definition kf
  | Lval _ (* function pointer *) ->
    false
  | _ ->
    Options.fatal "[has_fundef] not a left-value: '%a'" Printer.pp_exp exp

(* ************************************************************************** *)
(* RTL functions *)
(* ************************************************************************** *)

module RTL = struct

  (* prefix of all functions/variables from the public E-ACSL API *)
  let api_prefix = "__e_acsl_"

  (* prefix of temporal analysis functions of the public E-ACSL API *)
  let temporal_prefix = api_prefix ^ "temporal_"

  (* prefix of functions/variables generated by E-ACSL *)
  let e_acsl_gen_prefix = "__gen_e_acsl_"

  let mk_api_name fname = api_prefix ^ fname

  let is_generated_name name = startswith e_acsl_gen_prefix name

  let mk_gen_name name =
    if is_generated_name name then name else e_acsl_gen_prefix ^ name

  let get_original_name kf =
    strip_prefix e_acsl_gen_prefix (Kernel_function.get_name kf)

  let is_generated_kf kf =
    is_generated_name (Kernel_function.get_name kf)
end

(* ************************************************************************** *)
(* Libc functions *)
(* ************************************************************************** *)

module Libc = struct

  (* prefix of all builtin functions/variables from the public E-ACSL API,
     Builtin functions replace original calls in programs. *)
  let e_acsl_builtin_prefix =  RTL.api_prefix ^ "builtin_"

  let has_replacement = function
    | "strcpy"  | "strncpy" | "strlen" | "strcat" | "strncat" | "strcmp"
    | "strncmp" | "memcpy"  | "memset" | "memcmp" | "memmove" -> true
    | _ -> false

  let replacement_name fn = e_acsl_builtin_prefix ^ fn

  let is_vla_alloc_name name = name = "__fc_vla_alloc"
  let is_vla_free_name name = name = "__fc_vla_free"

  let actual_alloca = "__builtin_alloca"

  let is_memcpy_name name = name = "memcpy"
  let is_memset_name name = name = "memset"

  let apply_fn f exp = match exp.enode with
    | Lval(Var vi, _) -> f vi.vname
    | Lval _  (* function pointer *) -> false
    | _ -> Options.fatal "[Functions.Rtl.apply_fn] not a left-value"

  let is_vla_free exp = apply_fn is_vla_free_name exp
  let is_memcpy exp = apply_fn is_memcpy_name exp
  let is_memset exp = apply_fn is_memset_name exp

  let printf_fmt_position = function
    | "printf" -> 1
    | "syslog" | "dprintf" | "fprintf" | "sprintf" -> 2
    | "snprintf" -> 3
    | _ -> 0

  let is_printf_name name = printf_fmt_position name <> 0

  let get_printf_argument_str ~loc fn args =
    assert (is_printf_name fn);
    (* drop first n elements from a list *)
    let rec drop n l =
      assert (n >= 0);
      if n > 0 then
        let l = match l with _ :: e -> e | [] -> [] in
        drop (n-1) l
      else
        l
    in
    (* get a character representing an integer type *)
    let get_ikind_str = function
      | IInt -> "d" (* [int] *)
      | IUInt -> "D" (* [unsigned int] *)
      | ILong -> "l" (* [long] *)
      | IULong -> "L" (* [unsigned long] *)
      | ILongLong -> "r" (* [long long] *)
      | IULongLong -> "R" (* [unsigned long long] *)
      (* _Bool, char and short (either signed or unsigned are promoted to
         int) *)
      | IBool | IChar | ISChar | IUChar | IShort | IUShort -> "d"
    in
    (* get a character representing a floating point type *)
    let get_fkind_str = function
      (* Format-based functions expect only double-precision floats.
         Single-precision floating points are promoted to doubles so
         this case should never happen in fact. *)
      | FFloat -> assert false (* "f" *) (* [float] *)
      | FDouble  -> "e" (* [float/double] *)
      | FLongDouble -> "E" (* [long double] *)
    in
    (* get a character representing a pointer type *)
    let get_pkind_str a ty = match ty.tnode with
      | TInt IChar | TInt ISChar -> "s" (* [char*] *)
      | TInt IUChar -> "S" (* [unsigned char*] *)
      | TInt IShort -> "q" (* [short*] *)
      | TInt IUShort -> "Q" (* [unsigned short*] *)
      | TInt IInt -> "i" (* [int*] *)
      | TInt IUInt -> "I" (* [unsigned int*] *)
      | TInt ILong -> "z" (* [long int*] *)
      | TInt IULong -> "Z" (* [unsigned long int*] *)
      | TInt ILongLong -> "w" (* [long int*] *)
      | TInt IULongLong -> "W" (* [unsigned long int*] *)
      | TVoid -> "p" (* [void*] *)
      | _ ->
        Options.fatal "unexpected argument type in printf: type %a of arg %a@."
          Printer.pp_typ ty
          Printer.pp_exp a
    in
    let exps = drop (printf_fmt_position fn) args in
    let param_str =
      List.fold_right
        (fun exp acc -> match Cil.(unrollTypeNode (typeOf exp)) with
           | TInt k -> get_ikind_str k ^ acc
           | TFloat k -> get_fkind_str k ^ acc
           | TPtr ty -> get_pkind_str exp (Cil.unrollType ty) ^ acc
           | TVoid | TArray _ | TFun _ | TNamed _ | TComp _ | TEnum _
           | TBuiltin_va_list -> assert false)
        exps
        ""
    in
    Cil.mkString ~loc param_str

end

module Concurrency = struct
  let has_replacement fn =
    match fn with
    | "pthread_create" -> true
    | _ -> false

  let replacement_name fn = RTL.api_prefix ^ fn
end

let check kf =
  (* [kf] is monitored iff all functions must be monitored or [kf] belongs to
     the white list *)
  Options.Functions.is_empty ()
  || Options.Functions.mem kf
  ||
  (* also check if [kf] is a duplicate of a monitored function *)
  let s = RTL.get_original_name kf in
  try
    let gen_kf = Globals.Functions.find_by_name s in
    Options.Functions.mem gen_kf
  with Not_found ->
    false

let instrument kf =
  (* [kf] is monitored iff all functions must be monitored or [kf] belongs to
     the white list *)
  Options.Instrument.is_empty ()
  ||
  (Options.Instrument.mem kf
   &&
   (not (RTL.is_generated_kf kf)
    ||
    (* all duplicates belong to [Options.Instrument]. For them, look for
       their original version. *)
    let s = RTL.get_original_name kf in
    try
      let gen_kf = Globals.Functions.find_by_name s in
      Options.Instrument.mem gen_kf
    with Not_found ->
      false))

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
