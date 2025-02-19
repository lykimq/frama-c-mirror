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

type path = {
  hash : int ;
  path_name : string ;
  base_name : string ; (* Filename.basename *)
  dir : path option ; (* path whose path_name is Filename.dirname *)
  mutable symbolic_name : string option ; (* Symbolic name *)
}

let dummy = {
  path_name = "";
  hash = 0;
  base_name = ".";
  dir = None;
  symbolic_name = None
}

(* re_drive and re_root match drive expressions to deal with non-Cygwin
   Windows-like paths (e.g. with MinGW) *)
let re_drive = Str.regexp "[A-Za-z]:"
let re_path = Str.regexp "[/\\\\]"
let re_root = Str.regexp "/\\|\\([A-Za-z]:\\\\\\)\\|\\([A-Za-z]:/\\)"

(* -------------------------------------------------------------------------- *)
(* --- Path Indexing                                                      --- *)
(* -------------------------------------------------------------------------- *)

(* Can not use Weak, since the internal [path] representation is not returned.
   Can not use a weak-cache because each minor GC
   may empty the cache (see #191). *)

module HPath =
struct
  module H = Hashtbl.Make
      (struct
        type t = path
        let hash p = p.hash
        let equal p q = p.path_name = q.path_name
      end)
  let find = H.find
  let create = H.create
  let merge h p = try H.find h p with Not_found -> H.add h p p ; p
end

let hcons = HPath.create 128
let cache = Array.make 256 None

let root path_name =
  HPath.merge hcons { dummy with path_name ; hash = Hashtbl.hash path_name }

let make dir base_name =
  let path_name = Printf.sprintf "%s/%s" dir.path_name base_name in
  let hash = Hashtbl.hash path_name in
  HPath.merge
    hcons
    { dummy with
      path_name;
      hash;
      base_name = base_name;
      dir = Some dir
    }

let getdir path =
  match path.dir with
  | None -> dummy (* the parent of the root directory is itself *)
  | Some d -> d

let rec norm path = function
  | [] -> path
  | ".."::ps -> norm (getdir path) ps
  | "."::ps -> norm path ps
  | p::ps -> norm (make path p) ps

let insert base path_name =
  let full_path_name =
    (* if a <base> is provided while a <file> which is already absolute
       (and thus matches [re_root]) then the <base> is not taken
       into account *)
    if Str.string_match re_root path_name 0
    then path_name
    else base.path_name ^ "/" ^ path_name in
  let hash = Hashtbl.hash full_path_name in
  match Array.get cache (hash land 255) with
  | Some (pn, p) when full_path_name = pn -> p
  | _ ->
    let p = { dummy with path_name = full_path_name; hash } in
    try HPath.find hcons p
    with Not_found ->
      let base =
        (* if a <base> is provided while a <file> is already absolute
           (and thus matches [re_root]) then the <base> is not taken
           into account *)
        if Str.string_match re_root path_name 0
        then root (String.sub path_name 0 (Str.group_end 0 - 1))
        else base in
      let name_parts = Str.split re_path path_name in
      (* Windows paths may start with '<drive>:'. If so, remove it *)
      let parts = if List.length name_parts > 0 &&
                     Str.string_match re_drive (List.nth name_parts 0) 0 then
          List.tl name_parts
        else name_parts
      in
      let path = norm base parts in
      Array.set cache (hash land 255) (Some (path_name, path));
      path

(* Note: the call to Unix.realpath prevents some issues with symbolic links
   in directory names. If you have problems with this, please contact us.
   For the same reason, Sys.getcwd should _not_ be called directly, but only
   via this function, to avoid conflicting results in case the user forgot
   to call Unix.realpath.
*)
let pwd () = Unix.(realpath (getcwd ()))

let cwd = insert dummy (pwd ())

type existence =
  | Must_exist
  | Must_not_exist
  | Indifferent

exception No_file
exception File_exists

let normalize ?(existence=Indifferent) ?base_name path_name =
  let path =
    if path_name = ""
    then ""
    else
      let base =
        match base_name with
        | None -> cwd
        | Some b -> insert cwd b
      in
      let norm_path_name = (insert base path_name).path_name in
      if norm_path_name = ""
      then "/"
      else norm_path_name
  in
  match existence with
  | Indifferent ->
    path
  | Must_exist ->
    if Sys.file_exists path
    then path
    else raise No_file
  | Must_not_exist ->
    if Sys.file_exists path
    then raise File_exists
    else path

(* -------------------------------------------------------------------------- *)
(* --- Symboling Names                                                    --- *)
(* -------------------------------------------------------------------------- *)

(* Note: Symbolic directories are not currently projectified *)
let symbolic_dirs = Hashtbl.create 3

let add_symbolic_dir name dir =
  Hashtbl.replace symbolic_dirs dir name ;
  (insert cwd (dir:>string)).symbolic_name <- Some name

(** Initialize using Config *)
let add_symbolic_dir_list name =
  List.iter (fun d -> add_symbolic_dir name d)

let reset_symbolic_dirs () = Hashtbl.clear symbolic_dirs

let all_symbolic_dirs () =
  let compare (s1, s1') (s2, s2') =
    let c = String.compare s1 s2 in
    if c <> 0 then c
    else String.compare s1' s2'
  in
  List.sort compare @@
  Hashtbl.fold (fun dir name acc -> (name, dir) :: acc) symbolic_dirs []

let rec add_uri_path buffer path =
  let open Buffer in
  match path.symbolic_name with
  | None ->
    begin
      match path.dir with
      | None -> add_string buffer path.path_name; None
      | Some d ->
        if d != cwd (* hconsed *) then begin
          let symb_base = add_uri_path buffer d in
          add_char buffer '/';
          add_string buffer path.base_name;
          symb_base
        end else begin
          add_string buffer path.base_name;
          Some "PWD"
        end
    end
  | Some sn -> Some sn

let add_path path =
  let buf = Buffer.create 80 in
  match add_uri_path buf path with
  | None -> Buffer.contents buf
  | Some "PWD" -> Buffer.contents buf
  | Some symb -> symb ^ Buffer.contents buf

let rec skip_dot file_name =
  if String.starts_with ~prefix:"./" file_name then
    skip_dot (String.sub file_name 2 (String.length file_name - 2))
  else file_name

let pretty file_name =
  if file_name = "" then
    "<unknown location>"
  else if Filename.is_relative file_name then
    skip_dot file_name
  else
    let path = insert cwd file_name in
    skip_dot (add_path path)

(* -------------------------------------------------------------------------- *)
(* --- Relative Paths                                                     --- *)
(* -------------------------------------------------------------------------- *)

let relativize ?base_name file_name =
  let file_name = (insert cwd file_name).path_name in
  let base_name = match base_name with
    | None -> cwd.path_name
    | Some b -> (insert cwd b).path_name
  in
  if base_name = file_name then "." else
    let base_name = base_name ^ Filename.dir_sep in
    if String.starts_with ~prefix:base_name file_name then
      let n = String.length base_name in
      let file_name = String.sub file_name n (String.length file_name - n) in
      if file_name = "" then "." else file_name
    else file_name

let is_relative ?base_name file_name =
  let file_name = (insert cwd file_name).path_name in
  let base_name = match base_name with
    | None -> cwd.path_name
    | Some b -> (insert cwd b).path_name
  in
  base_name = file_name
  || String.starts_with ~prefix:(base_name ^ Filename.dir_sep) file_name

(* -------------------------------------------------------------------------- *)
(* --- Normalized Typed Module                                            --- *)
(* -------------------------------------------------------------------------- *)

module Normalized = struct
  type t = string

  let of_string ?existence ?base_name s = normalize ?existence ?base_name s
  let extend ?existence t ext = normalize ?existence (t ^ ext)
  let concat ?existence t s = normalize ?existence (t ^ "/" ^ s)
  let concats ?existence t sl =
    let s' = List.fold_left (fun acc s -> acc ^ "/" ^ s) "" sl in
    normalize ?existence (t ^ s')
  let to_pretty_string s = pretty s
  let to_string_list l = l
  let equal : t -> t -> bool = (=)
  let compare = String.compare

  let compare_pretty ?(case_sensitive=false) s1 s2 =
    let s1 = pretty s1 in
    let s2 = pretty s2 in
    if case_sensitive then String.compare s1 s2
    else
      String.compare
        (String.lowercase_ascii s1)
        (String.lowercase_ascii s2)

  let empty = normalize ""
  let is_empty fp = equal fp empty
  let special_stdout = normalize "-"
  let is_special_stdout fp = equal fp special_stdout

  let pretty fmt p =
    if is_special_stdout p then
      Format.fprintf fmt "<stdout>"
    else if is_empty p then
      Format.fprintf fmt "<unknown location>"
    else
      Format.fprintf fmt "%s" (pretty p)
  let pp_abs fmt p = Format.fprintf fmt "%s" p
  let is_file fp =
    try
      (Unix.stat (fp :> string)).Unix.st_kind = Unix.S_REG
    with _ -> false

  let to_base_uri name =
    let p = insert cwd name in
    let buf = Buffer.create 80 in
    let res = add_uri_path buf p in
    let uri =
      Buffer.contents buf in
    let uri =
      try
        if String.get uri 0 = '/' then
          String.sub uri 1 (String.length uri - 1)
        else uri
      with Invalid_argument _ -> uri
    in
    res, uri
end

type position =
  {
    pos_path : Normalized.t;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

let empty_pos = {
  pos_path = Normalized.empty;
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = -1;
}

let pp_pos fmt pos =
  let path = pos.pos_path in
  if Normalized.(is_empty path || is_special_stdout path) then
    Format.fprintf fmt "%a" Normalized.pretty path
  else
    Format.fprintf fmt "%a:%d" Normalized.pretty path pos.pos_lnum

let is_empty_pos pos = pos == empty_pos

let exists (s : Normalized.t) = Sys.file_exists (s :> string)

let is_dir (s : Normalized.t) = Sys.is_directory (s :> string)

let readdir (s : Normalized.t) = Sys.readdir (s :> string)

let remove (s : Normalized.t) = Sys.remove (s :> string)

let rename s t = Sys.rename s t

let basename p = Filename.basename p

let dirname p = Filename.dirname p

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
