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

let home () =
  match Sys.getenv "HOME" with
  | "" -> raise Not_found
  | s -> Filepath.Normalized.of_string s

let env_or_default env default =
  let open Filepath.Normalized in
  match Sys.getenv_opt env with
  | Some s when s <> "" -> concat (of_string s) "frama-c"
  | _ -> concats (home ()) default

let cache () =
  env_or_default "XDG_CACHE_HOME" [ "Library" ; "Caches" ; "frama-c"]
let config () =
  env_or_default "XDG_CONFIG_HOME" [ "Application Support" ; "frama-c" ; "config" ]
let state () =
  env_or_default "XDG_STATE_HOME" [ "Application Support" ; "frama-c" ; "state" ]
