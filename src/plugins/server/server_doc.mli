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

(* -------------------------------------------------------------------------- *)
(** Server Documentation *)
(* -------------------------------------------------------------------------- *)

open Markdown

(** The main chapters of the documentation. *)
type chapter = [ `Protocol | `Kernel | `Plugin of string ]

(** A page of the server documentation. *)
type page

val path : page -> string
val href : page -> string -> href
val chapter : page -> chapter

(** Obtain the given page in the server documentation.

    The readme introductory section is
    read from the source directory:
    - [src/plugins/server/<filename>] server and kernel pages,
    - [src/plugins/<plugin>/<filename>] for plugin's pages.
*)
val page : chapter ->
  title:string ->
  ?descr:elements ->
  ?plugin:Package.plugin ->
  readme:string option ->
  filename:string ->
  unit ->page

(** Adds a section in the corresponding page.
    Returns an href to the published section.
    If index items are provided, they are added to the server documentation
    index.
*)
val publish :
  page:page ->
  ?name:string ->
  ?index:string list ->
  title:string ->
  ?contents:Markdown.elements ->
  ?generated:(unit -> Markdown.elements) ->
  unit -> Markdown.href

(** Publish a protocol. *)
val protocol : title:string -> readme:string -> unit

(** Publish a package. *)
val package : Package.packageInfo -> unit

(** Dumps all published pages of documentations. Unless [~meta:false], also
    generates METADATA for each page in [<filename>.json] for each page. *)
val dump : root:Filepath.Normalized.t -> ?meta:bool -> unit -> unit

(* -------------------------------------------------------------------------- *)
