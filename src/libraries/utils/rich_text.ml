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
(* --- Rich Messages                                                      --- *)
(* -------------------------------------------------------------------------- *)

type tag = {
  p : int ; (* first position *)
  q : int ; (* last position (excluded) *)
  tag : Format.stag ;
  children : tag list ;
}

type message = string * tag list

let size (text,_) = String.length text
let char_at (text,_) k = String.get text k
let string (text,_) = text
let substring (text,_) k n = String.sub text k n

let rec lookup acc k = function
  | [] -> acc
  | { p ; q ; tag ; children } :: tags ->
    if k < p then lookup acc k tags else
    if q < k then acc else
      lookup ((tag,p,q+1-p)::acc) k children

let tags_at (_,tags) k = lookup [] k tags

type env = {
  text : string ;
  output : (string -> int -> int -> unit) option ;
  open_tag : (Format.stag -> int -> int -> unit) option ;
  close_tag : (Format.stag -> int -> int -> unit) option ;
}

let signal f tag p q =
  match f with None -> () | Some f -> f tag p (q+1-p)

let rec aux env p q = function
  | [] -> signal env.output env.text p q
  | { tag ; p=tp ; q=tq ; children } :: tags ->
    if q < tp then signal env.output env.text p q else
    if tq < q then aux env p q tags else
      begin
        if tp>p then signal env.output env.text p (tp-p) ;
        signal env.open_tag tag tp tq ;
        aux env tp tq children ;
        signal env.close_tag tag tp tq ;
        aux env (succ tq) q tags ;
      end

let visit ?output ?open_tag ?close_tag (text , tags) =
  aux { text ; output ; open_tag ; close_tag } 0 (String.length text) tags

let rec output_vbox fmt text k n =
  if n>0 then
    let p = try String.index_from text k '\n' with Not_found -> (-1) in
    if p < 0 || p >= k + n then
      Format.pp_print_string fmt (String.sub text k n)
    else
      begin
        Format.pp_print_string fmt (String.sub text k (p-k)) ;
        Format.pp_print_newline fmt () ;
        output_vbox fmt text (p+1) (n-p+k-1) ;
      end

let output_fmt fmt text k n = Format.pp_print_string fmt (String.sub text k n)
let open_tag fmt tag _k _n = Format.pp_open_stag fmt tag
let close_tag fmt _tag _k _n = Format.pp_close_stag fmt ()

let pretty ?vbox fmt message =
  let open_tag = open_tag fmt in
  let close_tag = close_tag fmt in
  match vbox with
  | None -> visit ~output:(output_fmt fmt) ~open_tag ~close_tag message
  | Some n ->
    begin
      Format.pp_open_vbox fmt n ;
      visit ~output:(output_vbox fmt) ~open_tag ~close_tag message ;
      Format.pp_close_box fmt () ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Extended Buffer with Tags                                          --- *)
(* -------------------------------------------------------------------------- *)

let min_buffer = 128    (* initial size of buffer *)

type buffer = {
  mutable formatter : Format.formatter ; (* formatter on self (recursive) *)
  content : Buffer.t ;
  mutable revtags : tag list ; (* in reverse order *)
  mutable stack : (int * tag list) list ; (* opened tag positions *)
}

let is_blank = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let trim_begin buffer =
  let rec lookup_fwd text k n =
    if k < n && is_blank (Buffer.nth text k) then
      lookup_fwd text (succ k) n else k
  in lookup_fwd buffer.content 0 (Buffer.length buffer.content)

let trim_end buffer =
  let rec lookup_bwd text k =
    if k >= 0 && is_blank (Buffer.nth text k) then
      lookup_bwd text (pred k) else k
  in lookup_bwd buffer.content (pred (Buffer.length buffer.content))

let shrink buffer =
  if Buffer.length buffer.content > min_buffer then
    Buffer.reset buffer.content

let truncate buffer size =
  let truncated = ref false in
  if Buffer.length buffer.content > size then
    begin
      let p = trim_begin buffer in
      let q = trim_end buffer in
      let n = q+1-p in
      if n <= 0 then
        shrink buffer
      else if n <= size then
        Buffer.blit buffer.content p (Buffer.to_bytes buffer.content) 0 n
      else
        let n_left = size / 2 - 3 in
        let n_right = size - n_left - 5 in
        if p > 0 then
          Buffer.blit buffer.content p (Buffer.to_bytes buffer.content) 0 n_left;
        let buf_right = Buffer.sub buffer.content (q-n_right+1) n_right in
        Buffer.truncate buffer.content n_left;
        Buffer.add_string buffer.content "[...]";
        Buffer.add_string buffer.content buf_right;
        truncated := true;
    end;
  !truncated

(* All text added shall go through this function *)
let append buffer s k n =
  Buffer.add_substring buffer.content s k n

let push_tag buffer _tag =
  let p = Buffer.length buffer.content in
  buffer.stack <- ( p , buffer.revtags ) :: buffer.stack ;
  buffer.revtags <- []

let pop_tag buffer tag =
  match buffer.stack with
  | [] -> ()
  | (p,tags)::stack ->
    let q = Buffer.length buffer.content in
    buffer.stack <- stack ;
    let children = List.rev buffer.revtags in
    buffer.revtags <- { p ; q ; tag ; children } :: tags

let no_mark _tag = ""

(* -------------------------------------------------------------------------- *)
(* --- External API                                                       --- *)
(* -------------------------------------------------------------------------- *)

let create ?indent ?margin () =
  let buffer = {
    formatter = Format.err_formatter ;
    content = Buffer.create min_buffer ;
    revtags = [] ;
    stack = [] ;
  } in
  let fmt = Format.make_formatter (append buffer) (fun () -> ()) in
  buffer.formatter <- fmt ;
  begin match indent , margin with
    | None , None -> ()
    | Some k , None ->
      let m = Format.pp_get_margin fmt () in
      Format.pp_set_max_indent fmt (max 0 (min k m))
    | None , Some m ->
      Format.pp_set_margin fmt (max 0 m) ;
      let k = Format.pp_get_max_indent fmt () in
      if k < m-10 then Format.pp_set_max_indent fmt (max 0 (m-10))
    | Some k , Some m ->
      Format.pp_set_margin fmt (max 0 m) ;
      Format.pp_set_max_indent fmt (max 0 (min k (m-10)))
  end ;
  let open Format in
  Format.pp_set_formatter_stag_functions fmt {
    Format.print_open_stag = push_tag buffer ;
    print_close_stag = pop_tag buffer ;
    mark_open_stag = no_mark ;
    mark_close_stag = no_mark ;
  } ;
  pp_set_print_tags fmt true ;
  pp_set_mark_tags fmt false ;
  buffer

let trim buffer =
  let p = trim_begin buffer in
  let q = trim_end buffer in
  p , q

let contents buffer =
  Buffer.contents buffer.content

let message buffer =
  ( Buffer.contents buffer.content , List.rev buffer.revtags )

let sub buffer p n = Buffer.sub buffer.content p n
let range buffer p q = Buffer.sub buffer.content p (q+1-p)

let add_char buffer c = Format.pp_print_char buffer.formatter c
let add_string buffer s = Format.pp_print_string buffer.formatter s
let add_substring buffer s k n =
  Format.pp_print_string buffer.formatter (String.sub s k n)

let formatter buffer = buffer.formatter
let bprintf buffer text = Format.fprintf buffer.formatter text
let kprintf kjob buffer text = Format.kfprintf kjob buffer.formatter text

let to_string ?(indent=20) ?(margin=40) ?(trim=true) pp data =
  let buffer = create ~indent ~margin () in
  let fmt = formatter buffer in
  pp fmt data ;
  Format.pp_print_flush fmt () ;
  if trim then
    let p = trim_begin buffer in
    let q = trim_end buffer in
    Buffer.sub buffer.content p (q+1-p)
  else
    Buffer.contents buffer.content

(* -------------------------------------------------------------------------- *)
