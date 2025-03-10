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

(** compatibility layer between gtksourceview 2 and 3. *)
include GSourceView3

let make_marker_attributes
    ~(source:source_view)
    ~(category:string)
    ~(priority: int)
    ?(background: Gdk.rgba option)
    ?(pixbuf:GdkPixbuf.pixbuf option)
    ?(icon_name:string option)
    () =
  let my_attributes = GSourceView3.source_mark_attributes () in
  Option.iter my_attributes#set_background background;
  Option.iter my_attributes#set_pixbuf pixbuf;
  Option.iter my_attributes#set_icon_name icon_name;
  source#set_mark_attributes ~category my_attributes priority
