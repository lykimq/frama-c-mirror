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

let inset_string () =
  if Kernel.Unicode.get () then Utf8_logic.inset else "IN"

let emptyset_string () =
  if Kernel.Unicode.get () then Utf8_logic.emptyset else "EMPTY_SET"

let union_string () =
  if Kernel.Unicode.get () then Utf8_logic.union else "U"

let top_string () =
  if Kernel.Unicode.get () then Utf8_logic.top else "TOP"

let bottom_string () =
  if Kernel.Unicode.get () then Utf8_logic.bottom else "BOTTOM"
