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

val product_category: Self.category

module Make
    (Context  : Abstract_context.S)
    (Value    : Abstract_value.S with type context = Context.t)
    (Location : Abstract_location.S with type value = Value.t)
    (Left  : Abstract.Domain.Internal
     with type context = Context.t
      and type value = Value.t
      and type location = Location.location)
    (Right : Abstract.Domain.Internal
     with type context = Context.t
      and type value = Value.t
      and type location = Location.location)
  : Abstract.Domain.Internal
    with type context = Context.t
     and type value = Value.t
     and type location = Location.location
     and type state = Left.state * Right.state


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
