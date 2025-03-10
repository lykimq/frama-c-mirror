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

(*
  Internal documentation.

  The Sparecode module aims at removing the unused code.

  It is composed of to parts :
  - one (in module {!module:Marks}) that computes some information
    to say what has to be kept in the result. It uses the generic PDG
    marking facility {{:../pdg/PdgMarks.ml}PdgMarks} and
    {{:../pdg/Marks.ml}Marks},
  - and a second one (module {!module:Transform}) that read thoses results to
    produce a new application. This part mainly use the kernel AST
    transformation Filter which provides a functor that filters an application
    to create another one.

  To select the useful statements, we start from the [main] outputs and the
  reachable annotations, and mark backward all the dependencies. When reaching
  a function call, the called function statements are also marked according to
  the needed outputs, but the inputs are not propagated immediately because it
  would make every function call visible. The information provided by the PDG
  marking system is kept to be used later.

  So, after the first step, we iterate on the input marks to propagate, and
  propagate them only for the visible calls, ie those which have at least one
  visible output. This process is repeated as long as there are some
  modification.
*)

module Register = Register
