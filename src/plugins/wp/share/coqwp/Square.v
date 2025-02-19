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

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require Reals.R_sqrt.
Require BuiltIn.
Require real.Real.
Require real.RealInfix.
Require real.Square.

(* Why3 goal *)
Lemma sqrt_lin1 : forall (x:R), (1%R < x)%R -> ((Reals.R_sqrt.sqrt x) < x)%R.
Proof.
  intros x h1.
  refine (Reals.R_sqrt.sqrt_less _ _ h1).
  apply (Rle_trans 0 1 x Rle_0_1)%R.
  exact (Rlt_le _ _ h1).
Qed.

(* Why3 goal *)
Lemma sqrt_lin0 :
  forall (x:R), ((0%R < x)%R /\ (x < 1%R)%R) -> (x < (Reals.R_sqrt.sqrt x))%R.
Proof.
  intros x (h1,h2).
  exact (Reals.R_sqrt.sqrt_more x h1 h2).
Qed.

(* Why3 goal *)
Lemma sqrt_0 : ((Reals.R_sqrt.sqrt 0%R) = 0%R).
Proof.
  exact Reals.R_sqrt.sqrt_0.
Qed.

(* Why3 goal *)
Lemma sqrt_1 : ((Reals.R_sqrt.sqrt 1%R) = 1%R).
Proof.
  exact Reals.R_sqrt.sqrt_1.
Qed.

