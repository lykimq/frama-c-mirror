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
Require BuiltIn.
Require bool.Bool.
Require int.Int.

(* Why3 goal *)
Definition set : forall (a:Type), Type.
Admitted.

(* Why3 goal *)
Definition empty {a:Type} {a_WT:WhyType a} : set a.
Admitted.

(* Why3 goal *)
Definition singleton {a:Type} {a_WT:WhyType a} : a -> set a.
Admitted.

(* Why3 goal *)
Definition union {a:Type} {a_WT:WhyType a} : (set a) -> (set a) -> set a.
Admitted.

(* Why3 goal *)
Definition inter {a:Type} {a_WT:WhyType a} : (set a) -> (set a) -> set a.
Admitted.

(* Why3 goal *)
Definition member {a:Type} {a_WT:WhyType a} : a -> (set a) -> Prop.
Admitted.

(* Why3 goal *)
Definition member_bool {a:Type} {a_WT:WhyType a} : a -> (set a) -> bool.
Admitted.

(* Why3 goal *)
Definition range : Z -> Z -> set Z.
Admitted.

(* Why3 goal *)
Definition range_sup : Z -> set Z.
Admitted.

(* Why3 goal *)
Definition range_inf : Z -> set Z.
Admitted.

(* Why3 goal *)
Definition range_all : set Z.
Admitted.

(* Why3 assumption *)
Definition eqset {a:Type} {a_WT:WhyType a} (a1:set a) (b:set a) : Prop :=
  forall (x:a), (member x a1) <-> (member x b).

(* Why3 assumption *)
Definition subset {a:Type} {a_WT:WhyType a} (a1:set a) (b:set a) : Prop :=
  forall (x:a), (member x a1) -> member x b.

(* Why3 assumption *)
Definition disjoint {a:Type} {a_WT:WhyType a} (a1:set a) (b:set a) : Prop :=
  forall (x:a), (member x a1) -> ~ (member x b).

(* Why3 goal *)
Lemma member_bool1 {a:Type} {a_WT:WhyType a} :
  forall (x:a), forall (s:set a),
  ((member x s) -> ((member_bool x s) = true)) /\
  (~ (member x s) -> ((member_bool x s) = false)).
Proof.
intros x s.

Admitted.

(* Why3 goal *)
Lemma member_empty {a:Type} {a_WT:WhyType a} :
  forall (x:a), ~ (member x (empty : set a)).
Proof.
intros x.

Admitted.

(* Why3 goal *)
Lemma member_singleton {a:Type} {a_WT:WhyType a} :
  forall (x:a) (y:a), (member x (singleton y)) <-> (x = y).
Proof.
intros x y.

Admitted.

(* Why3 goal *)
Lemma member_union {a:Type} {a_WT:WhyType a} :
  forall (x:a), forall (a1:set a) (b:set a),
  (member x (union a1 b)) <-> ((member x a1) \/ (member x b)).
Proof.
intros x a1 b.

Admitted.

(* Why3 goal *)
Lemma member_inter {a:Type} {a_WT:WhyType a} :
  forall (x:a), forall (a1:set a) (b:set a),
  (member x (inter a1 b)) <-> ((member x a1) /\ (member x b)).
Proof.
intros x a1 b.

Admitted.

(* Why3 goal *)
Lemma union_empty {a:Type} {a_WT:WhyType a} :
  forall (a1:set a),
  ((union a1 (empty : set a)) = a1) /\ ((union (empty : set a) a1) = a1).
Proof.
intros a1.

Admitted.

(* Why3 goal *)
Lemma inter_empty {a:Type} {a_WT:WhyType a} :
  forall (a1:set a),
  ((inter a1 (empty : set a)) = (empty : set a)) /\
  ((inter (empty : set a) a1) = (empty : set a)).
Proof.
intros a1.

Admitted.

(* Why3 goal *)
Lemma member_range :
  forall (x:Z) (a:Z) (b:Z),
  (member x (range a b)) <-> ((a <= x)%Z /\ (x <= b)%Z).
Proof.
intros x a b.

Admitted.

(* Why3 goal *)
Lemma member_range_sup :
  forall (x:Z) (a:Z), (member x (range_sup a)) <-> (a <= x)%Z.
Proof.
intros x a.

Admitted.

(* Why3 goal *)
Lemma member_range_inf :
  forall (x:Z) (b:Z), (member x (range_inf b)) <-> (x <= b)%Z.
Proof.
intros x b.

Admitted.

(* Why3 goal *)
Lemma member_range_all : forall (x:Z), member x range_all.
Proof.
intros x.

Admitted.

