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

open Tactical
open Conditions

(* -------------------------------------------------------------------------- *)
(* --- Step Look Around                                                   --- *)
(* -------------------------------------------------------------------------- *)

let around f k n =
  match f k with
  | Some s -> s
  | None ->
    let rec scan f k i n =
      match f (k-i) with
      | Some s -> s
      | None ->
        match f (k+i) with
        | Some s -> s
        | None ->
          let j = succ i in
          if k+j < n || j <= k then
            scan f k j n
          else raise Not_found
    in scan f k 1 n

let s_kind s = match s.condition with
  | Have _ | When _ | Core _ -> "have"
  | Type _ -> "type"
  | Init _ -> "init"
  | Branch _ -> "branch"
  | Probe _ -> "probe"
  | Either _ -> "either"
  | State _ -> "state"

let check_pattern ~pattern p =
  if not (Footprint.matches pattern (Lang.F.e_prop p))
  then raise Not_found

let lookup_occur ~occur p =
  Footprint.lookup ~occur ~inside:(Lang.F.e_prop p)

let lookup_step ~kind ~pattern hs k =
  try
    let s = Conditions.step_at hs k in
    if s_kind s <> kind then raise Not_found ;
    let p = Conditions.head s in
    check_pattern ~pattern p ; Some s
  with Not_found -> None

let lookup_inside ~kind ~occur hs k =
  try
    let s = Conditions.step_at hs k in
    if s_kind s <> kind then raise Not_found ;
    let p = Conditions.head s in
    Some (s , lookup_occur ~occur p)
  with Not_found -> None

let locate_step ~at ~kind ~pattern hs =
  around (lookup_step ~kind ~pattern hs) at (Conditions.size hs)

let locate_inside ~at ~kind ~occur hs =
  around (lookup_inside ~kind ~occur hs) at (Conditions.size hs)

(* -------------------------------------------------------------------------- *)
(* --- Selection of Json                                                  --- *)
(* -------------------------------------------------------------------------- *)

let pattern p =
  Footprint.pattern (Lang.F.e_prop p)

let occur p t =
  Footprint.locate ~inside:(Lang.F.e_prop p) ~select:t

let j_select s = "select" , `String s
let j_goal = j_select "clause-goal"
let j_step = j_select "clause-step"
let j_ingoal = j_select "inside-goal"
let j_instep = j_select "inside-step"
let j_compose = j_select "compose"
let j_multi = j_select "multi"
let j_kint = j_select "kint"
let j_range = j_select "range"
let j_id a = "id" , `String a
let j_at s = "at" , `Int s.id
let j_int z = "val" , `String (Integer.to_string z)
let j_min a = "min" , `Int a
let j_max b = "max" , `Int b
let j_kind s = "kind" , `String (s_kind s)
let j_pattern p = "pattern" , `String p
let j_ppattern p = j_pattern (pattern p)
let j_occur k = "occur" , `Int k
let j_pred p =
  let tgt = Pretty_utils.to_string Lang.F.pp_pred p in
  "target" , `String tgt
let j_term e =
  let tgt = Pretty_utils.to_string Lang.F.pp_term e in
  "target" , `String tgt

let rec json_of_selection = function

  | Empty -> `Null
  | Compose code -> json_of_compose code

  | Clause (Goal p) ->
    `Assoc[ j_goal ; j_pred p ; j_ppattern p ]

  | Clause (Step s) ->
    let p = Conditions.head s in
    `Assoc[ j_step ; j_at s ; j_kind s ; j_pred p ; j_ppattern p ]

  | Inside(Goal p,e) ->
    let n,m = occur p e in
    `Assoc [ j_ingoal ; j_occur n ; j_term e ; j_pattern m ]

  | Inside(Step s,e) ->
    let n,m = occur (Conditions.head s) e in
    `Assoc [ j_instep ; j_at s ; j_kind s ; j_occur n ;
             j_term e ; j_pattern m ]

  | Multi es ->
    `Assoc (j_multi :: j_args es)

and j_args = function
  | [] -> []
  | es -> ["args" , `List (List.map json_of_selection es)]

and json_of_compose = function
  | Cint a -> `Assoc [j_kint ; j_int a]
  | Range(a,b) -> `Assoc [j_range ; j_min a ; j_max b]
  | Code(_,id,es) -> `Assoc (j_compose :: j_id id :: j_args es)

(* -------------------------------------------------------------------------- *)
(* --- Json to Selection                                                  --- *)
(* -------------------------------------------------------------------------- *)

let (>?) js (fd:string) = Json.field fd js
let (|>) js op = op js

let j_pattern js =  js >? "pattern" |> Json.string
let j_at js = js >? "at" |> Json.int
let j_kind js = js >? "kind" |> Json.string
let j_occur js = js >? "occur" |> Json.int
let j_id js = js >? "id" |> Json.string
let j_args js = js >? "args" |> Json.list
let j_val js = js >? "val" |> Json.string |> Integer.of_string
let j_min js = js >? "min" |> Json.int
let j_max js = js >? "max" |> Json.int

let rec selection_of_json ((hs,g) as s : sequent) js =
  try
    let key = js >? "select" |> Json.string in
    match key with
    | "clause-goal" ->
      check_pattern ~pattern:(j_pattern js) g ;
      Clause (Goal g)
    | "clause-step" ->
      let pattern = j_pattern js in
      let s = locate_step ~at:(j_at js) ~kind:(j_kind js) ~pattern hs in
      Clause (Step s)
    | "inside-goal" ->
      let occur = j_occur js , j_pattern js in
      Inside(Goal g , lookup_occur ~occur g )
    | "inside-step" ->
      let occur = j_occur js , j_pattern js in
      let s,e = locate_inside ~at:(j_at js) ~kind:(j_kind js) ~occur hs in
      Inside(Step s,e)
    | "compose" ->
      let id = j_id js in
      let args = j_args js in
      Tactical.compose id (List.map (selection_of_json s) args)
    | "multi" ->
      let args = j_args js in
      Tactical.multi @@ List.map (selection_of_json s) args
    | "kint" -> Tactical.cint (j_val js)
    | "range" -> Tactical.range (j_min js) (j_max js)
    | _ -> raise Not_found
  with Not_found | Invalid_argument _ ->
    Empty

let selection_target js = js >? "target" |> Json.string

let json_of_named = function
  | None -> `Null
  | Some a ->
    `Assoc Tactical.[
        "id" , `String a.vid ;
        "title" , `String a.title ;
        "descr" , `String a.descr ;
      ]

let named_of_json find js =
  try
    let vid = js >? "id" |> Json.string in
    Some (find vid)
  with Not_found | Invalid_argument _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Tactical Json Parameters                                           --- *)
(* -------------------------------------------------------------------------- *)

let json_of_param (tac : tactical) = function
  | Checkbox fd -> ident fd , Json.of_bool (tac#get_field fd)
  | Spinner(fd,_) -> ident fd , Json.of_int (tac#get_field fd)
  | Composer(fd,_) -> ident fd , json_of_selection (tac#get_field fd)
  | Selector(fd,options,equal) ->
    ident fd , `String
      begin
        try
          let a = tac#get_field fd in
          let v = List.find (fun v -> equal v.value a) options in
          v.vid
        with _ -> "default"
      end
  | Search(fd,_,_) ->
    ident fd , json_of_named (tac#get_field fd)

let param_of_json (tac : tactical) seq js = function
  | Checkbox fd ->
    tac#set_field fd
      (try Json.bool (Json.field (ident fd) js)
       with _ -> default fd)
  | Spinner(fd,_) ->
    tac#set_field fd
      (try Json.int (Json.field (ident fd) js)
       with _ -> default fd)
  | Composer(fd,_) ->
    let sel = (try selection_of_json seq (Json.field (ident fd) js)
               with _ -> default fd) in
    tac#set_field fd sel
  | Selector(fd,options,_) ->
    tac#set_field fd
      begin
        try
          let jid = Json.string (Json.field (ident fd) js) in
          let v = List.find (fun v -> v.vid = jid) options in
          v.value
        with _ -> default fd
      end
  | Search(fd,_,find) ->
    tac#set_field fd
      begin
        try named_of_json find (Json.field (ident fd) js)
        with _ -> None
      end

let json_of_parameters (tac : tactical) =
  `Assoc (List.map (json_of_param tac) tac#params)

let parameters_of_json (tac : tactical) sequent js =
  List.iter (param_of_json tac sequent js) tac#params

(* -------------------------------------------------------------------------- *)
(* --- Tactic Encoding                                                    --- *)
(* -------------------------------------------------------------------------- *)

type jtactic = {
  header : string ;
  tactic : string ;
  params : Json.t ;
  select : Json.t ;
  strategy : string option ;
}

let jtactic ?strategy (tac : tactical) (sel : selection) =
  {
    header = tac#title ;
    tactic = tac#id ;
    params = json_of_parameters tac ;
    select = json_of_selection sel ;
    strategy ;
  }

let json_of_tactic t js =
  let strategy =
    match t.strategy with
    | None -> []
    | Some s -> [ "strategy" , `String s ]
  in
  let tactical = [
    "header" , `String t.header ;
    "tactic" , `String t.tactic ;
    "params" , t.params ;
    "select" , t.select ;
    "children" , `Assoc js ;
  ] in
  `Assoc (strategy @ tactical)

let children_of_json = function
  | `List js ->
    Wp_parameters.warning ~current:false ~once:true
      "Deprecated script(s) found ; consider using prover 'tip'" ;
    List.map (fun j -> "",j) js
  | `Assoc fs -> fs
  | _ -> []

let tactic_of_json js =
  try
    let header = js >? "header" |> Json.string in
    let tactic = js >? "tactic" |> Json.string in
    let params = try js >? "params" with Not_found -> `Null in
    let select = try js >? "select" with Not_found -> `Null in
    let children =
      try js >? "children" |> children_of_json with Not_found -> [] in
    let strategy =
      try Some (js >? "strategy" |> Json.string) with Not_found -> None in
    Some( { header ; tactic ; params ; select ; strategy } , children )
  with _ -> None

(* -------------------------------------------------------------------------- *)
(* --- Prover Encoding                                                    --- *)
(* -------------------------------------------------------------------------- *)

let json_of_verdict = function
  | VCS.NoResult | VCS.Computing _ -> `String "none"
  | VCS.Valid -> `String "valid"
  | VCS.Unknown -> `String "unknown"
  | VCS.Timeout -> `String "timeout"
  | VCS.Stepout -> `String "stepout"
  | VCS.Failed -> `String "failed"
  | VCS.Invalid -> `String "invalid"

let verdict_of_json = function
  | `String "valid" -> VCS.Valid
  | `String "unknown" -> VCS.Unknown
  | `String "timeout" -> VCS.Timeout
  | `String "stepout" -> VCS.Stepout
  | `String "failed" -> VCS.Failed
  | `String "invalid" -> VCS.Invalid
  | _ -> VCS.NoResult

let json_of_result (p : VCS.prover) (r : VCS.result) =
  let open VCS in
  let name = "prover" , `String (VCS.name_of_prover p) in
  let verdict = "verdict" , json_of_verdict r.verdict in
  let time = if r.prover_time > 0.0 then [ "time" , `Float r.prover_time ] else [] in
  let steps = if r.prover_steps > 0 then [ "steps" , `Int r.prover_steps ] else [] in
  `Assoc (name :: verdict :: (time @ steps))

let prover_of_json js =
  match js >? "prover" |> Json.string with
  | exception Not_found -> None
  | pname -> VCS.prover_of_name ~fallback:true pname

let result_of_json js =
  let verdict = try js >? "verdict" |> verdict_of_json with _ -> VCS.NoResult in
  let time = try js >? "time" |> Json.float with _ -> 0.0 in
  let steps = try js >? "steps" |> Json.int with _ -> 0 in
  VCS.result ~time ~steps verdict

(* -------------------------------------------------------------------------- *)
(* --- Script                                                             --- *)
(* -------------------------------------------------------------------------- *)

type jscript = alternative list
and alternative =
  | Prover of VCS.prover * VCS.result
  | Tactic of int * jtactic * (string * jscript) list (* pending goals *)
  | Error of string * Json.t

let is_prover = function Prover _ -> true | Tactic _ | Error _ -> false
let is_tactic = function Tactic _ -> true | Prover _ | Error _ -> false

let pending = function
  | Prover(_, r) -> if VCS.is_valid r then 0 else 1
  | Tactic(n,_,_) -> n
  | Error _ -> 1

let rec pending_any = function
  | [] -> 1
  | [a] -> pending a
  | a::s ->
    let n = pending a in
    if n = 0 then 0 else min n (pending_any s)

let rec subgoals n = function
  | [] -> n
  | (_,js)::s -> subgoals (n + pending_any js) s

let has_proof = List.exists is_tactic

let a_prover p r = Prover(p,r)
let a_tactic tac children  = Tactic(subgoals 0 children,tac,children)

(* -------------------------------------------------------------------------- *)
(* --- Codecs                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec decode = function
  | `Null -> []
  | `List alts -> List.map alternative alts
  | js -> [Error("Invalid Script",js)]

and subscript (key,js) = key , decode js

and alternative js =
  match prover_of_json js with
  | Some prover -> Prover(prover,result_of_json js)
  | None ->
    match tactic_of_json js with
    | Some(tactic, children) -> a_tactic tactic (List.map subscript children)
    | None -> Error("Invalid Tactic",js)

let rec encode script = `List (alternatives script)

and subgoal (k,alt) = k , encode alt

and alternatives = function
  | [] -> []
  | Prover(p,r) :: scr -> json_of_result p r :: alternatives scr
  | Tactic(_,t,s) :: scr -> json_of_tactic t (List.map subgoal s) :: alternatives scr
  | Error _ :: scr -> alternatives scr

let configure jtactic sequent =
  try
    let tactical = Tactical.lookup ~id:jtactic.tactic in
    tactical#reset ;
    parameters_of_json tactical sequent jtactic.params ;
    Conditions.index sequent ;
    let select = selection_of_json sequent jtactic.select in
    Some(tactical,select)
  with Not_found -> None

(* -------------------------------------------------------------------------- *)
(* --- Console                                                            --- *)
(* -------------------------------------------------------------------------- *)

class console ~pool ~title =
  object

    val mutable the_title = title

    method pool : Lang.F.pool = pool
    method interactive = false
    method get_title = the_title
    method set_title : 'a. 'a formatter =
      fun msg -> Pretty_utils.ksfprintf (fun s -> the_title <- s) msg
    method set_descr : 'a. 'a formatter =
      fun msg -> Pretty_utils.ksfprintf (fun s -> ignore s) msg

    method update_field :
      'a. ?enabled:bool -> ?title:string -> ?tooltip:string ->
      ?range:bool -> ?vmin:int -> ?vmax:int ->
      ?filter:(Lang.F.term -> bool) -> 'a field -> unit =
      fun ?enabled ?title ?tooltip ?range ?vmin ?vmax ?filter field ->
      ignore enabled ;
      ignore title ;
      ignore tooltip ;
      ignore field ;
      ignore vmin ; ignore vmax ;
      ignore range ; ignore filter ;
      ()

    val mutable errors = false
    method has_error = errors
    method set_error
      : 'a. 'a formatter
      = fun msg ->
        Pretty_utils.ksfprintf
          (fun s -> errors <- true ;
            Wp_parameters.warning "[%s] %s" title s)
          msg

  end

(* -------------------------------------------------------------------------- *)
