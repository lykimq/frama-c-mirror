(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2025                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Cil_types
open Logic_ptree
open Aorai_option
open Automaton_ast
open Bool3
open Format

type 'a printer = Format.formatter -> 'a -> unit

let bool3_to_string = function
  | True -> "True"
  | False -> "False"
  | Undefined -> "Undef"

let print_bool3 fmt b =
  Format.pp_print_string fmt (bool3_to_string b)

let print_state fmt st =
  Format.fprintf fmt "@[<2>%s@ (acc=%a;@ init=%a;@ num=%d)@]"
    st.name print_bool3 st.acceptation print_bool3 st.init st.nums

let print_statel fmt stl =
  Format.fprintf fmt "@[<2>States:@\n%a@]"
    (Pretty_utils.pp_list ~sep:"@\n" ~suf:"@\n" print_state) stl

let state_label num = "st"^string_of_int(num)
let print_state_label fmt st =
  Format.fprintf fmt "@[<2>%s:@ %s@]" (state_label st.nums) st.name


module Parsed =
struct
  let string_of_unop = function
    | Uminus -> "-"
    | Ustar -> "*"
    | Uamp -> "&"
    | Ubw_not -> "~"

  let rec print_expression fmt = function
    | PVar s -> Format.fprintf fmt "%s" s
    | PPrm (f,s) -> Format.fprintf fmt "%s().%s" f s
    | PMetavar s -> Format.fprintf fmt "$%s" s
    | PCst (IntConstant s) -> Format.fprintf fmt "%s" s
    | PCst (FloatConstant s) -> Format.fprintf fmt "%s" s
    | PCst (StringConstant s) -> Format.fprintf fmt "%S" s
    | PCst (WStringConstant s) -> Format.fprintf fmt "%S" s
    | PBinop(bop,e1,e2) ->
      Format.fprintf fmt "(@[%a@])@ %a@ (@[%a@])"
        print_expression e1 Printer.pp_binop (Logic_typing.type_binop bop)
        print_expression e2
    | PUnop(uop,e) ->
      Format.fprintf fmt "%s@;(@[%a@])"
        (string_of_unop uop)
        print_expression e
    | PArrget(e1,e2) ->
      Format.fprintf fmt "%a@;[@(%a@]]"
        print_expression e1 print_expression e2
    | PField(e,s) -> Format.fprintf fmt "%a.%s" print_expression e s
    | PArrow(e,s) -> Format.fprintf fmt "%a->%s" print_expression e s

  let rec print_condition fmt = function
    | PRel(rel,e1,e2) ->
      Format.fprintf fmt "%a %a@ %a"
        print_expression e1
        Printer.pp_relation (Logic_typing.type_rel rel)
        print_expression e2
    | PTrue -> Format.pp_print_string fmt "true"
    | PFalse -> Format.pp_print_string fmt "false"
    | POr(e1,e2) ->
      Format.fprintf fmt "(@[%a@])@ or@ (@[%a@])"
        print_condition e1 print_condition e2
    | PAnd(e1,e2) ->
      Format.fprintf fmt "(@[%a@])@ and@ (@[%a@])"
        print_condition e1 print_condition e2
    | PNot c ->
      Format.fprintf fmt "not(@[%a@])"
        print_condition c
    | PCall (s,None) -> Format.fprintf fmt "CALL(%s)" s
    | PCall (s, Some b) -> Format.fprintf fmt "CALL(%s::%s)" s b
    | PReturn s -> Format.fprintf fmt "RETURN(%s)" s

  let rec print_seq_elt fmt elt =
    Format.fprintf fmt "(%a%a){@[%a,%a@]}"
      (Pretty_utils.pp_opt print_condition) elt.condition
      print_sequence elt.nested
      (Pretty_utils.pp_opt print_expression) elt.min_rep
      (Pretty_utils.pp_opt print_expression) elt.max_rep

  and print_sequence fmt l =
    Pretty_utils.pp_list ~pre:"[@[" ~sep:";@ " ~suf:"@]]" print_seq_elt fmt l

  let print_guard fmt = function
    | Seq l -> print_sequence fmt l
    | Otherwise -> Format.pp_print_string fmt "Otherwise"

  let print_action fmt = function
    | Metavar_assign (s, e) ->
      Format.fprintf fmt "@[$%s := %a@]" s print_expression e

  let print_actionl fmt l =
    Pretty_utils.pp_list ~sep:"@\n" print_action fmt l
end


module Typed =
struct
  let rec print_condition fmt = function
    | TCall (kf,None) ->
      Format.fprintf fmt "Call(%a)" Kernel_function.pretty kf
    | TCall (kf, Some b) ->
      Format.fprintf fmt "Call(%a::%s)"
        Kernel_function.pretty kf b.Cil_types.b_name
    | TReturn kf ->
      Format.fprintf fmt "Return(%a)" Kernel_function.pretty kf
    | TOr  (c1,c2) ->
      Format.fprintf fmt "@[<hov>(@[<2>%a@])@]@ or@ @[<hov>(@[<2>%a@])@]"
        print_condition c1 print_condition c2
    | TAnd (c1,c2) ->
      Format.fprintf fmt "@[<hov>(@[<2>%a@])@]@ and@ @[<hov>(@[<2>%a@])@]"
        print_condition c1 print_condition c2
    | TNot c ->
      Format.fprintf fmt "@[<hov 4>@[<hov 5>not(%a@])@]" print_condition c
    | TTrue ->
      Format.pp_print_string fmt "True"
    | TFalse ->
      Format.pp_print_string fmt "False"
    | TRel(rel,exp1,exp2) ->
      (* \result will be printed as such, not as f().return *)
      Format.fprintf fmt "@[(%a)@]@ %a@ @[(%a)@]"
        Printer.pp_term exp1
        Printer.pp_relation rel
        Printer.pp_term exp2

  let print_action fmt = function
    | Counter_init lv ->
      Format.fprintf fmt "@[%a <- 1@]" Printer.pp_term_lval lv
    | Counter_incr lv ->
      Format.fprintf fmt "@[%a <- @[%a@ +@ 1@]@]"
        Printer.pp_term_lval lv Printer.pp_term_lval lv
    | Pebble_init (set,_,v) ->
      Format.fprintf fmt "@[%a <- {@[ %a @]}@]"
        Printer.pp_logic_var set.l_var_info Printer.pp_logic_var v
    | Pebble_move(s1,_,s2,_) ->
      Format.fprintf fmt "@[%a <- %a@]"
        Printer.pp_logic_var s1.l_var_info
        Printer.pp_logic_var s2.l_var_info
    | Copy_value(lv,v) ->
      Format.fprintf fmt "@[%a <- %a@]" Printer.pp_term_lval lv Printer.pp_term v

  let print_actionl fmt l =
    Pretty_utils.pp_list ~sep:"@\n" print_action fmt l

  (* Use well-parenthesized combination of escape_newline/normal_newline*)
  let escape_newline fmt =
    let funcs = Format.pp_get_formatter_out_functions fmt () in
    let has_printed = ref false in
    let out_newline () =
      if !has_printed then funcs.Format.out_string " \\\n" 0 3
      else funcs.Format.out_newline ()
    in
    let out_string s b l =
      if String.contains (String.sub s b l) '"' then
        has_printed:=not !has_printed;
      funcs.Format.out_string s b l
    in
    Format.pp_set_formatter_out_functions fmt
      { funcs with Format.out_newline; out_string };
    funcs

  let trans_label num = "tr"^string_of_int(num)

  let print_trans fmt trans =
    Format.fprintf fmt
      "@[<2>%s:@ %a@\n%a@]"
      (trans_label trans.numt)
      print_condition trans.cross
      print_actionl trans.actions

  let print_transition fmt tr =
    Format.fprintf fmt "@[<2>{@ %d:@ %s@ {%a@\n%a}@ %s@]}"
      tr.numt
      tr.start.name
      print_condition tr.cross
      print_actionl tr.actions
      tr.stop.name

  let print_transitionl fmt trl =
    Format.fprintf fmt "@[<2>Transitions:@\n%a@]"
      (Pretty_utils.pp_list ~sep:"@\n" ~suf:"@\n" print_transition) trl

  let print_automata fmt auto =
    Format.fprintf fmt "@[<2>Automaton:@\n%a%a@]"
      print_statel auto.states print_transitionl auto.trans

  let dot_state out st =
    let shape =
      if st.init = Bool3.True && st.acceptation=Bool3.True then "doubleoctagon"
      else if st.acceptation=Bool3.True then "octagon"
      else if st.init=Bool3.True then "doublecircle"
      else "circle"
    in
    Format.fprintf out "\"%a\" [shape = %s];@\n" print_state_label st shape

  let dot_trans out tr =
    let print_label fmt tr =
      if DotSeparatedLabels.get () then
        Format.pp_print_int fmt tr.numt
      else print_trans fmt tr
    in
    Format.fprintf
      out
      "\"%a\"@ ->@ \"%a\"@ [label = @[\"%a\"@]];@\n"
      print_state_label tr.start
      print_state_label tr.stop
      print_label tr

  let output_dot_automata {states ; trans} fichier =
    let cout = open_out fichier in
    let fmt = formatter_of_out_channel cout in
    let output_functions = escape_newline fmt in
    let one_line_comment s =
      let l = String.length s in
      let fill = if l >= 75 then 0 else 75 - l in
      let spaces = String.make fill ' ' in
      Format.fprintf fmt "@[/* %s%s*/@\n@]" s spaces
    in
    one_line_comment "File generated by Aorai Plug-in";
    one_line_comment "";
    one_line_comment "Usage of dot files '.dot' :";
    one_line_comment "  dot <MyFile.dot> -T<DesiredType> > <OutputFile>";
    one_line_comment "";
    one_line_comment "   Allowed types : canon,dot,xdot,fig,gd,gd2,";
    one_line_comment "   gif,hpgl,imap,cmap,ismap,jpg,jpeg,mif,mp,pcl,pic,plain,";
    one_line_comment "   plain-ext,png,ps,ps2,svg,svgz,vrml,vtx,wbmp";
    one_line_comment "";
    one_line_comment " Example with postscript file :";
    one_line_comment "    dot property.dot -Tps > property.ps";
    Format.fprintf fmt "@[<2>@\ndigraph %s {@\n@\n%a@\n%a@\n%t}@\n@]"
      (Filename.chop_extension (Filename.basename fichier))
      (Pretty_utils.pp_list dot_state) states
      (Pretty_utils.pp_list dot_trans) trans
      (fun fmt ->
         if DotSeparatedLabels.get () then
           (Format.fprintf fmt
              "/* guards of transitions */@\ncomment=\"%a\";@\n"
              (Pretty_utils.pp_list ~sep:"@\n" print_trans) trans));
    Format.pp_set_formatter_out_functions fmt output_functions;
    close_out cout
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
