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

(** Configuration of command line options.

    You can apply the functions below just before applying one of the functors
    provided by the functor {!Plugin.Register} and generating a new
    parameter.

    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val set_cmdline_stage: Cmdline.stage -> unit
(** Set the stage where the option corresponding to the parameter is
    recognized. Default is [Cmdline.Configuring].
    @since Beryllium-20090601-beta1 *)

val do_not_projectify: unit -> unit
(** Prevent projectification of the parameter: its state is shared by all the
    existing projects. Also imply {!do_not_save} and {!do_not_reset_on_copy}.
    @since Beryllium-20090601-beta1 *)

val do_not_reset_on_copy: unit -> unit
(** Prevents resetting the parameter to its default value when creating
    a project from a copy visitor.
    @since Neon-20140301 *)

val do_not_save: unit -> unit
(** Prevent serialization of the parameter.
    @since Carbon-20110201 *)

val set_negative_option_name: string -> unit
(** For boolean parameters, set the name of the negative
    option generating automatically from the positive one (the given option
    name). The default used value prefixes the given option name by "-no".
    Assume that the given string is a valid option name or empty.
    If it is empty, no negative option is created.
    @since Beryllium-20090601-beta1
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val set_negative_option_help: string -> unit
(** For boolean parameters, set the help message of the negative
      option generating automatically.
      Assume that the given string is non empty.
      @since Beryllium-20090601-beta1 *)

val set_unset_option_name: string -> unit
(** For string collection parameters, set the name of an option that
    will remove elements from the set. There is no default value: if
    the this function is not called (or if it is the empty string),
    it will only be possible to add elements from the command line.
    @since Fluorine-20130401 *)

val set_unset_option_help: string -> unit
(** For string collection parameters, gives the help message for
    the corresponding unset option. Useless if [set_unset_option_name]
    has not been called before. No default.
    @since Fluorine-20130401 *)

val set_group: Cmdline.Group.t -> unit
(** Affect a group to the parameter.
      @since Beryllium-20090901 *)

val is_invisible: unit -> unit
(** Prevent -help from listing the parameter.
    Also imply {!is_not_reconfigurable}.
    @since Carbon-20101201
*)

val argument_is_function_name: unit -> unit
(** Indicate that the string argument of the parameter must be a valid function
    name. A valid function name is the name of a function defined in the
    analysed C program. Do nothing if the following applied functor has not type
    [String].
    @since Oxygen-20120901
*)

val argument_may_be_fundecl: unit -> unit
(** Indicate that the argument of the parameter can match a valid function
    declaration (otherwise it has to match a defined functions).
    @since Sodium-20150201 *)

val argument_must_be_fundecl: unit -> unit
(** Indicate that the argument of the parameter must match a valid function
    declaration.
    @since Aluminium-20160501 *)

val argument_must_be_existing_fun: unit -> unit
(** Indicate that if the argument of the parameter does not match a valid
    function name, it raises an error whatever the value of the option
    -permissive is. Only meaningful for parameters that are collections of
    [kernel_function] or [fundec].

    This flag {b does not} imply [argument_may_be_fundecl]. If the latter is
    unset, names of defined-only functions will raise an error as well.
    @since Sodium-20150201 *)

val is_reconfigurable: unit -> unit
(** Ensure that the GUI will show this parameter. By default
    only parameters corresponding to options registered at the
    {!Cmdline.Configuring} stage are reconfigurable.
    @since Nitrogen-20111001
    @before 22.0-Titanium this function was called [do_iterate].
*)

val is_not_reconfigurable: unit -> unit
(** Prevent the GUI from showing this parameter. By default, only
    parameters corresponding to options registered at the
    {!Cmdline.Configuring} stage are reconfigurable.
    @since Nitrogen-20111001
    @before 22.0-Titanium this function was called [do_not_iterate].
*)

val no_category: unit -> unit
(** Prevent a collection parameter to use categories and the extension '+', and
    '-' mechanism. In particular, you should consider this customization when
    the parameter is a list of '-' prefixed options to an external tool, unless
    you are willing to let users escape the initial '-' everytime.
    @since Sodium-20150201
*)

(* ************************************************************************* *)
(** {2 Function names} *)
(* ************************************************************************* *)

val get_c_ified_functions:
  string -> Cil_datatype.Kf.Set.t
(** Function names can be modified (aka mangled) from the original source to
    valid C identifiers. In order to allow users to use the original names
    on the command-line options manipulating function names, this function
    will return the set of function whose name correspond to the given string,
    possibly via a mangling operation registered with the
    {!add_function_name_transformation} function below. By default, no
    mangling function is registered, and the returned set is either empty or
    a singleton corresponding to the unique function with that name. Results
    from all registered functions are cumulative, so that a mangling function
    should take care of returning the empty set for names that it does not
    understand.

    @since Sodium-20150201
*)

val add_function_name_transformation:
  (string -> Cil_datatype.Kf.Set.t) -> unit
(**
    Adds a mangling operation to allow writing user-friendly function names
    on command-line. See {!get_c_ified_functions} for more information.
    @since Sodium-20150201
*)

(**/**)
(* ************************************************************************* *)
(** {2 Internal kernel stuff} *)
(* ************************************************************************* *)

val reset: unit -> unit
(** Reset all customizers to their default values. *)

val set_module_name: string -> unit
(** For **kernel** parameters, set the name of the module name corresponding to
    the parameter. Not for casual users. *)

val find_kf_by_name: (string -> Cil_types.kernel_function) ref
(** @since Sodium-20150201 *)

(* ************************************************************************* *)
(** {3 The customizers themselves} *)
(* ************************************************************************* *)

val cmdline_stage_ref: Cmdline.stage ref
val negative_option_name_ref: string option ref
val negative_option_help_ref: string ref
val unset_option_name_ref: string ref
val unset_option_help_ref: string ref
val must_save_ref: bool ref
val reset_on_copy_ref: bool ref
val projectify_ref: bool ref
val optional_help_ref: (unit, Format.formatter, unit) format ref
val argument_is_function_name_ref: bool ref
val argument_may_be_fundecl_ref: bool ref
val argument_must_be_fundecl_ref: bool ref
val argument_must_be_existing_fun_ref: bool ref
val group_ref: Cmdline.Group.t ref
val is_reconfigurable_ref: bool option ref
val is_visible_ref: bool ref
val module_name_ref: string ref
val use_category_ref: bool ref

(**/**)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
