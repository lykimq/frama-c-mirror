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

(** Provided services for kernel developers.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************* *)
(** {2 Log Machinery} *)
(* ************************************************************************* *)

include Plugin.S

(* ************************************************************************* *)
(** {2 Message and warning categories} *)
(* ************************************************************************* *)

val dkey_acsl_extension: category

val dkey_alpha: category

val dkey_alpha_undo: category

val dkey_asm_contracts: category

val dkey_ast: category

val dkey_builtins: category

val dkey_check: category

val dkey_constfold: category

val dkey_comments: category

val dkey_compilation_db: category

val dkey_dataflow: category

val dkey_dataflow_scc: category

val dkey_dominators: category

val dkey_dyncalls: category

val dkey_emitter: category

val dkey_emitter_clear: category

val dkey_exn_flow: category

val dkey_file_transform: category

val dkey_file_print_one: category

val dkey_file_annot: category

val dkey_file_source: category
(** Messages related to operations on files during preprocessing/parsing. *)

val dkey_filter: category

val dkey_globals: category

val dkey_kf_blocks: category

val dkey_linker: category

val dkey_linker_find: category

val dkey_loops: category

val dkey_pp_keep_temp_files: category

val dkey_pp: category

val dkey_pp_logic: category

val dkey_print_attrs: category

val dkey_print_bitfields: category

val dkey_print_builtins: category

val dkey_print_logic_coercions: category

val dkey_print_logic_types: category

val dkey_print_imported_modules: category

val dkey_print_sid: category

val dkey_print_unspecified: category

val dkey_print_vid: category

val dkey_print_field_offsets: category

val dkey_prop_status: category

val dkey_prop_status_emit: category

val dkey_prop_status_merge: category

val dkey_prop_status_graph: category

val dkey_prop_status_reg: category

val dkey_rmtmps: category

val dkey_referenced: category

val dkey_task: category

val dkey_typing_global: category

val dkey_typing_init: category

val dkey_typing_chunk: category

val dkey_typing_cast: category

val dkey_typing_pragma: category

val dkey_ulevel: category

val dkey_visitor: category

val wkey_annot_error: warn_category
(** error in annotation. If only a warning, annotation will just be ignored. *)

val wkey_plugin_not_loaded: warn_category
(** Warning related to not loaded plugins.
    @since 29.0-Copper
*)

val wkey_extension_unknown: warn_category
(** Warning related to the use of an unregistered ACSL extension.
    @since 29.0-Copper
*)

val wkey_ghost_already_ghost: warn_category
(** ghost element is qualified with \ghost while this is already the case
    by default *)

val wkey_ghost_bad_use: warn_category
(** error in ghost code *)

val wkey_acsl_float_compare: warn_category

val wkey_large_array: warn_category

val wkey_conditional_feature: warn_category
(** parsing feature that is only supported in specific modes
    (e.g. C11, gcc, ...). *)

val wkey_drop_unused: warn_category

val wkey_linker_weak: warn_category

val wkey_implicit_conv_void_ptr: warn_category

val wkey_implicit_int: warn_category

val wkey_incompatible_types_call: warn_category

val wkey_incompatible_pointer_types: warn_category

val wkey_inconsistent_specifier: warn_category

val wkey_int_conversion: warn_category

val wkey_merge_conversion: warn_category

val wkey_cert_exp_46: warn_category

val wkey_cert_msc_37: warn_category

val wkey_cert_msc_38: warn_category

val wkey_cert_exp_10: warn_category

val wkey_check_volatile: warn_category

val wkey_jcdb: warn_category

val wkey_implicit_function_declaration: warn_category

val wkey_no_proto: warn_category

val wkey_missing_spec: warn_category

val wkey_multi_from: warn_category

val wkey_decimal_float: warn_category

val wkey_acsl_extension: warn_category

val wkey_cmdline: warn_category
(** Command-line related warning, e.g. for invalid options given by the user *)

val wkey_audit: warn_category
(** Warning related to options '-audit-*'. *)

val wkey_parser_unsupported: warn_category
(** Warning related to unsupported parsing-related features. *)

val wkey_parser_unsupported_attributes: warn_category
(** Warning related to unsupported attributes during parsing. *)

val wkey_parser_unsupported_pragma: warn_category
(** Warning related to unsupported _Pragma's during parsing. *)

val wkey_asm: warn_category
(** Warnings related to assembly code. *)

val wkey_unnamed_typedef: warn_category
(** Warning related to "unnamed typedef that does not introduce a struct
    or enumeration type". *)

val wkey_file_not_found: warn_category
(** Warnings related to missing files during preprocessing/parsing. *)

val wkey_c11: warn_category
(** Warnings related to usage of C11-specific constructions. *)

val wkey_line_directive: warn_category
(** Warnings related to unknown line directives. *)

val wkey_unknown_attribute: warn_category
(** Warning emitted when an unknown attribute is encountered during parsing. *)

(* ************************************************************************* *)
(** {2 Functors for late option registration}                                *)
(** Kernel_function-related options cannot be registered in this module:
    They depend on [Globals], which is linked later. We provide here functors
    to declare them after [Globals] *)
(* ************************************************************************* *)

module type Input_with_arg = sig
  include Parameter_sig.Input_with_arg
  val module_name: string
end

module Kernel_function_set(_:Input_with_arg): Parameter_sig.Kernel_function_set

(* ************************************************************************* *)
(** {2 Option groups} *)
(* ************************************************************************* *)

val inout_source: Cmdline.Group.t

val saveload: Cmdline.Group.t

val parsing: Cmdline.Group.t

val normalisation: Cmdline.Group.t

val analysis_options: Cmdline.Group.t

val seq: Cmdline.Group.t

val project: Cmdline.Group.t

val checks: Cmdline.Group.t

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

module Version: Parameter_sig.Bool
(** Behavior of option "-version" *)

module PrintVersion: Parameter_sig.Bool
(** Behavior of option "-print-version" *)

module PrintConfig: Parameter_sig.Bool
(** Behavior of option "-print-config" *)

module PrintShare: Parameter_sig.Bool
(** Behavior of option "-print-share-path" *)

module PrintLib: Parameter_sig.Bool
(** Behavior of option "-print-lib-path" *)

module PrintPluginPath: Parameter_sig.Bool
(** Behavior of option "-print-plugin-path" *)

module AutocompleteHelp: Parameter_sig.String_set
(** Behavior of option "-autocomplete" *)

module PrintConfigJson: Parameter_sig.Bool
(** Behavior of option "-print-config-json"
    @since 22.0-Titanium *)

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

module GeneralVerbose: Parameter_sig.Int
(** Behavior of option "-verbose" *)

module GeneralDebug: Parameter_sig.Int
(** Behavior of option "-debug" *)

module Quiet: Parameter_sig.Bool
(** Behavior of option "-quiet" *)

module Permissive: Parameter_sig.Bool
(** Behavior of option "-permissive" *)

(** @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Unicode: sig
  include Parameter_sig.Bool
  val without_unicode: ('a -> 'b) -> 'a -> 'b
  (** Execute the given function as if the option [-unicode] was not set. *)
end
(** Behavior of option "-unicode".
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

module Time: Parameter_sig.String
(** Behavior of option "-time" *)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

module PrintCode : Parameter_sig.Bool
(** Behavior of option "-print" *)

module PrintAsIs : Parameter_sig.Bool
(** Behavior of option "-print-as-is"
    @since 24.0-Chromium *)

module PrintMachdep : Parameter_sig.Bool
(** Behavior of option "-print-machdep"
    @since Phosphorus-20170501-beta1 *)

module PrintMachdepHeader : Parameter_sig.Bool
(** Behavior of option "-print-machdep-header"
    @since 27.0-Cobalt *)

module PrintLibc: Parameter_sig.Bool
(** Behavior of option "-print-libc"
    @since Phosphorus-20170501-beta1 *)

module PrintComments: Parameter_sig.Bool
(** Behavior of option "-keep-comments" *)

module PrintReturn : Parameter_sig.Bool
(** Behavior of option "-print-return"
    @since Sulfur-20171101 *)

(** Behavior of option "-ocode".
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module CodeOutput : sig
  include Parameter_sig.Filepath
  val output: (Format.formatter -> unit) -> unit
end

(** Behavior of option "-ast-diff" *)
module AstDiff: Parameter_sig.Bool

(** Behavior of option "-add-symbolic-path"
    @since Neon-20140301
    @before 23.0-Vanadium argument order was inversed (name:path); now it is
            (path:name). *)
module SymbolicPath: Parameter_sig.Filepath_map with type value = string

module FloatNormal: Parameter_sig.Bool
(** Behavior of option "-float-normal" *)

module FloatRelative: Parameter_sig.Bool
(** Behavior of option "-float-relative" *)

module FloatHex: Parameter_sig.Bool
(** Behavior of option "-float-hex" *)

module BigIntsHex: Parameter_sig.Int
(** Behavior of option "-hexadecimal-big-integers" *)

module EagerLoadSources: Parameter_sig.Bool
(** Behavior of option "-eager-load-sources" *)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

module SaveState: Parameter_sig.Filepath
(** Behavior of option "-save" *)

module LoadState: Parameter_sig.Filepath
(** Behavior of option "-load" *)

module LoadModule: Parameter_sig.String_list
(** Behavior of option "-load-module" *)

module LoadLibrary: Parameter_sig.String_list
(** Behavior of option "-load-library"
    @since 26.0-Iron
*)

module AutoLoadPlugins: Parameter_sig.Bool
(** Behavior of option "-autoload-plugins" *)

module Session_dir: Parameter_sig.User_dir
(** Directory in which session files are searched.
    @since Neon-20140301
    @before 23.0-Vanadium parameter type was string instead of Filepath.
*)

module Cache_dir: Parameter_sig.User_dir
(** Directory in which cache files are searched.
    @since 30.0-Zinc
*)

module Config_dir: Parameter_sig.User_dir
(** Directory in which config files are searched.
    @since Neon-20140301
    @before 23.0-Vanadium parameter type was string instead of Filepath.
*)

module State_dir: Parameter_sig.User_dir
(** Directory in which state files are searched.
    @since 30.0-Zinc
*)

(* this stop special comment does not work as expected (and as explained in the
   OCamldoc manual, Section 15.2.2. It just skips all the rest of the file
   instead of skipping until the next stop comment...
   (**/**)
*)

module Set_project_as_default: Parameter_sig.Bool
(** Undocumented. *)

(* See (meta-)comment on the previous stop comment
   (**/**)
*)

(* ************************************************************************* *)
(** {2 Customizing Normalization and parsing} *)
(* ************************************************************************* *)

module UnfoldingLevel: Parameter_sig.Int
(** Behavior of option "-ulevel" *)

module UnfoldingForce: Parameter_sig.Bool
(** Behavior of option "-ulevel-force"
    @since Neon-20140301 *)

(** Behavior of option "-machdep".
    If function [set] is called, then {!File.prepare_from_c_files} must be
    called for well preparing the AST. *)
module Machdep: sig
  include Parameter_sig.String

  (** [get_dir] returns the directory containing default machdeps.
      @since Frama-C+dev *)
  val get_dir : unit -> LoadState.t

  (** [get_default_file] return the file with the name format of machdep from
      the default machdep directory.
      @since Frama-C+dev *)
  val get_default_file : string -> LoadState.t

  (** [is_default] decides if the parameter refers to a default machdep or a
      user file.
      @since Frama-C+dev *)
  val is_default : string -> bool
end

(** Behavior of invisible option -keep-logical-operators:
    Tries to avoid converting && and || into conditional statements.
    Note that this option is incompatible with many (most) plug-ins of the
    platform and thus should only be enabled with great care and for very
    specific analyses need.
*)
module LogicalOperators: Parameter_sig.Bool

(** Behavior of option "-enums" *)
module Enums: Parameter_sig.String

module CppCommand: Parameter_sig.String
(** Behavior of option "-cpp-command" *)

module CppExtraArgs: Parameter_sig.String_list
(** Behavior of option "-cpp-extra-args" *)

module CppExtraArgsPerFile: Parameter_sig.Filepath_map with type value = string
(** Behavior of option "-cpp-extra-args-per-file" *)

module CppGnuLike: Parameter_sig.Bool
(** Behavior of option "-cpp-frama-c-compliant" *)

module PrintCppCommands: Parameter_sig.Bool
(** Behavior of option "-print-cpp-commands" *)

module AuditPrepare: Parameter_sig.Filepath
(** Behavior of option "-audit-prepare" *)

module AuditCheck: Parameter_sig.Filepath
(** Behavior of option "-audit-check" *)

module FramaCStdLib: Parameter_sig.Bool
(** Behavior of option "-frama-c-stdlib" *)

module ReadAnnot: Parameter_sig.Bool
(** Behavior of option "-read-annot" *)

module PreprocessAnnot: Parameter_sig.Bool
(** Behavior of option "-pp-annot" *)

module SimplifyCfg: Parameter_sig.Bool
(** Behavior of option "-simplify-cfg" *)

module KeepSwitch: Parameter_sig.Bool
(** Behavior of option "-keep-switch" *)

module KeepUnusedFunctions: Parameter_sig.String
(** Behavior of option "-keep-unused-functions". *)

module Keep_unused_types: Parameter_sig.Bool
(** Behavior of option "-keep-unused-types". *)

module SimplifyTrivialLoops: Parameter_sig.Bool
(** Behavior of option "-simplify-trivial-loops". *)

module Constfold: Parameter_sig.Bool
(** Behavior of option "-constfold" *)

module InitializedPaddingLocals: Parameter_sig.Bool
(** Behavior of option "-initialized-padding-locals" *)

module AggressiveMerging: Parameter_sig.Bool
(** Behavior of option "-aggressive-merging" *)

module AsmContractsGenerate: Parameter_sig.Bool
(** Behavior of option "-asm-contracts" *)

module AsmContractsInitialized: Parameter_sig.Bool
(** Behavior of option "-asm-contracts-ensure-init" *)

module AsmContractsAutoValidate: Parameter_sig.Bool
(** Behavior of option "-asm-contracts-auto-validate" *)

module InlineStmtContracts: Parameter_sig.Bool
(** Behavior of option "-inline-stmt-contracts" *)

module RemoveExn: Parameter_sig.Bool
(** Behavior of option "-remove-exn" *)

(** Analyzed files *)
module Files: Parameter_sig.Filepath_list
(** List of files to analyse *)

module Orig_name: Parameter_sig.Bool
(** Behavior of option "-orig-name" *)

val normalization_parameters: unit -> Typed_parameter.t list
(** All the normalization options that influence the AST (in particular,
    changing one will reset the AST entirely.contents
*)

module C11: Parameter_sig.Bool
(** Behavior of option "-c11" *)

module JsonCompilationDatabase: Parameter_sig.Filepath
(** Behavior of option "-json-compilation-database" *)

(* ************************************************************************* *)
(** {3 Customizing cabs2cil options} *)
(* ************************************************************************* *)

module AllowDuplication: Parameter_sig.Bool
(** Behavior of option "-allow-duplication". *)

module DoCollapseCallCast: Parameter_sig.Bool
(** Behavior of option "-collapse-call-cast".

    If false, the destination of a Call instruction should always have the
    same type as the function's return type.  Where needed, CIL will insert a
    temporary to make this happen.

    If true, the destination type may differ from the return type, so there
    is an implicit cast.  This is useful for analyses involving [malloc],
    because the instruction "T* x = malloc(...);" won't be broken into
    two instructions, so it's easy to find the allocation type.

    This is false by default.  Set to true to replicate the behavior
    of CIL 1.3.5 and earlier. *)

module GeneratedSpecMode: Parameter_sig.String
(** Behavior of option "-generated-spec-mode". *)

module GeneratedSpecCustom: Parameter_sig.Map
  with type key = string
   and type value = string
(** Behavior of option "-generated-spec-custom". *)

(* ************************************************************************* *)
(** {2 Analysis Behavior of options} *)
(* ************************************************************************* *)

(** Behavior of option "-main".

    You should usually use {!Globals.entry_point} instead of
    {!MainFunction.get} since the first one handles the case where the entry
    point is invalid in the right way. *)
module MainFunction: sig

  include Parameter_sig.String

  (** {2 Internal functions}

      Not for casual users. *)

  val unsafe_set: t -> unit

end

(** Behavior of option "-lib-entry".

    You should usually use {!Globals.entry_point} instead of
    {!LibEntry.get} since the first one handles the case where the entry point
    is invalid in the right way. *)
module LibEntry: sig
  include Parameter_sig.Bool
  val unsafe_set: t -> unit (** Not for casual users. *)
end

module UnspecifiedAccess: Parameter_sig.Bool
(** Behavior of option "-unspecified-access" *)

module SafeArrays: Parameter_sig.Bool
(** Behavior of option "-safe-arrays".
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

module SignedOverflow: Parameter_sig.Bool
(** Behavior of option "-warn-signed-overflow" *)

module UnsignedOverflow: Parameter_sig.Bool
(** Behavior of option "-warn-unsigned-overflow" *)

module LeftShiftNegative: Parameter_sig.Bool
(** Behavior of option "-warn-left-shift-negative" *)

module RightShiftNegative: Parameter_sig.Bool
(** Behavior of option "-warn-right-shift-negative" *)

module SignedDowncast: Parameter_sig.Bool
(** Behavior of option "-warn-signed-downcast" *)

module UnsignedDowncast: Parameter_sig.Bool
(** Behavior of option "-warn-unsigned-downcast" *)

module PointerDowncast: Parameter_sig.Bool
(** Behavior of option "-warn-pointer-downcast" *)

module SpecialFloat: Parameter_sig.String
(** Behavior of option "-warn-special-float" *)

module InvalidBool: Parameter_sig.Bool
(** Behavior of option "-warn-invalid-bool" *)

module InvalidPointer: Parameter_sig.Bool
(** Behavior of option "-warn-invalid-pointer" *)

module AbsoluteValidRange: Parameter_sig.String
(** Behavior of option "-absolute-valid-range" *)

(*
module FloatFlushToZero: Parameter_sig.Bool
  (** Behavior of option "-float-flush-to-zero" *)
*)

(* ************************************************************************* *)
(** {2 Checks} *)
(* ************************************************************************* *)

module Check: Parameter_sig.Bool
(** Behavior of option "-check" *)

module Copy: Parameter_sig.Bool
(** Behavior of option "-copy" *)

module TypeCheck: Parameter_sig.Bool
(** Behavior of option "-typecheck" *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
