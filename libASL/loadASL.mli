(****************************************************************
 * Functions for processing ASL files
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

module AST = Asl_ast
module TC  = Tcheck

type source = FileSource of string | DataSource of string * string
val pp_source : source -> string
val name_of_source : source -> string
val read_source : source -> string
 val write_source : string -> source -> unit

val mkLoc : string -> string -> AST.l

val report_parse_error : (unit -> 'a) -> (unit -> 'a) -> 'a
val report_type_error  : (unit -> 'a) -> (unit -> 'a) -> 'a
val report_eval_error  : (unit -> 'a) -> (unit -> 'a) -> 'a

(** Parse and typecheck ASL file *)
val read_file   : source -> bool -> bool -> Asl_ast.declaration list

val read_spec   : source -> bool -> Asl_ast.declaration list

(** Parse ASL file, but do not typecheck *)
val parse_file  : source -> bool -> bool -> Asl_ast.declaration list

val read_impdef : TC.Env.t -> AST.l -> string -> (string * AST.expr)
val read_expr   : TC.Env.t -> AST.l -> string -> AST.expr
val read_stmt   : TC.Env.t -> string -> AST.stmt

(****************************************************************
 * End
 ****************************************************************)
