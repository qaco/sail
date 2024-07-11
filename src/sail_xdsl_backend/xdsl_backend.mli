open Libsail

open Ast_defs
open Jib
open Type_check

val process_ast: Env.t -> out_channel -> typed_ast -> unit
