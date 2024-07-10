open Libsail

open Interactive.State

let opt_includes_xdsl : string list ref = ref []
let opt_specialize_xdsl = ref false

let xdsl_options = []

let xdsl_rewrites =
  let open Rewrites in
  [
    ("instantiate_outcomes", [String_arg "c"]);
    ("realize_mappings", []);
    ("remove_vector_subrange_pats", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("truncate_hex_literals", []);
    ("mono_rewrites", [If_flag opt_mono_rewrites]);
    ("recheck_defs", [If_flag opt_mono_rewrites]);
    ("toplevel_nexps", [If_mono_arg]);
    ("monomorphise", [String_arg "c"; If_mono_arg]);
    ("atoms_to_singletons", [String_arg "c"; If_mono_arg]);
    ("recheck_defs", [If_mono_arg]);
    ("undefined", [Bool_arg false]);
    ("vector_string_pats_to_bit_list", []);
    ("remove_not_pats", []);
    ("remove_vector_concat", []);
    ("remove_bitvector_pats", []);
    ("pattern_literals", [Literal_arg "all"]);
    ("tuple_assignments", []);
    ("vector_concat_assignments", []);
    ("simple_struct_assignments", []);
    ("exp_lift_assign", []);
    ("merge_function_clauses", []);
    ("recheck_defs", []);
    ("constant_fold", [String_arg "c"]);
  ]

let xdsl_target out_file { ast; effect_info; env; _ } =
  let close, output_chan = match out_file with Some f -> (true, open_out (f ^ ".c")) | None -> (false, stdout) in
  Reporting.opt_warnings := true;
  Xdsl_backend.compile_ast env effect_info output_chan !opt_includes_xdsl ast;
  flush output_chan;
  if close then close_out output_chan

let _ = Target.register ~name:"xdsl" ~options:xdsl_options ~rewrites:xdsl_rewrites xdsl_target
