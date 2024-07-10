open Libsail

open Ast
open Ast_util
open Jib
open Jib_compile
open Jib_util
open Jib_visitor
open Type_check
open PPrint
open Value2
module Document = Pretty_print_sail.Document

open Anf

module Big_int = Nat_big_num

let process_def_aux output_chan def_aux = match def_aux with
    DEF_type(type_def) -> ()
  | DEF_constraint(atyp) -> ()
  | DEF_fundef(fundef) -> ()
  | DEF_mapdef(mapdef) -> () (* mapping definition *)
  | DEF_impl(funcl) -> () (* impl definition *)
  | DEF_let(letbind) -> () (* value definition *)
  | DEF_overload(id,id_list) -> () (* operator overload specifications *)
  | DEF_fixity(prec,num,id) -> () (* fixity declaration *)
  | DEF_val(val_spec) -> () (* top-level type constraint *)
  | DEF_outcome(outcome_spec,def_list) -> () (* top-level outcome definition *)
  | DEF_instantiation(id,subst_list) -> () (* instantiation *)
  | DEF_default(default_typing_spec) -> () (* default kind and type assumptions *)
  | DEF_scattered(scattered_def) -> () (* scattered definition *)
  | DEF_measure(id,pat,exp) -> () (* separate termination measure declaration *)
  | DEF_loop_measures(id,loop_measure_list) -> () (* separate termination measure declaration *)
  | DEF_register(dec_spec) -> () (* register declaration *)
  | DEF_pragma(string0,string1,int) -> ()
  (* | DEF_private(def) -> () *)
  (* | DEF_attribute(string,attribute_data,option_def) -> () *)
  (* | DEF_doc(string, def) -> () *)
  | DEF_internal_mutrec(fundef_list) -> ()

let process_def output_chan def = match def with
    DEF_aux(aux,annot) -> process_def_aux output_chan aux

let rec process_def_list output_chan def_list = match def_list with
    [] -> ()
  | h :: t -> process_def output_chan h ;
              process_def_list output_chan t

(* ./src/lib/type_check.mli:type typed_ast = (tannot, env) ast *)
let process_ast (output_chan: out_channel) (ast: typed_ast) =
  print_endline "Generating XDSL...";
  begin
    match ast with
      {defs = d; comments = _} -> process_def_list output_chan d;
  end
