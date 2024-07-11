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

let string_of_id id = match id with
    Id_aux(Id(id),_) -> id
  | Id_aux(Operator(_),_) -> assert(false)

let process_dec_spec env output reg = match reg with
    Typ_aux(Typ_id(id),_) ->
     let sid = string_of_id id in
     print_string(": " ^ sid);
  | Typ_aux(Typ_var(kid),_) -> ()
  | Typ_aux(Typ_app(id,typ_list),_) ->
     let sid = string_of_id id in
     print_endline(": " ^ sid);
     assert(false)
  | Typ_aux(Typ_fn(id_in,typ_list),_) -> ()
  | Typ_aux(Typ_bidir(typ1,typ2),_) -> ()
  | Typ_aux(Typ_tuple(typ_list),_) -> ()
  | Typ_aux(Typ_exist(kinded_id_list,typ0,typ1),_) -> ()
  | Typ_aux(Typ_internal_unknown,_) -> ()


let process_def_aux env output_chan def_aux = match def_aux with
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
  (* register declaration *)
  | DEF_register(DEC_aux(DEC_reg(reg,id,opt),_)) ->
     let sid = string_of_id id in
     print_string("register " ^ sid);
     process_dec_spec env output reg;
     print_endline("");
  | DEF_pragma(string0,string1,int) -> ()
  (* | DEF_private(def) -> () *)
  (* | DEF_attribute(string,attribute_data,option_def) -> () *)
  (* | DEF_doc(string, def) -> () *)
  | DEF_internal_mutrec(fundef_list) -> ()

let rec process_def_list env output_chan def_list = match def_list with
    [] -> ()
  | DEF_aux(aux,_) :: t -> process_def_aux env output_chan aux ;
                           process_def_list env output_chan t

(* Env: ~/src/upstream/sail/src/lib/type_env.ml *)
(* Ast: ./src/lib/type_check.mli:type typed_ast = (tannot, env) ast *)
let process_ast (env: Env.t) (output_chan: out_channel) (ast: typed_ast) =
  print_endline "Generating XDSL...";
  begin
    match ast with
      {defs = d; comments = _} -> process_def_list env output_chan d;
  end
