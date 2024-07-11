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

let string_of_nexp_aux nexp_aux = match nexp_aux with
    Nexp_id(id) -> assert(false)
  | Nexp_var(kid) -> assert(false)
  | Nexp_constant(num) -> Nat_big_num.to_string(num)
  | Nexp_app(id,nexp_list) -> assert(false)
  | Nexp_if(n_constraint,nexp0,nexp1) -> assert(false)
  | Nexp_times(nexp0, nexp1) -> assert(false)
  | Nexp_sum(nexp0,nexp1) -> assert(false)
  | Nexp_minus(nexp0,nexp1) -> assert(false)
  | Nexp_exp(nexp) -> assert(false)
  | Nexp_neg(nexp) -> assert(false)

let rec process_dec_spec env output (Typ_aux(reg,_)) = match reg with
    Typ_id(id) ->
     let sid = string_of_id id in
     sid
  | Typ_var(kid) -> assert(false)
  | Typ_app(id,typ_arg_list) ->
     let sid = string_of_id id in
     let string_typ_arg_list = List.map (process_typ_args env output) typ_arg_list in
     let h,t = List.hd string_typ_arg_list, List.tl string_typ_arg_list in
     let string_typ_args = h ^ (List.fold_left (fun acc x -> acc^","^x) "" t) in
     sid ^ "(" ^ string_typ_args ^ ")"
  | Typ_fn(id_in,typ_list) -> assert(false)
  | Typ_bidir(typ1,typ2) -> assert(false)
  | Typ_tuple(typ_list) -> assert(false)
  | Typ_exist(kinded_id_list,typ0,typ1) -> assert(false)
  | Typ_internal_unknown -> assert(false)
and process_typ_args env output (A_aux (typ_arg_aux, _)) = match typ_arg_aux with
    A_nexp n ->
     begin
       match n with
         Nexp_aux(nexp_aux0,nexp_aux1) -> string_of_nexp_aux nexp_aux0
     end
  | A_typ(t) -> process_dec_spec env output t
  | A_bool(NC_aux(nca,_)) ->
     begin
       match nca with
         NC_equal(nexp0,nexp1) -> assert(false)
       | NC_bounded_ge(nexp0,nexp1) -> assert(false)
       | NC_bounded_gt(nexp0,nexp1) -> assert(false)
       | NC_bounded_le(nexp0,nexp1) -> assert(false)
       | NC_bounded_lt(nexp0,nexp1) -> assert(false)
       | NC_not_equal(nexp0,nexp1) -> assert(false)
       | NC_set(nexp,num_list) -> assert(false)
       | NC_or(n_constraint0, n_constraint1) -> assert(false)
       | NC_and(n_constraint0,n_constraint1) -> assert(false)
       | NC_app(id,typ_arg_list) -> assert(false)
       | NC_id (id) -> assert(false)
       | NC_var(kid) -> assert(false)
       | NC_true -> assert(false)
       | NC_false -> assert(false)
     end

let process_def_aux env output_chan def_aux = match def_aux with
    DEF_type(type_def) -> ""
  | DEF_constraint(atyp) -> ""
  | DEF_fundef(fundef) -> ""
  | DEF_mapdef(mapdef) -> "" (* mapping definition *)
  | DEF_impl(funcl) -> "" (* impl definition *)
  | DEF_let(letbind) -> "" (* value definition *)
  | DEF_overload(id,id_list) -> "" (* operator overload specifications *)
  | DEF_fixity(prec,num,id) -> "" (* fixity declaration *)
  | DEF_val(val_spec) -> "" (* top-level type constraint *)
  | DEF_outcome(outcome_spec,def_list) -> "" (* top-level outcome definition *)
  | DEF_instantiation(id,subst_list) -> "" (* instantiation *)
  | DEF_default(default_typing_spec) -> "" (* default kind and type assumptions *)
  | DEF_scattered(scattered_def) -> "" (* scattered definition *)
  | DEF_measure(id,pat,exp) -> "" (* separate termination measure declaration *)
  | DEF_loop_measures(id,loop_measure_list) -> "" (* separate termination measure declaration *)
  (* register declaration *)
  | DEF_register(DEC_aux(DEC_reg(reg,id,opt),_)) ->
     let string_id = string_of_id id in
     let string_register_definition = process_dec_spec env output reg in
     "register " ^ string_id ^ ": " ^ string_register_definition
  | DEF_pragma(string0,string1,int) -> ""
  (* | DEF_private(def) -> "" *)
  (* | DEF_attribute(string,attribute_data,option_def) -> "" *)
  (* | DEF_doc(string, def) -> "" *)
  | DEF_internal_mutrec(fundef_list) -> ""

let rec process_def_list env output_chan def_list = match def_list with
    [] -> ""
  | DEF_aux(aux,_) :: t -> (match process_def_aux env output_chan aux with
                              "" -> process_def_list env output_chan t
                            | s -> s ^ "\n" ^ process_def_list env output_chan t)

(* Env: ~/src/upstream/sail/src/lib/type_env.ml *)
(* Ast: ./src/lib/type_check.mli:type typed_ast = (tannot, env) ast *)
let process_ast (env: Env.t) (output_chan: out_channel) (ast: typed_ast) =
  print_endline "Generating XDSL...";
  begin
    match ast with
      {defs = d; comments = _} -> print_string(process_def_list env output_chan d);
  end
