open Libsail
open MlirDef
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
    Nexp_id(id) -> string_of_id id
  | Nexp_var(Kid_aux(Var(v),_)) -> "(* Nexp_var *) " ^ v
  | Nexp_constant(num) -> Nat_big_num.to_string(num)
  | Nexp_app(id,nexp_list) -> assert(false)
  | Nexp_if(n_constraint,nexp0,nexp1) -> assert(false)
  | Nexp_times(nexp0, nexp1) -> assert(false)
  | Nexp_sum(nexp0,nexp1) -> assert(false)
  | Nexp_minus(nexp0,nexp1) -> assert(false)
  | Nexp_exp(nexp) -> assert(false)
  | Nexp_neg(nexp) -> assert(false)

let rec process_dec_spec env (Typ_aux(reg,_)) = match reg with
    Typ_id(id) ->
     let sid = string_of_id id in
     sid
  | Typ_var(kid) -> assert(false)
  | Typ_app(id,typ_arg_list) ->
     let sid = string_of_id id in
     let string_typ_arg_list = List.map (process_typ_args_stub env) typ_arg_list in
     let h,t = List.hd string_typ_arg_list, List.tl string_typ_arg_list in
     let string_typ_args = h ^ (List.fold_left (fun acc x -> acc^","^x) "" t) in
     "(* Typ_app *) " ^ sid ^ "(" ^ string_typ_args ^ ")"
  | Typ_fn(id_in,typ_list) -> assert(false)
  | Typ_bidir(typ1,typ2) -> assert(false)
  | Typ_tuple(typ_list) -> assert(false)
  | Typ_exist(kinded_id_list,typ0,typ1) ->
     prerr_string "Typ_exist not implemented\n";
     assert(false)
  | Typ_internal_unknown -> assert(false)
and process_typ_args_stub env (A_aux (typ_arg_aux, _)) = match typ_arg_aux with
    A_nexp n ->
     begin
       match n with
         Nexp_aux(nexp_aux0,nexp_aux1) -> string_of_nexp_aux nexp_aux0
     end
  | A_typ(t) -> process_dec_spec env t
  | A_bool(NC_aux(nca,_)) ->
     begin
       match nca with
         NC_equal(nexp0,nexp1) -> assert(false)
       | NC_bounded_ge(nexp0,nexp1) -> assert(false)
       | NC_bounded_gt(nexp0,nexp1) -> assert(false)
       | NC_bounded_le(nexp0,nexp1) -> assert(false)
       | NC_bounded_lt(nexp0,nexp1) -> assert(false)
       | NC_not_equal(nexp0,nexp1) -> assert(false)
       | NC_set(nexp,num_list) ->
          prerr_string "NC_set not implemented\n";
          assert(false)
       | NC_or(n_constraint0, n_constraint1) -> assert(false)
       | NC_and(n_constraint0,n_constraint1) -> assert(false)
       | NC_app(id,typ_arg_list) -> assert(false)
       | NC_id (id) -> assert(false)
       | NC_var(kid) -> assert(false)
       | NC_true -> assert(false)
       | NC_false -> assert(false)
     end

let process_typ_def_aux_stub env typ_def_aux = match typ_def_aux with
    TD_abbrev(id,typquant,typ_arg) ->
     let string_id = string_of_id id in
     let string_typ_arg = process_typ_args_stub env typ_arg in
     "(* TD_abbrev *) " ^ "type " ^ string_id ^ ": " ^ string_typ_arg
  | TD_record(id,typquant,typ_id_list,bool) ->
     prerr_string "TD_record not implemented\n";
     ""
  | TD_variant(id,typquant,type_union_list,bool) ->
     prerr_string "TD_variant not implemented\n";
     ""
  | TD_enum(id, [],bool) -> "enum " ^ string_of_id id ^ " {}" 
  | TD_enum(id, id_h::id_t ,bool) ->
     let string_id = string_of_id id in
     let string_enum = List.fold_left
                         (fun acc id -> acc ^ "," ^ string_of_id id)
                         (string_of_id id_h) id_t in
     "enum " ^ string_id ^ " {" ^ string_enum ^ "}" 
  | TD_abstract(id,kind) -> assert(false)
  | TD_bitfield(id,typ, id_index_range_list) -> assert(false)

let process_def_aux_stub env output_chan (DEF_aux(aux,_)) = match aux with
    DEF_type(TD_aux(typ_def_aux,_)) ->
     begin
       match process_typ_def_aux_stub env typ_def_aux with
         "" -> ()
       | def -> output_string output_chan (def^"\n");
     end
     (* prerr_string "DEF_type not implemented\n"; *)
     (* () *)
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
  | DEF_register(DEC_aux(DEC_reg(reg,id,opt),_)) -> (* register declaration *)
     let string_id = string_of_id id in
     let string_register_definition = process_dec_spec env reg in
     let def = "register " ^ string_id ^ ": " ^ string_register_definition ^ "\n" in
     output_string output_chan def;
  | DEF_pragma(string0,string1,int) -> ()
  | DEF_internal_mutrec(fundef_list) -> ()

let process_nexp_aux (nexp_aux: nexp_aux): mlir_type = match nexp_aux with
    Nexp_id(id) -> NotImplemented
  | Nexp_var(Kid_aux(Var(v),_)) -> NotImplemented
  | Nexp_constant(num) -> Nop   (* normal *)
  | Nexp_app(id,nexp_list) -> NotImplemented (* TODO *)
  | Nexp_if(n_constraint,nexp0,nexp1) -> NotImplemented
  | Nexp_times(nexp0, nexp1) -> NotImplemented
  | Nexp_sum(nexp0,nexp1) -> NotImplemented
  | Nexp_minus(nexp0,nexp1) -> NotImplemented
  | Nexp_exp(nexp) -> NotImplemented
  | Nexp_neg(nexp) -> NotImplemented

let bind_defs_env defs_env name nexp_aux = match nexp_aux with
    Nexp_id(id) -> ()
  | Nexp_var(Kid_aux(Var(v),_)) -> ()
  | Nexp_constant(num) ->
     let mlir_type = match Nat_big_num.to_int(num) with
         2 -> Scalar(I2)
       | 4 -> Scalar(I4)
       | 8 -> Scalar(I8)
       | 16 -> Scalar(I16)
       | 32 -> Scalar(I32)
       | 64 -> Scalar(I64)
       | n -> MultiDim(Tensor([n],I2)) in
     bind defs_env name mlir_type
  | Nexp_app(id,nexp_list) -> () (* TODO *)
  | Nexp_if(n_constraint,nexp0,nexp1) -> ()
  | Nexp_times(nexp0, nexp1) -> ()
  | Nexp_sum(nexp0,nexp1) -> ()
  | Nexp_minus(nexp0,nexp1) -> ()
  | Nexp_exp(nexp) -> ()
  | Nexp_neg(nexp) -> ()

let process_typ_def_aux typs_env defs_env typ_def_aux = match typ_def_aux with
    TD_abbrev(id,typquant,A_aux(A_nexp(Nexp_aux(nexp_aux,_)),_)) ->
     (* numeric expression, of kind Int *)
     let name = string_of_id id in
     bind_defs_env defs_env name nexp_aux;
     begin
       match process_nexp_aux nexp_aux with
         Nop -> Nop
       | nexp -> TypeDef(name,nexp)
     end
  | TD_abbrev(id,typquant,A_aux(A_typ(t),_)) -> NotImplemented
  | TD_abbrev(id,typquant,A_aux(A_bool(NC_aux(nca,_)),_)) -> NotImplemented
  | TD_record(id,typquant,typ_id_list,bool) ->
     TypeDef(string_of_id id, NotImplemented)
  | TD_variant(id,typquant,type_union_list,bool) ->
     TypeDef(string_of_id id, NotImplemented)
  | TD_enum(id, [],bool) ->
     TypeDef(string_of_id id, NotImplemented)
  | TD_enum(id, id_h::id_t ,bool) ->
     TypeDef(string_of_id id, NotImplemented)
  | TD_abstract(id,kind) ->
     TypeDef(string_of_id id, NotImplemented)
  | TD_bitfield(id,typ, id_index_range_list) ->
     TypeDef(string_of_id id, NotImplemented)

let process_def_aux typs_env defs_env (DEF_aux(aux,_)) = match aux with
    DEF_type(TD_aux(typ_def_aux,_)) -> process_typ_def_aux typs_env defs_env typ_def_aux
  | DEF_constraint(atyp) -> NotImplemented
  | DEF_fundef(fundef) -> NotImplemented
  | DEF_mapdef(mapdef) -> NotImplemented (* mapping definition *)
  | DEF_impl(funcl) -> NotImplemented (* impl definition *)
  | DEF_let(letbind) -> NotImplemented (* value definition *)
  | DEF_overload(id,id_list) -> NotImplemented (* operator overload specifications *)
  | DEF_fixity(prec,num,id) -> NotImplemented (* fixity declaration *)
  | DEF_val(val_spec) -> NotImplemented (* top-level type constraint *)
  | DEF_outcome(outcome_spec,def_list) -> NotImplemented (* top-level outcome definition *)
  | DEF_instantiation(id,subst_list) -> NotImplemented (* instantiation *)
  | DEF_default(default_typing_spec) -> NotImplemented (* default kind and type assumptions *)
  | DEF_scattered(scattered_def) -> NotImplemented (* scattered definition *)
  | DEF_measure(id,pat,exp) -> NotImplemented (* separate termination measure declaration *)
  | DEF_loop_measures(id,loop_measure_list) -> NotImplemented (* separate termination measure declaration *)
  | DEF_register(DEC_aux(DEC_reg(reg,id,opt),_)) -> NotImplemented (* register declaration *)
  | DEF_pragma(string0,string1,int) -> NotImplemented
  | DEF_internal_mutrec(fundef_list) -> NotImplemented

(* Env: ~/src/upstream/sail/src/lib/type_env.ml *)
(* Ast: ./src/lib/type_check.mli:type typed_ast = (tannot, env) ast *)
let process_ast (typs_env: Env.t) (o: out_channel) ({defs = d; comments = _}: typed_ast) =
  let defs_env = { types_aliases = ref []} in
  let mlir_defs = List.map (process_def_aux typs_env defs_env) d in
  let strings_mlir_defs = List.map string_of_mlir_def mlir_defs in
  string_of_defs_env defs_env |> print_string;
  List.iter print_string strings_mlir_defs;
