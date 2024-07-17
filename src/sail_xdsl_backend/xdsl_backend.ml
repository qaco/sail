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

let process_nexp_aux (nexp_aux: nexp_aux): mlir_type = match nexp_aux with
    Nexp_id(id) -> Num(Var(string_of_id id))
  | Nexp_var(Kid_aux(Var(v),_)) -> Num(FreeVar v)
  | Nexp_constant(num) -> Num(Lit(Nat_big_num.to_int(num)))
  | Nexp_app(id,nexp_list) -> NotImplemented("Nexp_app")
  | Nexp_if(n_constraint,nexp0,nexp1) -> NotImplemented("Nexp_if")
  | Nexp_times(nexp0, nexp1) -> NotImplemented("Nexp_times")
  | Nexp_sum(nexp0,nexp1) -> NotImplemented("Nexp_sum")
  | Nexp_minus(nexp0,nexp1) -> NotImplemented("Nexp_minus")
  | Nexp_exp(nexp) -> NotImplemented("Nexp_exp")
  | Nexp_neg(nexp) -> NotImplemented("Nexp_neg")

let rec process_dec_spec typs_env (Typ_aux(reg,_)): mlir_type = match reg with
    Typ_id id -> Typ_id (string_of_id id)
  | Typ_var(kid) -> NotImplemented("Typ_var")
  | Typ_app(id,typ_arg_list) ->
     let mlir_types_list = List.map (process_typ_arg typs_env) typ_arg_list in
     Typ_parameterized(string_of_id id, mlir_types_list)
  | Typ_fn(id_in,typ_list) -> NotImplemented("Typ_fn")
  | Typ_bidir(typ1,typ2) -> NotImplemented("Typ_bidir")
  | Typ_tuple(typ_list) -> NotImplemented("Typ_tuple")
  | Typ_exist(kinded_id_list,typ0,typ1) -> NotImplemented("Typ_exist")
  | Typ_internal_unknown -> NotImplemented("Typ_internal_unknown")
and process_typ_arg typs_env (A_aux(typ_arg_aux, _)): mlir_type = match typ_arg_aux with
    A_nexp(Nexp_aux(nexp_aux,_)) -> (* numeric expression, of kind Int *)
     process_nexp_aux nexp_aux
  | A_typ(t) -> process_dec_spec typs_env t
  | A_bool(NC_aux(nca,_)) -> NotImplemented("DEF_type/TD_abbrev/NC_aux")
    
let process_typ_def_aux typs_env typ_def_aux = match typ_def_aux with
    TD_abbrev(id,typquant,typ_arg) ->
     let name = string_of_id id in
     TypeDef(name,process_typ_arg typs_env typ_arg)
     (* NotImplemented("DEF_type/TD_abbrev/A_typ") *)
  | TD_record(id,typquant,typ_id_list,bool) ->
     TypeDef(string_of_id id, NotImplemented("DEF_type/TD_record"))
  | TD_variant(id,typquant,type_union_list,bool) ->
     TypeDef(string_of_id id, NotImplemented("DEF_type/TD_variant"))
  | TD_enum(id, id_list ,bool) ->
     TypeDef(string_of_id id, NotImplemented("DEF_type/TD_enum"))
  | TD_abstract(id,kind) ->
     TypeDef(string_of_id id, NotImplemented("DEF_type/TD_abstract"))
  | TD_bitfield(id,typ, id_index_range_list) ->
     TypeDef(string_of_id id, NotImplemented("DEF_type/TD_bitfield"))

let process_def_aux typs_env (DEF_aux(aux,_)) = match aux with
    DEF_type(TD_aux(typ_def_aux,_)) -> process_typ_def_aux typs_env typ_def_aux
  | DEF_register(DEC_aux(DEC_reg(reg,id,_),_)) -> (* register declaration *)
     let name = string_of_id id in
     RegDef(name, process_dec_spec typs_env reg)
  | DEF_constraint(atyp) -> NotImplemented("DEF_constraint")
  | DEF_fundef(fundef) -> NotImplemented("DEF_fundef")
  | DEF_mapdef(mapdef) -> NotImplemented("DEF_mapdef") (* mapping definition *)
  | DEF_impl(funcl) -> NotImplemented("DEF_impl") (* impl definition *)
  | DEF_let(letbind) -> NotImplemented("DEF_let") (* value definition *)
  | DEF_overload(id,id_list) -> NotImplemented("DEF_overload") (* operator overload specifications *)
  | DEF_fixity(prec,num,id) -> NotImplemented("DEF_fixity") (* fixity declaration *)
  | DEF_val(val_spec) -> NotImplemented("DEF_val") (* top-level type constraint *)
  | DEF_outcome(outcome_spec,def_list) -> NotImplemented("DEF_outcome") (* top-level outcome definition *)
  | DEF_instantiation(id,subst_list) -> NotImplemented("DEF_instantiation") (* instantiation *)
  | DEF_default(default_typing_spec) -> NotImplemented("DEF_default") (* default kind and type assumptions *)
  | DEF_scattered(scattered_def) -> NotImplemented("DEF_scattered") (* scattered definition *)
  | DEF_measure(id,pat,exp) -> NotImplemented("DEF_measure") (* separate termination measure declaration *)
  | DEF_loop_measures(id,loop_measure_list) -> NotImplemented("DEF_loop_measures") (* separate termination measure declaration *)
  | DEF_pragma(string0,string1,int) -> NotImplemented("DEF_pragma")
  | DEF_internal_mutrec(fundef_list) -> NotImplemented("DEF_internal_mutrec")

(* Env: ~/src/upstream/sail/src/lib/type_env.ml *)
(* Ast: ./src/lib/type_check.mli:type typed_ast = (tannot, env) ast *)
let process_ast (typs_env: Env.t) (o: out_channel) ({defs = d; comments = _}: typed_ast) =
  let mlir_defs = List.map (process_def_aux typs_env) d in
  let strings_mlir_defs = List.map string_of_mlir_def mlir_defs in
  List.iter print_string strings_mlir_defs;
