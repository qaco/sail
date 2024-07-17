type id = string

type mlir_builtin_scalar_types = I2 | I4 | I8 | I16 | I32 | I64

type dim = Question | Size of int

type general_dims = Poly | Dims of dim list

type mlir_builtin_multidim_types =
  Tensor of general_dims * mlir_builtin_scalar_types
| MemRef of general_dims * mlir_builtin_scalar_types

type mlir_builtin_types =
  Scalar of mlir_builtin_scalar_types
| MultiDim of mlir_builtin_multidim_types

type num = Lit of int | FreeVar of id | Var of id

type mlir_type =
  Builtin of mlir_builtin_types
| Num of num
| Typ_id of id
| Typ_parameterized of id * mlir_type list 
| NotImplemented of string

type mlir_type_alias = string * mlir_type
type mlir_types_aliases = mlir_type_alias list
type defs_env = { types_aliases: mlir_types_aliases ref}
let bind defs_env name mlir_type =
  defs_env.types_aliases := (name,mlir_type) :: !(defs_env.types_aliases)

(* type mlir_type_def = *)
(*   Nop *)
(* | NumExp of mlir_num_exp *)
(* | NotImplemented of string *)

type mlir_def =
  Nop
| TypeDef of id * mlir_type
| RegDef of id * mlir_type
| NotImplemented of string

let string_of_mlir_builtin_scalar_type t = match t with
  I2 -> "i2"
| I4 -> "i4"
| I8 -> "i8"
| I16 -> "i16"
| I32 -> "i32"
| I64 -> "i64"

let string_of_dim d = match d with
    Question -> "?"
  | Size i -> string_of_int i

let string_of_general_dims ds = match ds with
    Poly -> "*"
  | Dims [] -> assert(false)
  | Dims(h::t) ->
     string_of_dim h ^ List.fold_left (fun acc e -> acc ^ "x" ^ string_of_dim e) "" t

let string_of_mlir_builtin_multidim_type t = match t with
    Tensor(il,t) ->
     "tensor<"
     ^ string_of_general_dims il
     ^ "x"
     ^ string_of_mlir_builtin_scalar_type t
     ^ ">"
  | MemRef(il,t) -> assert(false)

let string_of_mlir_builtin_type t = match t with
    Scalar t -> string_of_mlir_builtin_scalar_type t
  | MultiDim t -> string_of_mlir_builtin_multidim_type t

let string_of_num n = match n with
    Lit l -> string_of_int l
  | FreeVar v | Var v -> v

let rec string_of_mlir_type t = match t with
    Builtin t' -> string_of_mlir_builtin_type t'
  | Num i -> string_of_num i
  | Typ_id id -> id
  | Typ_parameterized(id, params_list) ->
     let string_params_list = match List.map string_of_mlir_type params_list with
       | [] -> ""
       | h::t -> h ^ (List.fold_left (fun acc x -> acc^","^x) "" t) in
     id ^ "(" ^ string_params_list ^ ")"
  | NotImplemented s -> "NotImplemented(" ^ s ^ ")"

let string_of_mlir_def mlir_def = match mlir_def with
    Nop -> ""
  | NotImplemented e ->
     ""
     (* "NotImplemented(" ^ e ^ ")\n" *)
  | TypeDef(id,e) -> "type " ^ id ^ ": " ^ string_of_mlir_type(e) ^ "\n"
  | RegDef(id,mlir_type) ->
     "register " ^ id ^ ": " ^ string_of_mlir_type mlir_type ^ "\n"

let string_of_defs_env defs_env =
  "types_aliases = {\n"
  ^ List.fold_left
    (fun acc (str,ty) -> acc ^ "  " ^ str ^ ": " ^ string_of_mlir_type ty ^ "\n")
    ""
    !(defs_env.types_aliases)
  ^ "}\n"
