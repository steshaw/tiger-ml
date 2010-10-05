structure Env : sig
  type access
  type ty

  datatype enventry 
    = VarEntry of {ty: ty}
    | FunEntry of {formals: ty list, result: ty}

  val base_tenv: ty Symbol.table (* predefined types *)
  val base_venv: enventry Symbol.table (* predefined functions (well, values...) *)
end = 
struct
  type ty = Types.ty
  type access = unit (* TODO *)

  datatype enventry 
    = VarEntry of {ty: ty}
    | FunEntry of {formals: ty list, result: ty}

  val base_tenv = Symbol.empty (* TODO *)
  val base_venv = Symbol.empty (* TODO *)
end
