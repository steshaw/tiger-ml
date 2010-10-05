structure Env : sig
  type access

  type venv
  type tenv

  datatype enventry 
    = VarEntry of {ty: Types.ty}
    | FunEntry of {formals: Types.ty list, result: Types.ty}

  val base_tenv: tenv (* predefined types *)
  val base_venv: venv (* predefined functions (well, values...) *)
end = 
struct
  structure S = Symbol

  type access = unit (* TODO *)

  datatype enventry 
    = VarEntry of {ty: Types.ty}
    | FunEntry of {formals: Types.ty list, result: Types.ty}

  type tenv = Types.ty S.table
  type venv = enventry S.table

  val base_tenv = S.empty (* TODO *)
  val base_venv = S.empty (* TODO *)
end
