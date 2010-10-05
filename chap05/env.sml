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
  structure T = Types

  type access = unit (* TODO *)

  datatype enventry 
    = VarEntry of {ty: T.ty}
    | FunEntry of {formals: T.ty list, result: T.ty}

  type tenv = T.ty S.table
  type venv = enventry S.table

  val base_tenv = S.enter(S.enter(S.empty, S.symbol "int", T.INT), S.symbol "string", T.STRING)
    (* TODO: use fold here *)
      
  val base_venv = S.empty (* TODO *)
end
