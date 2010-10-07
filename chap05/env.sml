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

  val predefinedTypes = 
    [("int", T.INT)
    ,("string", T.STRING)
    ,("unit", T.UNIT) (* FIXME: remove 'unit' type - this is a hack *)
    ]

  fun enterTy((name, ty), tenv) = S.enter(tenv, S.symbol name, ty)
  val base_tenv = List.foldr enterTy S.empty predefinedTypes
 
  (* TODO: add runtime library functions *)
  val predefinedVars = 
    [("nil", VarEntry {ty=T.NIL})
    ]

  fun enterVar((name, enventry), venv) = S.enter (venv, S.symbol name, enventry)
  val base_venv = List.foldr enterVar S.empty predefinedVars
end
