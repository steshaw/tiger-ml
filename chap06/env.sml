structure Env: sig
  type access

  type venv
  type tenv

  datatype enventry
    = VarEntry of {access: TL.access, ty: Types.ty}
    | FunEntry of {
        level: TL.level,
        label: Temp.label,
        formals: Types.ty list,
        result: Types.ty
      }

  val base_tenv: tenv (* predefined types *)
  val base_venv: venv (* predefined functions (well, values...) *)
end =
struct
  structure S = Symbol
  structure T = Types

  type access = unit (* TODO *)

  datatype enventry
    = VarEntry of {access: TL.access, ty: Types.ty}
    | FunEntry of {
        level: TL.level,
        label: Temp.label,
        formals: Types.ty list,
        result: Types.ty
      }

  type tenv = T.ty S.table
  type venv = enventry S.table

  val predefinedTypes =
    [("int", T.INT)
    ,("string", T.STRING)
    ,("unit", T.UNIT) (* FIXME: remove 'unit' type - this is a hack *)
    ]

  fun enterTy((name, ty), tenv) = S.enter(tenv, S.symbol name, ty)
  val base_tenv = List.foldr enterTy S.empty predefinedTypes

  fun globalFun name formals result =
    (name, FunEntry {level=TL.outermostLevel,
                     label=Temp.namedLabel name,
                     formals=formals,
                     result=result})
 
  val predefinedVars =
    [("nil", VarEntry {access=TL.globalAccess, ty=T.NIL})
    ,globalFun "print"  [T.STRING]  T.UNIT
    ,globalFun "flush" [] T.UNIT
    ,globalFun "getchar" [] T.STRING
    ,globalFun "ord" [T.STRING] T.INT
    ,globalFun "chr" [T.INT] T.STRING
    ,globalFun "size" [T.STRING] T.INT
    ,globalFun "substring" [T.STRING, T.INT, T.INT] T.STRING
    ,globalFun "concat" [T.STRING, T.STRING] T.STRING
    ,globalFun "not" [T.INT] T.INT (* TODO: Tiger doesn't include a boolean type. Would be useful here. *)
    ,globalFun "exit" [T.INT] T.UNIT
    ]

  fun enterVar((name, enventry), venv) = S.enter (venv, S.symbol name, enventry)
  val base_venv = List.foldr enterVar S.empty predefinedVars
end
