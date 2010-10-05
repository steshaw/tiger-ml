structure Semant : sig val transProg: Absyn.exp -> unit end =
struct
  structure A = Absyn

  type ty = Types.ty

  type tenv = ty Symbol.table
  type venv = Env.enventry Symbol.table

  val todoTy = Types.INT
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=Env.base_venv, tenv=Env.base_tenv}

  fun transProg(exp: A.exp):unit = ()

  type expty = {exp: Translate.exp, ty: Types.ty}

  fun transVar(venv: venv, tenv: tenv, var: A.var): expty = todoExpTy
  fun transExp(venv: venv, tenv: tenv, exp: A.exp): expty = todoExpTy
  fun transDec(venv: venv, tenv: tenv, dec: A.dec): {venv: venv, tenv: tenv} = todoDecValEntTyEnv
  fun transTy (            tenv: tenv, ty: A.ty): Types.ty = todoTy;


end
