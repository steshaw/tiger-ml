structure Semant : 
sig 
  val transProg: Absyn.exp -> unit 

  (* XXX: Don't think these function below really need exporting. Just need their types defined *)
  (* XXX: because I'm using pattern matching to define them below *)
  type expty
  type tenv
  type venv
  val transExp: venv * tenv * Absyn.exp -> expty;
end =
struct
  structure A = Absyn

  val error = ErrorMsg.error
  val todoTrExp = ()

  type ty = Types.ty
  type expty = {exp: Translate.exp, ty: Types.ty}

  type tenv = ty Symbol.table
  type venv = Env.enventry Symbol.table

  val todoTy = Types.INT
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=Env.base_venv, tenv=Env.base_tenv}

  fun transProg(exp: A.exp):unit = ()


  fun checkInt({exp, ty}, pos) = 
      case ty
        of Types.INT => ()
        | _ => error pos "integer required";

  fun transVar(venv: venv, tenv: tenv, var: A.var): expty = todoExpTy

  fun transExp(venv, tenv, exp) = 
    let
      fun trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=todoTrExp, ty=Types.INT})
    in
      trexp(exp)
    end
    
  fun transDec(venv: venv, tenv: tenv, dec: A.dec): {venv: venv, tenv: tenv} = todoDecValEntTyEnv

  fun transTy (            tenv: tenv, ty: A.ty): Types.ty = todoTy;

end
