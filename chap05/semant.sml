structure Semant : 
sig 
  val transProg: Absyn.exp -> unit 

  (* XXX: Don't think these function below really need exporting. Just need their types defined *)
  (* XXX: because I'm using pattern matching to define them below *)
  (*val transExp: venv * tenv * A.exp -> expty; *)
end =
struct
  structure A = Absyn

  val error = ErrorMsg.error
  val todoTrExp = ()

  type ty = Types.ty

  type tenv = ty Symbol.table
  type venv = Env.enventry Symbol.table

  val todoTy = Types.INT
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=Env.base_venv, tenv=Env.base_tenv}

  fun transProg(exp: A.exp):unit = ()

  type expty = {exp: Translate.exp, ty: Types.ty}

  fun checkInt({exp, ty}, pos) = 
      case ty
        of Types.INT => ()
        | _ => error pos "integer required";

  fun transVar(venv: venv, tenv: tenv, var: A.var): expty = todoExpTy

(*
  fun transExp(venv, tenv) = let
    fun trexp(A.OpExp{left, oper=A.PlusOp, right, pos})
*)
    
  fun transExp(venv, tenv, A.OpExp {left, oper=A.PlusOp, right, pos}) =
    let
      val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
      val {exp=_, ty=tyright} = transExp(venv, tenv, right)
    in
      case tyleft 
        of Types.INT => ()
        | _ => error pos "integer required";
      case tyright
        of Types.INT => ()
        | _ => error pos "integer required";
      {exp=todoTrExp, ty=Types.INT}
    end

  fun transDec(venv: venv, tenv: tenv, dec: A.dec): {venv: venv, tenv: tenv} = todoDecValEntTyEnv

  fun transTy (            tenv: tenv, ty: A.ty): Types.ty = todoTy;

end
