structure Semant :
sig
  val transProg: Absyn.exp -> unit

  (* XXX: Don't think these function below really need exporting. Just need their types defined *)
  (* XXX: because I'm using pattern matching to define them below *)
  type expty
  type tenv
  type venv
  val transExp: venv * tenv * Absyn.exp -> expty
end =
struct
  structure A = Absyn
  structure S = Symbol
  structure E = Env

  val error = ErrorMsg.error
  val todoTrExp = ()

  type ty = Types.ty
  type expty = {exp: Translate.exp, ty: Types.ty}

  type tenv = ty S.table
  type venv = E.enventry S.table

  val todoTy = Types.INT
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=E.base_venv, tenv=E.base_tenv}

  fun transProg(exp: A.exp):unit = ()


  fun checkInt({exp, ty}, pos) = 
      case ty
        of Types.INT => ()
        | _ => error pos "integer required";

  fun transVar(venv: venv, tenv: tenv, var: A.var): expty = todoExpTy

  fun actual_ty(ty) : ty = todoTy (* TODO *)

  fun transDecs(venv, tenv, decs) = {venv=venv, tenv=tenv} (* TODO *)

  fun transExp(venv, tenv, exp) = 
    let
      fun trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=todoTrExp, ty=Types.INT})
      |   trexp(A.RecordExp {fields, typ, pos}) = todoExpTy
      |   trexp(A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            in transExp(venv', tenv', body) (* book has transExp(venv', tenv') body *)
            end
      and trvar(A.SimpleVar(id, pos)) =
        (case S.look(venv, id)
          of SOME(E.VarEntry {ty}) => {exp=todoTrExp, ty=actual_ty ty}
          |  NONE                  => (error pos ("undefined variable " ^ S.name id);
                                       {exp=todoTrExp, ty=Types.INT})
        )
      |  trvar(A.FieldVar(v, id, pos)) = todoExpTy
    in
      trexp(exp)
    end


  fun transDec(venv: venv, tenv: tenv, dec: A.dec): {venv: venv, tenv: tenv} = todoDecValEntTyEnv

  fun transTy (            tenv: tenv, ty: A.ty): Types.ty = todoTy

end
