structure Semant:
sig
  val transProg: Absyn.exp -> unit

  (* XXX: Don't think these function below really need exporting. Just need their types defined *)
  (* XXX: because I'm using pattern matching to define them below *)
  type expty

  val transVar: Env.venv * Env.tenv * Absyn.var -> expty
  val transExp: Env.venv * Env.tenv * Absyn.exp -> expty
  val transDec: Env.venv * Env.tenv * Absyn.dec -> {venv: Env.venv, tenv: Env.tenv}
  val transTy:             Env.tenv * Absyn.ty  -> Types.ty
end =
struct
  structure A = Absyn
  structure S = Symbol
  structure E = Env

  type expty = {exp: Translate.exp, ty: Types.ty}

  val todoTy = Types.INT
  val todoTrExp = ()
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=E.base_venv, tenv=E.base_tenv}

  val error = ErrorMsg.error

  fun checkInt({exp, ty}, pos) =
      case ty
        of Types.INT => ()
        | _ => error pos "integer required"

  and transVar(venv, tenv, var) = todoExpTy

  and actual_ty(ty) = todoTy (* TODO *)


(*
  and transDec(venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) = {venv=venv, tenv=tenv}
*)
  and transDec(venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
    let val {exp=_ (*TODO*), ty} = transExp(venv, tenv, init)
    in {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=ty})}
    end
(* TODO other VarDec + FunctionDec and TypeDec *)
(*
    | transDec(venv, tenv, A.VarDec {name, escape, SOME(symbol, pos), init, pos}) =
    let val {trExp, ty} = transExp(init)
    in {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=ty})}
    end
*)
(*
  = FunctionDec of fundec list
  | VarDec of {name: symbol,
               escape: bool ref,
               typ: (symbol * pos) option,
               init: exp,
               pos: pos}
  | TypeDec of {name: symbol, ty: ty, pos: pos} list
*)

  and transDecs(venv, tenv, []) = {venv=venv, tenv=tenv}
    | transDecs(venv, tenv, dec::decs) =
        let val {tenv=tenv', venv=venv'} = transDec(venv, tenv, dec)
        in transDecs(venv', tenv', decs)
        end

  and transExp(venv, tenv, exp) =
    let
      fun trexp(A.OpExp{left, oper=A.PlusOp, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=todoTrExp, ty=Types.INT})
        | trexp(A.RecordExp {fields, typ, pos}) = todoExpTy
        | trexp(A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            in transExp(venv', tenv', body) (* book has transExp(venv', tenv') body *)
            end
      and trvar(A.SimpleVar(id, pos)) =
        (case S.look(venv, id)
          of SOME(E.VarEntry {ty}) => {exp=todoTrExp, ty=actual_ty ty}
          |  NONE                  => (error pos ("undefined variable " ^ S.name id);
                                       {exp=todoTrExp, ty=Types.INT})
        )
        | trvar(A.FieldVar(v, id, pos)) = todoExpTy
    in
      trexp(exp)
    end

  and transTy (            tenv: E.tenv, ty: A.ty): Types.ty = todoTy

  and transProg(exp: A.exp):unit = (transExp(E.base_venv, E.base_tenv, exp); ())

end
