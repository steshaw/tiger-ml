structure Semant:
sig
  val transProg: Absyn.exp -> unit

  (* XXX: Don't think these declaration below really need exporting. *)
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

  open List

  type expty = {exp: Translate.exp, ty: Types.ty}

  val todoAccess = ()
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

  and transDec(venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
    let val {exp=_ (*TODO*), ty} = transExp(venv, tenv, init)
    in {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=ty})}
    end

  |   transDec(venv, tenv, A.VarDec {name, escape, typ=SOME(symbol, decTyPos), init, pos}) =
    let val {exp=_ (*TODO*), ty} = transExp(venv, tenv, init)
        val SOME(decTy) = S.look(tenv, symbol)
    in (
      if decTy <> ty then
        (error pos "declared type does not match inferred type"; (* TODO: include types etc in message *)
        ())
      else ();
      {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=decTy})} (* continue with declared type *)
    )
    end

  |  transDec(venv, tenv, A.TypeDec typeDecList) =
    let
      fun transTyDec({name, ty, pos}) =
        {tenv=S.enter(tenv, name, ty), venv=venv}
    in
        {tenv=tenv, venv=venv} (* TODO fold over the typeDecList here *)
    end

  |  transDec(venv, tenv, A.FunctionDec[{name, params, body, pos, result=SOME(resTySy, resPos)}]) =
    (* TODO: Much left out here see MCI/ML p119 *)
    let
      val SOME(resTy) = S.look(tenv, resTySy)
      fun transParam {name, escape, typ, pos} =
        case S.look(tenv, typ) of SOME t => {name=name, ty=t}
      val params' = map transParam params (* map [ty] => [(name, ty)] *)
      val venv' = S.enter(venv, name, E.FunEntry {formals=map #ty params', result=resTy})
      fun enterParam({name, ty}, venv) =
        S.enter(venv, name, E.VarEntry {(*access=todoAccess,*) ty=ty})
      val venv'' = foldl enterParam venv' params'
          (* XXX: book had fold instead of foldl and I had to reverse the last two args. Should this be a foldr? *)
    in
      transExp(venv'', tenv, body);
      {venv=venv', tenv=tenv}
    end

  and transDecs(venv, tenv, []) = {venv=venv, tenv=tenv}
    | transDecs(venv, tenv, dec::decs) =
      let val {tenv=tenv', venv=venv'} = transDec(venv, tenv, dec)
      in transDecs(venv', tenv', decs)
      end

  and transExp(venv, tenv, exp) =
    let
(*
  | CallExp of {func: symbol, args: exp list, pos: pos}
  | RecordExp of {fields: (symbol * exp * pos) list,
                  typ: symbol, pos: pos}
  | SeqExp of (exp * pos) list
  | AssignExp of {var: var, exp: exp, pos: pos}
  | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
  | WhileExp of {test: exp, body: exp, pos: pos}
  | ForExp of {var: symbol, escape: bool ref,
               lo: exp, hi: exp, body: exp, pos: pos}
  | BreakExp of pos
  | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}
*)
      fun trexp(A.NilExp) = {exp=todoTrExp, ty=Types.NIL}
        | trexp(A.IntExp _) = {exp=todoTrExp, ty=Types.INT}
        | trexp(A.StringExp _) = {exp=todoTrExp, ty=Types.STRING}
        | trexp(A.OpExp{left, oper, right, pos}) =
          (checkInt(trexp left, pos);
           checkInt(trexp right, pos);
           {exp=todoTrExp, ty=Types.INT})
        | trexp(A.RecordExp {fields, typ, pos}) = todoExpTy
        | trexp(A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            in transExp(venv', tenv', body) (* book has transExp(venv', tenv') body *)
            end
        | trexp(A.SeqExp expList) = 
            let 
              val rs = map trexp (map #1 expList)
              val {exp=_, ty=lastTy} = last rs
            in {exp=todoTrExp, ty=lastTy}
            end
            
      and trvar(A.SimpleVar(id, pos)) =
        (case S.look(venv, id)
          of SOME(E.VarEntry {ty}) => {exp=todoTrExp, ty=actual_ty ty}
          |  NONE                  => (error pos ("undefined variable " ^ S.name id);
                                       {exp=todoTrExp, ty=Types.INT})
        )
        | trvar(A.FieldVar(var, sym, pos)) = todoExpTy
        | trvar(A.SubscriptVar(var, exp, pos)) = todoExpTy
    in
      trexp(exp)
    end

  and transTy (            tenv: E.tenv, ty: A.ty): Types.ty = todoTy

  and transProg(exp: A.exp):unit =
    (transExp(E.base_venv, E.base_tenv, exp);
     ())

end
