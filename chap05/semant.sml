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
  structure T = Types

  open List

  type expty = {exp: Translate.exp, ty: T.ty}

  val todoAccess = ()
  val todoTy = T.INT
  val todoTrExp = ()
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=E.base_venv, tenv=E.base_tenv}

  val error = ErrorMsg.error

  (* TODO: allow type aliases/synonyms *)
  fun checkInt({exp, ty}, pos) =
    case ty
      of T.INT => ()
      | _ => error pos "integer required"

  (* TODO: allow type aliases/synonyms *)
  fun checkUnit({exp, ty}, pos) =
    case ty
      of T.UNIT => ()
      | _ => error pos "unit required"

  (* TODO: allow type aliases/synonyms *)
  fun reqSameType({exp=_, ty=ty1}, {exp=_, ty=ty2}, pos) =
    if ty1 <> ty2 then
      error pos "types do not match" (* TODO: better msg here *)
    else ()

  fun lookupTy(pos, tenv, ty) =
    case S.look(tenv, ty)
      of SOME ty => ty
       | NONE   => (error pos "type does not exist"; T.NIL)

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

  |  transDec(venv, tenv, A.TypeDec []) = {tenv=tenv, venv=venv}
  |  transDec(venv, tenv, A.TypeDec ({name, ty, pos}::decs)) =
      let
        val ty = case ty 
          of A.NameTy (sym, pos) => T.NAME (sym, ref NONE) (* XXX: what's the NAME type? with ty option ref? *)
           | A.RecordTy fields => T.RECORD ([(*TODO fields *)], ref ()) (* TODO: convert field *)
           | A.ArrayTy (sym, pos) => T.ARRAY (lookupTy(pos, tenv, sym), ref ())
      in
        transDec(venv, S.enter(tenv, name, ty), A.TypeDec decs)
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
  | AssignExp of {var: var, exp: exp, pos: pos}
  | ForExp of {var: symbol, escape: bool ref,
               lo: exp, hi: exp, body: exp, pos: pos}
  | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}
*)
      fun trexp(A.NilExp) = {exp=todoTrExp, ty=T.NIL}

        | trexp(A.ArrayExp {typ=tySymbol, size=sizeExp, init=initExp, pos}) =
          let
            val recordTy = lookupTy(pos, tenv, tySymbol)
            val sizeA = trexp sizeExp
            val initA = trexp initExp
          in
            checkInt(sizeA, pos);
            case recordTy
              of T.ARRAY (ty, unique) => 
                  (reqSameType(initA, {exp=(), ty=ty}, pos);
                   {exp=todoTrExp, ty=recordTy})
               | _ => (error pos "type is not an array";
                      {exp=todoTrExp, ty=T.UNIT})
          end

        | trexp(A.WhileExp {test=testExp, body=bodyExp, pos}) =
          let
            val testA = trexp testExp
            val bodyA = trexp bodyExp
          in
            checkInt(testA, pos);
            checkUnit(bodyA, pos);
            {exp=todoTrExp, ty=T.UNIT}
          end

        | trexp(A.IfExp {test=testExp, then'=thenExp, else'=SOME elseExp, pos}) =
          let
            val testA = trexp testExp
            val thenA = trexp thenExp
            val elseA = trexp elseExp
            val {exp=_, ty=resTy} = thenA
          in
            checkInt(testA, pos);
            reqSameType(thenA, elseA, pos);
            {exp=todoTrExp, ty=resTy}
          end

        | trexp(A.IfExp {test=testExp, then'=thenExp, else'=NONE, pos}) =
          let
            val testA = trexp testExp
            val thenA = trexp thenExp
            val {exp=_, ty=resTy} = thenA
          in
            checkInt(testA, pos);
            checkUnit(thenA, pos);
            {exp=todoTrExp, ty=T.UNIT}
          end

        | trexp(A.RecordExp {fields, typ, pos}) = 
          (
            (* TODO check fields match record type *)
            case S.look(tenv, typ) 
              of SOME t => {exp=todoTrExp, ty=t}
              |  NONE   => (error pos "record type does not exist"; {exp=todoTrExp, ty=T.NIL})
          )

        | trexp(A.IntExp _) = {exp=todoTrExp, ty=T.INT}
        | trexp(A.StringExp _) = {exp=todoTrExp, ty=T.STRING}
        | trexp(A.BreakExp pos) = {exp=todoTrExp, ty=T.UNIT}
        | trexp(A.OpExp{left, oper, right, pos}) =
          (checkInt(trexp left, pos);
           checkInt(trexp right, pos);
           {exp=todoTrExp, ty=T.INT})
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
                                       {exp=todoTrExp, ty=T.INT})
        )
        | trvar(A.FieldVar(var, sym, pos)) = todoExpTy
        | trvar(A.SubscriptVar(var, exp, pos)) = todoExpTy
    in
      trexp(exp)
    end

  and transTy (            tenv: E.tenv, ty: A.ty): T.ty = todoTy

  and transProg(exp: A.exp):unit =
    (transExp(E.base_venv, E.base_tenv, exp);
     ())

end
