structure Semant:
sig
  val transProg: Absyn.exp -> unit
end =
struct
  structure A = Absyn
  structure P = PrintAbsyn
  structure S = Symbol
  structure E = Env
  structure T = Types

  (* FIXME: Loops in type aliases cause non-halting type check phase *)
  (* FIXME: Reject multiple definitions with same type name. When consecutive only - otherwise shadows. *)
  (* FIXME: Reject multiple definitions with same variable name? or shadows? *)

  (* TODO: Consider adding boolean type to the language *)

  type expty = {exp: Translate.exp, ty: T.ty}

  val todoAccess = ()
  val todoTy = T.INT
  val todoTrExp = ()
  val todoExpTy = {exp=(), ty=todoTy}
  val todoDecValEntTyEnv = {venv=E.base_venv, tenv=E.base_tenv}

  (* Value to use when expression is in error and no better value/ty can be provided *)
  val errorTrExpTy = {exp=todoTrExp, ty=T.NIL}
  val errorTrExp = ()

  val wouldBeBooleanTy = T.INT

  val error = ErrorMsg.error

  fun lookupActualType(pos, tenv, ty) =
    case S.look(tenv, ty)
      of SOME ty => actual_ty(pos, tenv, ty)
       | NONE   => (error pos ("Type '" ^ S.name ty ^ "' is not defined"); T.NIL)

  and actual_ty(pos, tenv, T.NAME (sym, ref(SOME(ty)))) = actual_ty(pos, tenv, ty)
    | actual_ty(pos, tenv, T.NAME (sym, ref(NONE))) = (error pos ("type '" ^ S.name sym ^ "' is still NONE"); T.NIL)
    | actual_ty(pos, tenv, ty) = ty

  fun checkInt({exp, ty}, pos) =
    case ty
      of T.INT => ()
      | _ => error pos "Type 'int' required"

  fun checkUnit({exp, ty}, pos) =
    case ty
      of T.UNIT => ()
      | _ => error pos "unit required"

  (* TODO: allow records to be compatible with NIL from either left or right? *)
  fun reqSameType(pos, tenv, {exp=_, ty=ty1}, {exp=_, ty=ty2}) =
    let val t1 = actual_ty(pos, tenv, ty1)
        val t2 = actual_ty(pos, tenv, ty2)
    in
      if t1 <> t2 then
        case t1
          of T.RECORD _ => if t2 = T.NIL then () else (error pos "types do not match"; ())
           | _          => error pos "types do not match"
      else ()
    end

  fun findVarType(tenv, venv, A.SimpleVar (sym, pos)) =
    (case S.look(venv, sym)
      of SOME(E.VarEntry {ty}) => actual_ty (pos, tenv, ty)
       | SOME(E.FunEntry _) => (error pos "Cannot assign to a function"; T.NIL)
       | _ => (error pos "Variable does not exist"; T.NIL)
    )

    | findVarType(tenv, venv, A.FieldVar (var, sym, pos)) =
      let
        val ty = findVarType(tenv, venv, var) (* Lookup type of nested var. It should be a record type. *)
      in
        case ty
          of T.RECORD (fields, unique) =>
            (case List.find (fn (fSym, fTy) => fSym = sym) fields
              of SOME(sym, ty) => actual_ty (pos, tenv, ty)
              |  NONE          => (error pos ("field " ^ S.name sym ^ " does not exist"); T.NIL)
            )
          |  _                       => (error pos "Variable is not a record"; T.NIL)
      end

    | findVarType(tenv, venv, A.SubscriptVar (var, exp, pos)) =
      let
        (* Lookup type of nested var. It should be an array type. *)
        val ty = findVarType(tenv, venv, var)
        val expA = transExp(venv, tenv, exp)
      in
        case ty
          of T.ARRAY (ty, unique) => (reqSameType(pos, tenv, expA, {exp=(), ty=T.INT}); actual_ty (pos, tenv, ty))
          |  _                    => (error pos "Variable is not an array"; T.NIL)
      end

  and transVar(venv, tenv, var) = todoExpTy

  and transDec(venv, tenv, A.VarDec {name, escape, typ=NONE, init, pos}) =
    let val {exp=_ (*TODO*), ty} = transExp(venv, tenv, init)
    in {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=ty})}
    end

  |   transDec(venv, tenv, A.VarDec {name, escape, typ=SOME(symbol, decTyPos), init, pos}) =
    let val {exp=_ (*TODO*), ty} = transExp(venv, tenv, init)
        val decTy = lookupActualType(pos, tenv, symbol)
    in
      reqSameType(pos, tenv, {exp=(), ty=decTy}, {exp=(), ty=ty});
      {tenv=tenv, venv=S.enter(venv, name, E.VarEntry {ty=decTy})} (* continue with declared type *)
    end

  | transDec(venv, tenv, A.TypeDec typeDecs) =
    let
      fun updateDecs (venv, tenv) =
        let
          fun updateDec {name, ty, pos} = 
            let
              fun lookupType(pos, tenv, ty) =
                case S.look(tenv, ty)
                  of SOME ty => ty
                   | NONE   => (error pos ("Type '" ^ S.name ty ^ "' is not defined"); T.NIL)
              val T.NAME(tyName, tyRef) = lookupType (pos, tenv, name)
              val ty = case ty
                of A.NameTy (name, pos) => 
                    T.NAME (name, ref (SOME (lookupType (pos, tenv, name))))
                 | A.RecordTy fields => 
                    T.RECORD (map (fn ({name, escape, typ, pos}) => (name, lookupType (pos, tenv, typ))) fields, ref ())
                 | A.ArrayTy (name, pos) => 
                    T.ARRAY (lookupType (pos, tenv, name), ref ())
            in
              tyRef := SOME(ty)
            end
        in
          app updateDec typeDecs
        end

      fun enterTypeHeader ({name, ty, pos}, tenv) = S.enter (tenv, name, T.NAME (name, ref NONE))
      val tenv' = foldl enterTypeHeader tenv typeDecs
    in
      updateDecs (venv, tenv');
      {tenv=tenv', venv=venv}
    end

  |  transDec(venv, tenv, A.FunctionDec funDecs) =
    let
      fun computeResultType (tenv, result) =
        (case result
           of SOME (resTySym, resPos) => lookupActualType (resPos, tenv, resTySym)
            | NONE => T.UNIT)

      fun transFunDecs(venv, tenv, funDecs) =
        let
          fun transFunDec({name, params, body, pos, result}) =
            let
              val resTy = computeResultType (tenv, result)
              fun transParam {name, escape, typ, pos} = {name=name, ty=lookupActualType(pos, tenv, typ)}
              val params' = map transParam params (* map [ty] => [{name, ty}] *)
              fun enterParam({name, ty}, venv) = S.enter(venv, name, E.VarEntry {(*access=todoAccess,*) ty=ty})
              val venv' = foldl enterParam venv params'
              val bodyA = transExp(venv', tenv, body);
            in
              reqSameType(pos, tenv, bodyA, {exp=(), ty=resTy})
            end
        in
          app transFunDec funDecs
        end

      fun enterFunHeader({name, params, body, pos, result}, venv) =
        let
          val resTy = computeResultType (tenv, result)
          fun transParam {name, escape, typ, pos} = {name=name, ty=lookupActualType(pos, tenv, typ)}
          val params' = map transParam params (* map [ty] => [{name, ty}] *)
        in
          S.enter(venv, name, E.FunEntry {formals=map #ty params', result=resTy})
        end
      val venv' = foldl enterFunHeader venv funDecs
    in
      transFunDecs (venv', tenv, funDecs);
      {venv=venv', tenv=tenv}
    end

  and transDecs(venv, tenv, []) = {venv=venv, tenv=tenv}
    | transDecs(venv, tenv, dec::decs) =
      let val {tenv=tenv', venv=venv'} = transDec(venv, tenv, dec)
      in transDecs(venv', tenv', decs)
      end

  and transExp(venv, tenv, exp) =
    let
      fun trexp(A.NilExp) = {exp=todoTrExp, ty=T.UNIT}

        | trexp(A.VarExp var) = {exp=todoTrExp, ty=findVarType(tenv, venv, var)}

        | trexp(A.AssignExp {var, exp, pos}) =
          let
            val expA = trexp exp
            val varTy = findVarType(tenv, venv, var)
          in
            reqSameType(pos, tenv, {exp=(), ty=varTy}, expA);
            {exp=todoTrExp, ty=T.UNIT}
          end

        | trexp(A.ArrayExp {typ=tySymbol, size=sizeExp, init=initExp, pos}) =
          let
            val arrayTy = lookupActualType(pos, tenv, tySymbol)
            val sizeA = trexp sizeExp
            val initA = trexp initExp
          in
            checkInt(sizeA, pos);
            case arrayTy
              of T.ARRAY (ty, unique) =>
                  (reqSameType(pos, tenv, {exp=(), ty=ty}, initA);
                   {exp=todoTrExp, ty=arrayTy})
               | _ => (error pos "type is not an array";
                      {exp=todoTrExp, ty=T.UNIT})
          end

        | trexp(A.ForExp {var=varSym, escape, lo, hi, body, pos}) =
          let
            val loA = trexp lo
            val hiA = trexp hi
            val venv'=S.enter(venv, varSym, E.VarEntry {ty=T.INT}) (* declare loop variable *)
            (* TODO: ensure loop variable is not assigned to *)
            val bodyA = transExp (venv', tenv, body)
          in
            checkInt(loA, pos);
            checkInt(hiA, pos);
            checkUnit(bodyA, pos);
            {exp=todoTrExp, ty=T.UNIT}
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

        | trexp(A.CallExp {func, args, pos}) =
          let
          in
            case S.look(venv, func)
              of SOME(E.VarEntry _) => (error pos "Variable is not a function"; errorTrExpTy)
               | NONE => (error pos "Function does not exist"; errorTrExpTy)
               | SOME(E.FunEntry {formals, result=resTy}) =>
                  let
                    val formalsN = length formals
                    val actualsN = length args
                  in
                    if formalsN <> actualsN then
                      (error pos "Function has the wrong arity"; errorTrExpTy)
                    else
                      let
                        val z = ListPair.zip (args, formals)
                        fun checkType(argExp, fTy) = reqSameType(pos, tenv, trexp argExp, {exp=(), ty=fTy})
                      in
                        (* check the type of each argument expression matches the corresponding formal parameter type *)
                        app checkType z;
                        {exp=todoTrExp, ty=resTy}
                      end
                  end
          end

        | trexp(A.IfExp {test=testExp, then'=thenExp, else'=SOME elseExp, pos}) =
          let
            val testA = trexp testExp
            val thenA = trexp thenExp
            val elseA = trexp elseExp
            val {exp=_, ty=resTy} = thenA
          in
            checkInt(testA, pos);
            reqSameType(pos, tenv, thenA, elseA);
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
          (* FIXME: Oops, I've required the fields in the record expression to be specified in the same order as in the 
                    record declaration! *)
          let
            fun checkField((symbol, exp, pos), (tySymbol, ty)) =
              let
                val expA = trexp exp
              in
                if symbol = tySymbol then
                  reqSameType(pos, tenv, {exp=(), ty=ty}, expA)
                else
                  error pos "field is not in record type"
              end

            fun checkFields(tyFields) = 
              if length fields = length tyFields then
                app checkField (ListPair.zip(fields, tyFields))
              else
                error pos "Record expression has the wrong arity"

            val t = lookupActualType(pos, tenv, typ)
          in
            case t
              of T.RECORD (tyFields, unique) => (checkFields (tyFields); {exp=todoTrExp, ty=t})
               | _                           => (error pos "Not a record type"; errorTrExpTy)
          end

        | trexp(A.IntExp _) = {exp=todoTrExp, ty=T.INT}
        | trexp(A.StringExp _) = {exp=todoTrExp, ty=T.STRING}
        | trexp(A.BreakExp pos) = {exp=todoTrExp, ty=T.UNIT}

        | trexp(A.OpExp{left, oper, right, pos}) =
          let val leftA = trexp left
              val rightA = trexp right
              val {exp=_, ty=leftTyBare} = leftA
              val {exp=_, ty=rightTyBare} = rightA
              val leftTy = actual_ty (pos, tenv, leftTyBare) (* XXX: Not required? *)
              val rightTy = actual_ty (pos, tenv, rightTyBare) (* XXX: Not required? *)
          in
            (* The following are int-only operations: + - * / < > <= >= *)
            case oper 
              of (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp) =>
                let in
                  checkInt(leftA, pos);
                  checkInt(rightA, pos);
                  {exp=todoTrExp, ty=T.INT}
                end
              | (A.LtOp | A.LeOp | A.GtOp | A.GeOp) => 
                let in
                  checkInt(leftA, pos);
                  checkInt(rightA, pos);
                  {exp=todoTrExp, ty=wouldBeBooleanTy}
                end
              | (A.EqOp | A.NeqOp) => let in
                (* Operators = and <> operate on int, string, record and arrays. *)
                (* TODO: Refactor checking of unique type for records/arrays. *)
                case (leftTy, rightTy)
                  of (T.INT, T.INT) => {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | (T.STRING, T.STRING) => {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | (T.RECORD (_, lUnique), T.RECORD (_, rUnique)) =>
                     if lUnique <> rUnique then
                       (error pos "Record types are not equal"; errorTrExpTy)
                     else {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | (T.NIL, T.RECORD _) => {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | (T.RECORD _, T.NIL) => {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | (T.ARRAY (lTy, lUnique), T.ARRAY (rTy, rUnique)) =>
                     if lUnique <> rUnique then
                       (error pos "Array types are not equal"; errorTrExpTy)
                     else {exp=todoTrExp, ty=wouldBeBooleanTy}
                   | _ => (error pos "Types mismatch"; errorTrExpTy)
                end
          end

        | trexp(A.LetExp {decs, body, pos}) =
            let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
            (* TODO: The book has transExp(venv', tenv') body. Wonder if this makes it easier to map/app. *)
            in transExp(venv', tenv', body)
            end

        | trexp(A.SeqExp expList) = 
            let 
              val rs = map trexp (map #1 expList)
              val {exp=_, ty=lastTy} = List.last rs
            in {exp=todoTrExp, ty=lastTy}
            end
            
      and trvar(A.SimpleVar(id, pos)) =
        (case S.look(venv, id)
          of SOME(E.VarEntry {ty}) => {exp=todoTrExp, ty=actual_ty(pos, tenv, ty)}
           | SOME(E.FunEntry _)    => (error pos ("variable points to a function - compiler bug?: " ^ S.name id);
                                       {exp=errorTrExp, ty=T.INT})
           | NONE                  => (error pos ("undefined variable " ^ S.name id);
                                       {exp=errorTrExp, ty=T.INT})
        )
        | trvar(A.FieldVar(var, sym, pos)) = todoExpTy
        | trvar(A.SubscriptVar(var, exp, pos)) = todoExpTy
    in
      trexp(exp)
    end

  and transTy (            tenv: E.tenv, ty: A.ty): T.ty = todoTy (* XXX: what's this? *)

  and transProg(exp: A.exp):unit =
    (transExp(E.base_venv, E.base_tenv, exp);
     ())

end
