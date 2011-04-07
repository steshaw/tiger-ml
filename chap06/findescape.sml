(* TODO: implement me *)
structure FindEscape: sig
  val findEscape: Absyn.exp -> unit
end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(env: escEnv, d: depth, s: Absyn.var): unit = raise Fail "todo"

  and traverseExp(env: escEnv, d: depth, s: Absyn.exp): unit = raise Fail "todo"

  and traverseDecs(env, d, s: Absyn.dec list): escEnv = raise Fail "todo"

  fun findEscape(prog: Absyn.exp): unit = ()
(*
    print "findEscapes pass: nop"
*)

end
