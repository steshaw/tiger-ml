(* vim: set filetype=sml: *)
signature FRAME =
sig
  type frame
  type access
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val allocLocal: frame -> bool -> access

  val FP: Temp.temp
  val wordSize: int
  val exp: access -> Tree.exp -> Tree.exp
end
