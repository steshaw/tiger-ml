(* vim: set filetype=sml: *)
signature FRAME =
sig
  type frame
  type access
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val allocLocal: frame -> bool -> access

  val procEntryExit1: {frame: frame, body: Tree.exp} -> Tree.exp

  val FP: Temp.temp
  val wordSize: int
  val exp: access -> Tree.exp -> Tree.exp

  datatype frag
    = PROC of {body: Tree.stm, frame: frame}
    | STRING of Temp.label * string
end
