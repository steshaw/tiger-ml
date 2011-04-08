(* vim: set filetype=sml: *)
signature FRAME =
sig
  type frame
  type access
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val allocLocal: frame -> bool -> access
end
