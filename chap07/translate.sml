functor Translate(Frame: FRAME):
sig
  type level
  type access

  val outermostLevel: level
  val globalAccess: access

  val newLevel: { parent: level, name: Temp.label, formals: bool list } -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
end =
struct
  datatype level
    = Level of {
        parent: level,
        name: Temp.label,
        formals: bool list,
        frame: Frame.frame
      }
    | Outermost

  datatype access
    = Global
    | Local of level * Frame.access

  val outermostLevel = Outermost

  val globalAccess = Global

  fun newLevel({parent=parent, name=name, formals=formals}) =
    Level {parent=parent, name=name, formals=formals, frame=Frame.newFrame({name=Temp.newLabel(), formals=formals})}

  fun formals level = case level
    of Outermost => raise Fail "no formals for outermost"
    | Level({parent=_,name=_,formals=formals,frame=_}) => [] (* TODO *)

  fun allocLocal level escapes = case level
    of Outermost => raise Fail "cannot allocate locals at outermost level"
    | Level {parent=parent, name=name, formals=formals, frame=frame} => Local (level, Frame.allocLocal frame escapes)
end
