functor Translate(Frame: FRAME):
sig
  type level
  type access

  val outermostLevel: level
  val globalAccess: access

  val newLevel: { parent: level, name: Temp.label, formals: bool list } -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  type exp

  val unEx: exp -> Tree.exp
  val unNx: exp -> Tree.stm
  val unCx: exp -> (Temp.label * Temp.label -> Tree.stm)

  val simpleVar: access -> level -> exp
end =
struct

  structure T = Tree

  datatype level
    = Level of {
        parent: level,
        name: Temp.label,
        formals: bool list,
        frame: Frame.frame,
        id: unit ref
      }
    | Outermost

  datatype access
    = Global
    | Local of level * Frame.access

  val outermostLevel = Outermost

  val globalAccess = Global

  fun newLevel({parent=parent, name=name, formals=formals}) =
    Level {parent=parent, name=name, 
           formals=formals, 
           frame=Frame.newFrame({name=Temp.newLabel(), formals=formals}),
           id = ref ()}

  fun formals level = case level
    of Outermost => raise Fail "no formals for outermost"
    | Level({parent=_,name=_,formals=formals,frame=_, id=_}) => [] (* TODO *)

  fun allocLocal level escapes = case level
    of Outermost => raise Fail "cannot allocate locals at outermost level"
    | Level {parent=_, name=_, formals=_, frame=frame, id=_} => 
        Local (level, Frame.allocLocal frame escapes)

  datatype exp
    = Ex of T.exp
    | Nx of T.stm
    | Cx of Temp.label * Temp.label -> T.stm

  fun unEx (Ex e) = e
    | unEx (Cx mkStm) =
        let 
          val r = Temp.newTemp ()
          val t = Temp.newLabel ()
          val f = Temp.newLabel ()
        in
          T.ESEQ(T.seq[T.MOVE(T.TEMP r, T.CONST 1),
                       mkStm (t, f),
                       T.LABEL f,
                       T.MOVE (T.TEMP r, T.CONST 0),
                       T.LABEL t],
                 T.TEMP r)
        end
    | unEx (Nx s) = T.ESEQ (s, T.CONST 0)

  fun unNx exp = raise Fail "implement Translate.unNx"

  fun unCx (Cx mkStm) = mkStm
  fun unCx (Ex e) = case e
    of (T.CONST 0) => fn (t, f) => T.EXP e
(*
     | (T.CONST 1) => fn (t, f) => T.EXP e
*)

  (* TODO: Handle following static links when levels are different. *)
  fun simpleVar (Local (localLevel, localAccess)) level = Ex (Frame.exp localAccess (T.TEMP Frame.FP))

end
