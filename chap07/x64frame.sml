(*
 * Frame for the x64 architecture i.e. amd64 / x86-64.
 *
 * Will use 32bit pointers/integers though.
 *
 *)
structure X64Frame: FRAME = struct
  datatype access
    = InFrame of int
    | InReg of Temp.temp

  type frame = {
    name: Temp.label,
    formals: access list,
    locals: access list ref
  }

  fun toFormal escapes =
    if escapes
    then InFrame 0 (* FIXME: allocate temporary here *)
    else InReg (Temp.newTemp())

  fun toLocal escapes =
    if escapes
    then InFrame 0 (* FIXME: allocate temporary here *)
    else InReg (Temp.newTemp())

  fun newFrame {name=name, formals=formals} = {name = name, formals=map toFormal formals, locals=ref []}

  fun formals {name=_, formals=formals, locals=_} = formals

  fun allocLocal {name=_, formals=_, locals=locals} escapes =
    let 
      val l = toLocal escapes
    in
      locals := l :: !locals;
      l
    end
end
