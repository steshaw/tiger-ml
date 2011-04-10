(*
 * Frame for the x64 architecture i.e. amd64 / x86-64.
 *
 * Will use 32bit pointers/integers though.
 *
 *)
structure X64Frame: FRAME = struct
  val FP = Temp.newTemp ()
  val wordSize = 32 (* in bits *)
  val wordSizeInBytes = wordSize div 8

  datatype access
    = InFrame of int
    | InReg of Temp.temp

  type frame = {
    name: Temp.label,
    formals: access list,
    nextLocalOffset: int ref,
    locals: access list ref
  }

  fun toFormal (offset, escapes) =
    if escapes
    then InFrame offset
    else InReg (Temp.newTemp())

  fun toLocal nextLocalOffset escapes =
    if escapes
    then 
      let
        val offset = !nextLocalOffset
      in
        nextLocalOffset := !nextLocalOffset - wordSizeInBytes;
        InFrame offset
      end
    else InReg (Temp.newTemp())

  fun newFrame {name=name, formals=formals} =
    let
      val positions = List.tabulate (length formals, fn n => (n * 4) + 8)
      val v = ListPair.zip (positions, formals)
    in 
      {name = name, formals=map toFormal v, nextLocalOffset = ref (0-4), locals=ref []}
    end

  fun formals {name=_, formals=formals, nextLocalOffset=_, locals=_} = formals

  fun allocLocal {name=_, formals=_, nextLocalOffset=nextLocalOffset, locals=locals} escapes =
    let
      val l = toLocal nextLocalOffset escapes
    in
      locals := l :: !locals;
      l
    end

  structure T = Tree

  fun exp (InFrame k) exp = T.MEM(T.BINOP(T.PLUS, exp, T.CONST k))
    | exp (InReg temp) _ = T.TEMP temp
end
