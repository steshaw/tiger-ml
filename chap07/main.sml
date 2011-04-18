structure Main =
struct
  open Parse
  structure PP = PrintAbsyn

  structure Fr = X64Frame (* FIXME: why can't I use Frame here? *)

  fun dumpFrag (frag: Fr.frag): unit =
    case frag
      of Fr.PROC {body=body, frame=frame} => let in
           print("  body:\n");
           Printtree.printTreeIndent (TextIO.stdOut, body, 4)
         end
       | Fr.STRING (label, name) => let in
           print("  label ");
           print(Symbol.name label);
           print(": ");
           print(name);
           print("\n")
         end

  fun dumpFrags (fs: X64Frame.frag list) = let in
    print("frags:\n");
    app dumpFrag fs
  end

  fun compile (file: string) = let in
    print (file ^ ":\n");
    let
      val ast = parse file
    in
      print ("parse =>\n");
      PP.print(TextIO.stdOut, ast);
      print ("find escapes =>\n");
      FindEscape.findEscape(ast); (* pass: note variables that escape *)
      print ("type check =>\n");
      Semant.transProg(ast) (* pass: type check *)
        handle Fail msg => ErrorMsg.error 1 ("failed with: " ^ msg)
        handle        _ => ErrorMsg.error 1 "type check failed with unknown exception";
      dumpFrags (TL.getResult ())
    end
  end

  fun main(cmd: string, args: string list): OS.Process.status =
    let in
      app compile args; 0
    end
end
