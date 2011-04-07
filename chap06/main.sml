structure Main =
struct
  open Parse
  structure PP = PrintAbsyn

  fun compile(file: string) = let in
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
        handle        _ => ErrorMsg.error 1 "type check failed with unknown exception"
    end
  end

  fun main(cmd: string, args: string list): OS.Process.status =
    let in
      app compile args; 0
    end
end
