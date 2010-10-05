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
      Semant.transProg(ast) (* type check *)
    end
  end

  fun main(cmd : string, args : string list) : OS.Process.status = let in
    app compile args; 0
  end
end
