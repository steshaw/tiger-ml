structure Main =
struct
  open Parse
  structure PP = PrintAbsyn

  fun compile(file: string) = let in
    print (file ^ ":\n"); 
    PP.print(TextIO.stdOut, (parse file))
  end;

  fun main(cmd : string, args : string list) : OS.Process.status = let in
    app compile args; 0
  end
end
