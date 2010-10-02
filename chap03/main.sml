structure Main =
struct
  open Parse

  fun compile(file: string) = let in
    print (file ^ ":\n"); 
    parse file
  end;

  fun main(cmd : string, args : string list) : OS.Process.status = let in
    app compile args; 0
  end
end
