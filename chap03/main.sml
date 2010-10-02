structure Main =
struct
  open Parse
  fun main(cmd : string, args : string list) : OS.Process.status =
    let
      (* XXX: Ugly! *)
      fun loop n =
        if (n < 50) then (
          let 
            val file = ("../testcases/test" ^ Int.toString(n) ^ ".tig") 
          in 
            print (file ^ ":\n"); 
            parse file
          end;
        loop (n + 1)
        )
        else ()
    in 
      loop 1;
      0
    end
end
