structure Main =
struct 
  open Parse
  fun main(cmd : string, args : string list) : OS.Process.status = (
    parse "../testcases/test1.tig";
    0
  )
end
