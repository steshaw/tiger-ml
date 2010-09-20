signature TREE = 
sig 
  type label = Temp.label
  type size

datatype stm = SEQ of stm * stm
             | LABEL of label
             | JUMP of exp * label list
             | CJUMP of relop * exp * exp * label * label
	     | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | MEM of exp
             | TEMP of Temp.temp
             | ESEQ of stm * exp
             | NAME of label
             | CONST of int
	     | CALL of exp * exp list

      and binop = PLUS | MINUS | MUL | DIV 
                | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

      and relop = EQ | NE | LT | GT | LE | GE 
	        | ULT | ULE | UGT | UGE

  val notRel : relop -> relop
  val commute: relop -> relop
end

structure Tree : TREE = 
struct
  type label=Temp.label
  type size = int

datatype stm = SEQ of stm * stm
             | LABEL of label
             | JUMP of exp * label list
             | CJUMP of relop * exp * exp * label * label
	     | MOVE of exp * exp
             | EXP of exp

     and exp = BINOP of binop * exp * exp
             | MEM of exp
             | TEMP of Temp.temp
             | ESEQ of stm * exp
             | NAME of label
             | CONST of int
	     | CALL of exp * exp list

      and binop = PLUS | MINUS | MUL | DIV 
                | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

      and relop = EQ | NE | LT | GT | LE | GE 
	        | ULT | ULE | UGT | UGE

end

