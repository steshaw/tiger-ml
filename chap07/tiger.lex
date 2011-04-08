(* vim: set filetype=sml: *)
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun stringWithoutQuotes(s: string) =
  String.extract (s, 1, SOME(String.size s - 2))

exception NotAnInt

fun getInt(optionInt : int option) = case optionInt of
  SOME(n) => n
  | _ => raise NotAnInt 

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT;
letter = [a-zA-Z];
digit = [0-9];
id = {letter}({letter}|{digit}|_)*;
space = [\ \t];
quote = ["];
notQuote = [^"];
%%
<INITIAL>"var"      => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL>"type"     => (Tokens.TYPE(yypos, yypos + size yytext));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL>"break"    => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL>"of"       => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL>"end"      => (Tokens.END(yypos, yypos + size yytext));
<INITIAL>"in"       => (Tokens.IN(yypos, yypos + size yytext));
<INITIAL>"nil"      => (Tokens.NIL(yypos, yypos + size yytext));
<INITIAL>"let"      => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL>"do"       => (Tokens.DO(yypos, yypos + size yytext));
<INITIAL>"to"       => (Tokens.TO(yypos, yypos + size yytext));
<INITIAL>"for"      => (Tokens.FOR(yypos, yypos + size yytext));
<INITIAL>"while"    => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>"else"     => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>"then"     => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>"if"       => (Tokens.IF(yypos, yypos + size yytext));
<INITIAL>"array"    => (Tokens.ARRAY(yypos, yypos + size yytext));

<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL>"|"  => (Tokens.OR(yypos, yypos + size yytext));
<INITIAL>"&"  => (Tokens.AND(yypos, yypos + size yytext));
<INITIAL>">=" => (Tokens.GE(yypos, yypos + size yytext));
<INITIAL>">"  => (Tokens.GT(yypos, yypos + size yytext));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos + size yytext));
<INITIAL>"<"  => (Tokens.LT(yypos, yypos + size yytext));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL>"="  => (Tokens.EQ(yypos, yypos + size yytext));
<INITIAL>"/"  => (Tokens.DIVIDE(yypos, yypos + size yytext));
<INITIAL>"*"  => (Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL>"-"  => (Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL>"+"  => (Tokens.PLUS(yypos, yypos + size yytext));

<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos + size yytext));

<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos + size yytext));

<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos + size yytext));

<INITIAL>"." => (Tokens.DOT(yypos, yypos + size yytext));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos + size yytext));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL>":" => (Tokens.COLON(yypos, yypos + size yytext));

<INITIAL>{id}                       => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>{digit}+                   => (Tokens.INT(getInt (Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>{quote}{notQuote}*{quote}  => (Tokens.STRING(stringWithoutQuotes(yytext), yypos, yypos + size yytext));

<INITIAL>"/*"   => (YYBEGIN COMMENT; continue());
<COMMENT>"*/"   => (YYBEGIN INITIAL; continue());
<COMMENT>.      => (continue());

{space}+        => (continue());
"\n"            => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
.               => (ErrorMsg.error yypos ("illegal character '" ^ yytext ^ "'"); continue());
