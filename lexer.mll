{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let beta  = lower | upper | digit

rule token = parse
| space+  { token lexbuf } (* skip spaces *)
| "("	  { LPAREN }
| ")"	  { RPAREN }
| "<"	  { LRESET }
| ">"     { RRESET }
| "true"  { TRUE }
| "false" { FALSE }
| "lam"   { LAMBDA }
| "."     { DOT }
| "@"     { APP }
| "S"     { SHIFT }
| digit+  { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| lower beta* { VARIABLE (Lexing.lexeme lexbuf) }
| eof	  { EOF }
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
