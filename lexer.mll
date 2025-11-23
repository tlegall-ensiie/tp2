{

(* This file is distributed under the MIT license.
   See file LICENSE for more details.
*)

 open Parser

let kw =
  [ "true", TRUE;
    "false", FALSE;
    "skip", SKIP;
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "fi", FI;
    "while", WHILE;
    "do", DO;
    "done", DONE;
  ]

let mk_id s =
  match List.assoc_opt s kw with
  | Some kw -> kw
  | None -> ID s

 }

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper | '_'
let number = digit+
let id = letter (letter | digit)*

rule main = parse
| number { CST (Z.of_string (Lexing.lexeme lexbuf)) }
| id { mk_id (Lexing.lexeme lexbuf) }
| "+" { ADD }
| "*" { STAR }
| "-" { MIN }
| "/" { DIV }
| "=" { EQ }
| "<" { LT }
| ">" { GT }
| "<>" { NE }
| "<=" { LE }
| ">=" { GE }
| "&"  { ADDROF }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
| ":=" { SET }
| "(" { LPAR }
| ")" { RPAR }
| ";" { SEMICOL }
| ' ' | '\t' | '\n' { main lexbuf }
| eof { EOF }
