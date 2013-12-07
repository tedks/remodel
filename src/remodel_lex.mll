{
 open Remodel_parse
 exception Eof

 let strip_quotes s = String.sub s 1 ((String.length s) - 2)
   

}
let alpha = ['a'-'z' 'A'-'Z'] 
let num = ['0'-'9'] 
let whitespace = [' ' '\t']
let syms = ['&' '\'' '/' '\\' '#'  '.' '!' '?' '[' ']' '-' '>' '<' '(' ')' '+' '%' '=' '*' '|' '^' '{' '}' '$']

rule file = parse
  | whitespace+ { file lexbuf }
  | eof { EOF }
  | "<-" { RULEDIR }
  | ":" { RULESEP }
  | "," { COMMA }
  | "\n"+ { NEWLINE }
  | "#"(alpha|num|whitespace|syms)* { COMMENT }
  | "DEFAULT" { DEFAULT }
  | "\""(alpha|syms|num|whitespace)*"\"" as lxm { COMMAND(strip_quotes(lxm)) }
  | (alpha|syms|num)+ as lxm { STRING(lxm) }
  | _ as lxm { failwith (Printf.sprintf "Error: No such token <<|%c|>>" lxm) }
