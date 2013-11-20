%{
  open Remodel_ast
  let parse_error s = print_endline s
%}
%token NEWLINE
%token RULEDIR RULESEP
%token COMMA COMMENT 
%token DEFAULT
%token <string> STRING
%token <string> COMMAND
%token EOF

%type <Remodel_ast.program> prog
%start prog
%% 

prog:
| EOF { [] }
| COMMENT NEWLINE prog { $3 }
| production NEWLINE prog { $1 :: $3 }

production:
| DEFAULT RULEDIR filelist { Default($3) }
| filelist RULEDIR filelist { Bare_target($1, $3) }
| filelist RULEDIR filelist RULESEP COMMAND
    { Command_target($1, $3, $5) }

filelist:
| STRING { [$1] }
| STRING COMMA filelist { $1 :: $3 } 

