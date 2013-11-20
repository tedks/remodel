let _ = 
  let prog = Remodel_parse.prog Remodel_lex.file (Lexing.from_channel stdin) in 
  List.iter (fun prod -> 
    print_endline (Remodel_ast.string_of_production prod)) prog
;;

  
