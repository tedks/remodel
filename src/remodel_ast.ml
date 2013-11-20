
type dependency = string list
type target = string list
type command = string

type production = 
| Default of target
| Bare_target of target * dependency
| Command_target of target * dependency * command

type program = production list

let string_of_production = function
  | Default(t) -> Printf.sprintf "Default <- <<%s>>" (String.concat ", " t)
  | Bare_target(t, d) -> 
    Printf.sprintf "Target<<%s>> <- Deps<<%s>>" 
      (String.concat ", " t) (String.concat ", " d)
  | Command_target (t, d, c) -> 
    Printf.sprintf "Target<<%s>> <- Deps<<%s>> : <<%s>>" 
      (String.concat ", " t) (String.concat ", " d) c
