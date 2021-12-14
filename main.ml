open Syntax
open Eval
open TySyntax
open ConstraintSolver
       
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;

  try
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (tyscs, newtyenv) = infer_command tyenv cmd in
    let (ids, newenv, vs) = eval_command env cmd in 
    printvals ids tyscs vs; read_eval_print newenv newtyenv
  with
  | Parsing.Parse_error -> 
    Printf.printf "Error: cannot parse"; print_newline (); read_eval_print env tyenv
  | Failure _ -> 
    Printf.printf "Error: Syntax error"; print_newline (); read_eval_print env tyenv
  | TyError ->
    Printf.printf "Error: type error"; print_newline (); read_eval_print env tyenv
  | EvalErr -> 
    Printf.printf "Error: cannot evaluate"; print_newline (); read_eval_print env tyenv
  | Unbound x -> 
    Printf.printf "Error: Unbound value %s" x; print_newline (); read_eval_print env tyenv
  | Div_by_Zero -> 
    Printf.printf "Exception: Division_by_zero."; print_newline (); read_eval_print env tyenv

let initial_env = empty_env
    
let _ = read_eval_print initial_env initial_env
