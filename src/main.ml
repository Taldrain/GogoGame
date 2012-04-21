(************************************************************************
 * 
 *                             Gogo Game
 * 
 *                 Yet   another   AI   for   the   Go   Game 
 * 
 * 
 *************************************************************************)
open Batteries_uni

let end_game = ref false

let format (str:string) = str

let gtp_main_loop () =
  let input = input_line stdin in
  let action = Parser.parse_line input in
  let output = AI.do_action action in
  let output = format output in
  print_string output; flush stdout


let _ =
  while not !end_game
  do
    gtp_main_loop ()
  done
