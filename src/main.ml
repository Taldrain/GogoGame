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
  match Parser.parse_line input with
  | Parser.Success | Parser.SuccessID _ | Parser.SuccessFull _ -> ()
  | Parser.SuccessSTR  _ | Parser.Failure _ | Parser.FailureFull _ -> ()
  | Parser.Command cmd | Parser.CommandFull (_,cmd) ->
  let output = AI.do_action cmd in
  let output = format output in
  print_string output; flush stdout


let _ =
  while not !end_game
  do
    gtp_main_loop ()
  done
