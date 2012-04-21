(************************************************************************
*
* Gogo Game
*
* Yet another AI for the Go Game
*
*
*************************************************************************)
open Batteries_uni

let end_game = ref false
exception Quit_signal

let clean_exit () =
  end_game := true


let gtp_main_loop () =
  let input = input_line stdin in
  match Parser.parse_line input with
  | Protocol.Success -> ()
  | Protocol.SuccessID _ -> ()
  | Protocol.SuccessFull _ -> ()
  | Protocol.SuccessSTR _ -> ()
  | Protocol.Failure _ -> ()
  | Protocol.FailureFull _ -> ()
  | Protocol.Command cmd | Protocol.CommandFull (_, cmd) ->
      let output = Engine.action cmd in
      let output = Parser.Formatter.format output in
      print_string output; flush stdout

let _ =
  while not !end_game
  do
    try gtp_main_loop () with Quit_signal -> clean_exit ()
  done
