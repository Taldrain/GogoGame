(************************************************************************
*
* Gogo Game
*
* Yet another AI for the Go Game
*
*
*************************************************************************)
open Batteries_uni
open Common

let clean_exit () =
  print_string ("="^(Parser.Formatter._id ())^"\n\n");
  Pervasives.exit 0

let failure str =
  (let output = (Parser.Formatter.format (Protocol.Failure str))
    in print_string output; flush stdout)

let gtp_main_loop () =
  let input = input_line stdin in
  try
    (match Parser.parse_line input with
      | Protocol.Success -> ()
      | Protocol.SuccessID _ -> ()
      | Protocol.SuccessFull _ -> ()
      | Protocol.SuccessSTR _ -> ()
      | Protocol.SuccessLST _ -> ()
      | Protocol.SuccessLSTID _ -> ()
      | Protocol.Failure _ -> ()
      | Protocol.FailureFull _ -> ()
      | Protocol.Command cmd | Protocol.CommandFull (_, cmd) ->
          let output = Engine.action cmd in
          let output = Parser.Formatter.format output in
          print_string output; flush stdout)
  with
  | Parser.Protocol_error -> failure "protocol error"
  | Parser.Syntax_error -> failure "syntax error"
  | Parser.Unknown_command -> failure "unknown command"

let _ =
  while true
  do
    try gtp_main_loop () with Quit_signal -> clean_exit ()
  done
