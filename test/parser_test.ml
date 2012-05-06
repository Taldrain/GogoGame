open OUnit
  
open Entities
  
open Entities.Move
  
open Entities.Color
  
open Entities.Vertex
  
let test str cmd =
  let str = BatString.enum str
  in
    assert_equal ~printer: Protocol.string_of_command cmd
      (Parser.parse_cmd str)
  
let suite =
  "Parser test suite" >:::
    [ "protocol_version" >::
        (fun () -> test "protocol_version" Protocol.Protocol_version);
      "name" >:: (fun () -> test "name" Protocol.Name);
      "version" >:: (fun () -> test "version" Protocol.Version);
      "true_known_command" >::
        (fun () -> test "known_command name" (Protocol.Known_command "name"));
      "false_known_command" >::
        (fun () -> test "known_command plop" (Protocol.Known_command "plop"));
      "list_commands" >::
        (fun () -> test "list_commands" Protocol.List_commands);
      "quit" >:: (fun () -> test "quit" Protocol.Quit);
      "boardsize" >:: (fun () -> test "boardsize 13" (Protocol.Boardsize 13));
      "clear_board" >:: (fun () -> test "clear_board" Protocol.Clear_board);
      "komi" >:: (fun () -> test "komi 6.5" (Protocol.Komi 6.5));
      "fixed_handicap" >::
        (fun () -> test "fixed_handicap 3" (Protocol.Fixed_handicap 3));
      "place_free_handicap" >::
        (fun () ->
           test "place_free_handicap 3" (Protocol.Place_free_handicap 3));
      "set_free_handicap" >::
        (fun () ->
           test "set_free_handicap A1 B2 C3"
             (Protocol.Set_free_handicap
                [ { letter = 'A'; nb = 1; pass = false; };
                  { letter = 'B'; nb = 2; pass = false; };
                  { letter = 'C'; nb = 3; pass = false; } ]));
      "play_classic" >::
        (fun () ->
           test "play B F5"
             (Protocol.Play
                { color = Black; vert = { letter = 'F'; nb = 5; pass=false }; }));
      "play_long_color_name" >::
        (fun () ->
           test "play Black G10"
             (Protocol.Play
                { color = Black; vert = { letter = 'G'; nb = 10; pass=false }; }));
      "play_pass" >::
        (fun () ->
           test "play W pass"
             (Protocol.Play
                { color = White; vert = { letter = 'A'; nb = 1; pass=true }; }));
      "genmove_B" >:: (fun () -> test "genmove B" (Protocol.GenMove Black));
      "genmove_W" >:: (fun () -> test "genmove W" (Protocol.GenMove White));
      "undo" >:: (fun () -> test "undo" Protocol.Undo);
      "showboard" >:: (fun () -> test "showboard" Protocol.Showboard);
      "negatif_1" >::
        (fun () ->
           assert_raises Parser.Unknown_command
             (fun () -> Parser.parse_cmd (BatString.enum "blop"))) ]
  
