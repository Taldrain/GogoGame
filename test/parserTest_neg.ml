(**
Test NEGATIFS du parser, doivent tous lancer une exception
**)

open OUnit
open Entities
open Entities.Move
open Entities.Color
open Entities.Vertex

let test str exc =
  let str = BatString.enum str
  in
  assert_raises exc (Parser.parse_cmd str)

let _ =
  "Parser NEGATIVE" >:::
  (List.map
      (fun (arg, name, res) ->
            let title = Printf.sprintf "%s->%s" (if name = "" then arg else name) exc
            in title >:: (fun () -> test arg res))
      [ ("unknown_command", "", Parser.Unknown_command);
      ("name", "name", Protocol.Name);
      ("version", "version", Protocol.Version);
      ("true_known_command", "known_command name",
        (Protocol.Known_command "name"));
      ("false_known_command", "known_command plop",
        (Protocol.Known_command "plop"));
      ("list_commands", "list_commands", Protocol.List_commands);
      ("quit", "quit", Protocol.Quit);
      ("boardsize", "boardsize 13", (Protocol.Boardsize 13));
      ("clear_board", "clear_board", Protocol.Clear_board);
      ("komi", "komi 6.5", (Protocol.Komi 6.5));
      ("fixed_handicap", "fixed_handicap 3", (Protocol.Fixed_handicap 3));
      ("place_free_handicap", "place_free_handicap 3",
        (Protocol.Place_free_handicap 3));
      ("set_free_handicap", "set_free_handicap A1 B2 C3",
        (Protocol.Set_free_handicap
          [ { letter = 'A'; nb = 1; pass = false; };
          { letter = 'B'; nb = 2; pass = false; };
          { letter = 'C'; nb = 3; pass = false; } ]));
      ("play_classic", "play B F5",
        (Protocol.Play
          { color = Black; vert = { letter = 'F'; nb = 5; pass = false; };
          }));
      ("play_long_color_name", "play Black G10",
        (Protocol.Play
          {
            color = Black;
            vert = { letter = 'G'; nb = 10; pass = false; };
          }));
      ("play_pass", "play W pass",
        (Protocol.Play
          { color = White; vert = { letter = 'A'; nb = 1; pass = true; };
          }));
      ("genmove_B", "genmove B", (Protocol.GenMove Black));
      ("genmove_W", "genmove W", (Protocol.GenMove White));
      ("genmove_black", "genmove black", (Protocol.GenMove Black));
      ("genmove_BLACK", "genmove BLACK", (Protocol.GenMove Black));
      ("genmove_BlAcK", "genmove BlAcK", (Protocol.GenMove Black));
      ("genmove_white", "genmove white", (Protocol.GenMove White));
      ("genmove_WHITE", "genmove WHITE", (Protocol.GenMove White));
      ("genmove_WhItE", "genmove WhItE", (Protocol.GenMove White));
      ("undo", "undo", Protocol.Undo);
      ("showboard", "showboard", Protocol.Showboard) ])