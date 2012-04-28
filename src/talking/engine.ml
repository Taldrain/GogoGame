(**
Logique fonctionnelle du jeu (aKa pas l'intelligence, uniquement les reactions
**)
open Protocol
open Entities.Move
open Entities.Vertex
open Entities.Color

exception Illegal_move

let b = Globals.board

let list_commands () = "protocol_version\nname\nversion\nquit\nknown_command
list_commands\nboardsize\nclear_board\nkomi\nfixed_handicap\nplace_free_handicap
set_free_handicap\nplay\ngenmove\nundo"

let set_boardsize i =
  if i < 7 || i > 25 then Failure "unacceptable size"
  else (Globals.board#set (new Board.board i); Success)

let clean_board () = Globals.event_clear#get#raise
let set_komi f = Globals.komi := f
let choose_fixed_handicap i = (* TODO *)
  if not b#get#is_clear then Failure "board not empty"
  else if i <= 0 then Failure "invalid number of stones"
  else if (b#get#size mod 2 = 0) || (b#get#size = 7) && i > 4
  then Failure "invalid number of stones"
  else if (b#get#size mod 2 = 1) && i > 9 then Failure "invalid number of stones"
  else
    Success

let make_a_play m =
  let b = b#get in
  b#place_stone m;
  History.play m;
  AI.refresh_groups m

let place_free_handicap i = Success (* TODO *)
let set_free_handicap l = Success (* TODO *)
let play m = make_a_play m
let genmove c =
  Globals.color#set c;
  let move = AI.genmove c in
  make_a_play move;
  let v = Entities.Vertex.string_of_vertex move.vert in
    SuccessSTR v

let undo () = (b#get#unset_stone (History.undo ()))

let action = function
  | Protocol_version -> SuccessSTR "2"
  | Name -> SuccessSTR "Gogo Game"
  | Version -> SuccessSTR "3.14"
  | Quit -> Success
  | Known_command s -> SuccessSTR "true"
  | List_commands -> SuccessSTR(list_commands ())
  | Boardsize i -> set_boardsize i
  | Clear_board -> clean_board (); Success
  | Komi f -> set_komi f; Success
  | Fixed_handicap i -> choose_fixed_handicap i
  | Place_free_handicap i -> place_free_handicap i
  | Set_free_handicap l -> set_free_handicap l
  | Play m -> (try (play m; Success) with Illegal_move -> Failure "illegal_move")
  | GenMove c -> genmove c
  | Undo -> undo (); Success
