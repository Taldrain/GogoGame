(**
Logique fonctionnelle du jeu (aKa pas l'intelligence, uniquement les reactions
**)
open Protocol

exception Illegal_move

let list_commands () = "protocol_version\nname\nversion\nquit\nknown_command
list_commands\nboardsize\nclear_board\nkomi\nfixed_handicap\nplace_free_handicap
set_free_handicap\nplay'genmove\nundo"

let boardsize = ref 0
let is_clear = ref true

let set_boardsize i =
  if i < 7 || i > 25 then Failure "unacceptable size"
  else (boardsize := i; Success)

let clean_board () = () (* TODO *)
let set_komi f = () (* TODO *)
let choose_fixed_handicap i = (* TODO *)
  if not !is_clear then Failure "board not empty"
  else if i <= 0 then Failure "invalid number of stones"
  else if ((!boardsize mod 2 = 0) || !boardsize = 7) && i > 4
  then Failure "invalid number of stones"
  else if (!boardsize mod 2 = 1) && i > 9 then Failure "invalid number of stones"
  else
    Success

let place_free_handicap i = Success (* TODO *)
let set_free_handicap l = Success (* TODO *)
let play m = () (* TODO *)
let genmove c = SuccessSTR "" (* TODO *)
let undo () = () (* TODO *)

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
