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

(* Input: x et y, deux int en coordonnees
 * Output un move de couleur Noir
 *)
let make_mv x y =
  move_of_string("Black "^string_of_vertex(
    vertex_of_int b#get#size (x*b#get#size+y)))

(* Input: taille de la board, nbr d'handicap
 * Output: list des moves d'handicaps
 *)
let make_list_handicap b n =
  let dist = if b < 12 then 3 else 4 in
  let lst = ref[] in
    for i = 1 to n do
      match i with
        | 1 -> lst := (make_mv dist dist) :: !lst
        | 2 -> lst := (make_mv (b-dist) (b-dist)) :: !lst
        | 3 -> lst := (make_mv dist (b-dist)) :: !lst
        | 4 -> lst := (make_mv (b-dist) dist) :: !lst
        | 5 -> lst := (make_mv (b/2) (b/2)) :: !lst
        | 6 -> lst := List.tl !lst;
               lst := (make_mv dist (b/2)) :: !lst; 
               lst := (make_mv (b-dist) (b/2)) :: !lst
        | 7 -> lst := (make_mv (b/2) (b/2)) :: !lst
        | 8 -> lst := List.tl !lst;
               lst := (make_mv (b/2) dist) :: !lst;
               lst := (make_mv (b/2) (b-dist)) :: !lst
        | _ -> lst := (make_mv (b/2) (b/2)) :: !lst
    done;
    !lst

let make_a_play m =
  let b = b#get in
  b#place_stone m;
  History.play m;
  AI.refresh_groups m
    
(* Input: list de move
 * Output: Place les pions sans mettre a jour l'historique -> gtp
 *)
let rec make_play_handicap = function
    [] -> ()
  | e::l -> b#get#place_stone e; AI.refresh_groups e; make_play_handicap l

(* Input: nbr d'handicap
 * Function GTP, check le nombre de pions d'handicaps a placer
 * place ces pions
 *)
let choose_fixed_handicap i =
  if not b#get#is_clear then Failure "board not empty"
  else if i <= 1 then Failure "invalid number of stones"
  else if (b#get#size mod 2 = 0) || (b#get#size = 7) && i > 4
  then Failure "invalid number of stones"
  else if (b#get#size mod 2 = 1) && i > 9 then Failure "invalid number of stones"
  else if (b#get#size < 7) then Failure "invalid number of stones"
  else
    let lst = make_list_handicap b#get#size i in
    make_play_handicap lst;
    let rec foo = function
        [] -> []
      | e::l -> (string_of_vertex e.vert) :: foo l
    in SuccessLST (foo lst)

(* Input: liste de vertices
 * Function GTP, place la liste de vertices passe in input
 * Amelioration: checker le nbr d'element de la liste, normalement correct
 *)
let rec set_free_handicap l = match l with 
    [] -> ()
  | e::l -> let m = move_of_string("Black "^(string_of_vertex e)) in
      b#get#place_stone m;
      AI.refresh_groups m;
      set_free_handicap l

let play m = make_a_play m
let genmove c =
  Globals.color#set c;
  let move = AI.genmove c in
  make_a_play move;
  let v = Entities.Vertex.string_of_vertex move.vert in
    SuccessSTR v

(* Appele par place_free_handicap, place les pions d'handicaps manuellement *)
let rec move_free_handicap i = match i with
    0 -> []
  | n -> Globals.color#set Black;
         let move = AI.genmove Black in
         make_a_play move;
         (string_of_vertex move.vert) :: move_free_handicap (n-1)

(* Input: nbr de pions a placer manuellement
 * Function GTP, check le nbr de pions a placer et appel la fonction pour les
 * placer
 *)
let place_free_handicap i = (*TODO:Placer stone manuellement*)
  if not b#get#is_clear then Failure "board not empty"
  else if i < 2 then Failure "invalid number of stones"
  else if i >= (b#get#size * b#get#size) then Failure "invalide numver of stones"
  else SuccessLST (move_free_handicap i)

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
  | Set_free_handicap l -> set_free_handicap l; Success
  | Play m -> (try (play m; Success) with Illegal_move -> Failure "illegal_move")
  | GenMove c -> genmove c
  | Undo -> undo (); Success
