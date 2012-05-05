(**
Ce module sert á afficher le plateau de jeu en ASCII
**)

open BatPervasives
open Board
open Entities.Color

let last = ref 0
let collumn = ref 0
let rec get_next board =
  let s = board#size in
  if !last = 0 then (last := s*s-1;collumn := !last) else ();
  match board#get !last with
    | Middle (i,_,(_,_,_,r)) -> last := i; board#get r
    | Corner (i,_,(_,r)) | Border (i,_,(_,_,r))
      -> if i mod s = 0 then (last := !collumn; get_next board)
         else (last := i; board#get r)


let fill buffer board =
  let width = board#size and max = (board#size * board#size) -1 in
  let rec rec_fill i =
    BatEnum.push !buffer '|';
    ((match (get_next board) |> Board.color_of_node with
      | Empty -> BatEnum.push !buffer ' '
      | Black -> BatEnum.push !buffer '0'
      | White -> BatEnum.push !buffer '1');
    BatEnum.push !buffer '|';
    if (i mod width = 0) then BatEnum.push !buffer '\n' else ());
    if (i < max) then rec_fill (i+1) else !buffer
  in
  (BatEnum.push !buffer '\n';rec_fill 0)

let display board =
  let buff = ref (BatEnum.empty ()) in
  let e = fill buff board in 
    BatString.of_enum e