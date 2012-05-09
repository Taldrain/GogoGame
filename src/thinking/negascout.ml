(**
Negascout, une ammélioration de alpha - beta
**)
open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

type state =
  {
    col : Color.t;
    blk : BatBitSet.t;
    wht : BatBitSet.t;
    grp : BatList.t;
    shp : BatList.t;
    mov : Move.t;
    scr : BatArray.t
  }

let state color black white group shape move score =
  {
    blk = black;
    wht = white;
    grp = group;
    shp = shape;
    mov = move;
    scr = score;
  }
(* TODO *)
let groups_refresh g = g
let shapes_refresh s = s


(* setup *)
let depth = 3

let size = 13

let ss = size * size

(* fin setup *)
let i_want_to_pass = false

let is_end s = Globals.last_is_pass#get && i_want_to_pass

let eval (blacks,whites,groups,shapes,{color=c;vert=v}) = 10

let generate_next s =
  let empties =
    (BatBitSet.union s.blk s.wht) |>
    (BatBitSet.differentiate (BatBitSet.create_full ss))
  in
  let rec next_empty i =
    if not (BatBitSet.is_set empties (i+1)) then next_empty (i+2) else (i+1)
  in
  let rec gen i accu =
    if i = ss then accu
    else
      let (blacks, whites) =
        if s.col = Black
        then (bitset_set_nth s.blk i, s.wht)
        else (s.bk, bitset_set_nth s.wht i)
      and g = groups_refresh s.grp
      and s = shapes_refresh s.shp
      and m = {color=s.col;vert=(vertex_of_int i)}
      in
      let score = eval (blacks,whites,g,s)
      in
      gen (next_empty i) (sort_and_push (state (invert_color s.col) black white g s
        {color=s.col;vert=(vertex_of_int i)} score))
  in
  gen (next_empty 0) (BatEnum.empty ())



let rec negascout depth alpha beta state =
  let first_child = ref true in
  let rec main depth b beta alpha state =
    let score =
      let s = - (negascout (depth - 1) (- b) (- alpha) state)
      in
      if (not !first_child) && ((alpha < s) && (s > beta))
      then - (negascout (depth - 1) (- beta) (- alpha) state)
      else s
    in
    (first_child := false;
      let alpha = max alpha score
      in if alpha >= beta then raise (Result alpha) else b := alpha + 1)
  in
  if is_end state
  then max_int
  else
  if depth = 0
  then eval state
  else
    (first_child := true;
      (let b = beta
        
        and children = generate_next state
        in
        try BatEnum.iter (main depth b beta alpha) children
        with | Result r -> r))

let find_best states =
  let take_best l =
    let rec best (b_state, b_score) =
      function
      | [] -> b
      | (state, score) :: l ->
          if score > b_score
          then best (state, score) l
          else best (b_state, b_score) l
    in best (List.hd l) (List.tl l)
  in
  (BatEnum.map (fun s -> (s, (negascout depth min_int max_int s))) states)
  |> take_best
