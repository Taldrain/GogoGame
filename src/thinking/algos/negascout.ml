(**
Negascout, une ammélioration de alpha - beta
**)
open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

open AlgoUtils

(* setup *)
let depth = 3

let size = 13
let ss = 169

(* fin setup *)
let i_want_to_pass = false

let is_end s = Globals.last_is_pass#get && i_want_to_pass

l
let eval_cell i =
  let m = i mod 13 in
  match (i,m) with
  | (0,_) | (12,_) | (156,_) | (169,_) -> -30 (* coins *)
  | (i,_) when i < 12 | (i,_) when i > 156 | (_,0) -> -10 (* bord *)
  | (i,_) when i < 29 | (i,_) when i > 141 | (_,1) -> 5 (* ligne du territoire *)
  | (i,_) when i < 40 | (i,_) when i > 133 | (_,2) -> 10 (* ligne de la victoire *)
  | _ -> 0

let eval (last_score,blacks,whites,groups,shapes,c,id) =
  let score = ref 0 in
 

let generate_next s = AlgoUtils.generate_next s


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
