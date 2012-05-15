(**
Negascout, une ammélioration de alpha - beta
**)
open BatPervasives
open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

open AlgoUtils
open Group_again

(* setup *)
let depth = 2

let size = 13
let ss = 169

(* fin setup *)
let i_want_to_pass = false

let is_end s = (BatBitSet.count s.blk) + (BatBitSet.count s.wht) > 165


let eval_cell i =
  let m = i mod 13 in
  match (i,m) with
  | (0,_) | (12,_) | (156,_) | (169,_) -> -30 (* coins *)
  | (i,0) | (i,_) when i < 12 || i > 156  -> -10 (* bord *)
  | (i,1) | (i,_) when i < 29 || i > 141 -> 5 (* ligne du territoire *)
  | (i,2) | (i,_) when i < 40 || i > 133 -> 10 (* ligne de la victoire *)
  | _ -> 0

let find_shapes set =
  let rec find i offset =
    assert (offset >= 0);
    if not (bitSet_is_set set offset) then false
    else
      let shp = Shape.shapes.(i) and res = ref true in
      let len = Array.length (shp offset) and j = ref 0 in
      while !j < len && !res do
        let jj = (shp !j).(offset) in
        (assert (jj > 0);
        if jj < 168 then
          res := !res && (bitSet_is_set set jj))
      done; !res
  in
  let max = Array.length Shape.shapes
  and count = ref 0 in
  for i = 0 to max do
  	for j = 0 to 160 do
    	if (i mod 13 < 11) && find i j then incr count
    done
  done;
  (50 * !count)

let rate_groups set =
  let tbl = Hashtbl.create 101
  and nb_groups = ref 0
  and lib = ref 0
  in
  for i = 0 to 168 do
  	let g = Group_again.Groups.group_of_stone i in
    if Hashtbl.mem tbl g then ()
    else
      let { stones = _; lib = l } = g in
      lib := !lib + l;
      incr nb_groups
  done;
  (!nb_groups * (!lib * !lib))



let eval (blacks,whites,c,id) =
  let bonus = eval_cell id
  and shape_bonus = (if c = Black then find_shapes blacks else find_shapes whites)
  and groups_bonus = (if c = Black then rate_groups blacks else rate_groups whites)
  in
  bonus + shape_bonus + groups_bonus

let generate_next color s = AlgoUtils.generate_next color s


let take_best l =
  let rec best ((b_score,b_state):(int * AlgoUtils.state)) =
    function
    | [] -> (b_score,b_state)
    | (score,state) :: l ->
        if score > b_score
        then best (score,state) l
        else best (b_score,b_state) l
  in best (List.hd l) (List.tl l)


exception Result of AlgoUtils.state
let rec negascout : int -> int -> int -> AlgoUtils.state -> Entities.Color.t -> (int * AlgoUtils.state) =
 fun depth alpha beta state color ->
  let first_child = ref true in
  let rec main depth b beta alpha state =
    let (score,state) =
      let (s,state) = (negascout (depth - 1) (- b) (- alpha) state (invert_color color))
      in
      let s = -s in
      if (not !first_child) && ((alpha < s) && (s > beta))
      then
        let (s,state) = (negascout (depth - 1) (- beta) (- alpha) state (invert_color color)) in
        (-s,state)
      else (s,state)
    in
    (first_child := false;
      let alpha = max alpha score in
      if alpha >= beta then (alpha,state)
      else main depth (alpha + 1) beta alpha state)
  in
  if is_end state
  then (max_int,state)
  else
  if depth = 0
  then
    let { color = c; vert = v } = state.mov in
    let id = int_of_v v in
    (eval (state.blk, state.wht, c, id), state)
  else
   (first_child := true;
   let b = beta
    
    and children = generate_next color state ()
    in
    List.map (main depth b beta alpha) children |> take_best
  )

let find_best color states =
  (List.map (fun s -> negascout depth min_int max_int s color) states)
  |> take_best
