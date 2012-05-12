(**
UCT est un algo de recherche basé sur Monte - Carlo, fait pour le GO

On verra bien ce que ca va donner...
**)

open BatRandom
open Entities
open Entities.Color

let seed = BatRandom.self_init ()
let uctk = 0.44 (* sqrt (1/5) *)

class node color id blk wht =
object
  val mutable wins = 0
  val mutable visits = 0
  val blacks = blk
  val whites = wht
  val color = color

  val mutable child = None
  method color = color
  method visits = visits
  method wins = wins
  method blacks = blk
  method whites = wht

  method sibling = match child with
    | None -> None
    | Some child ->
      let c = BatSet.choose child in
      (new node (invert_color color) c.blk c.wht )

  method expand =
    let col = if color = Black then White else Black in
    let set = AlgoUtils.generate_next {
      blk = blacks;
      wht = whites;
      grp =[];
      shp =[];
      mov = { col = col; vert =(vertex_of_int id)};
      }
    in
    child <- Some set

  method update result =
    visits <- visits + 1;
    if result = Win then wins <- wins + value else ()

  method getWinRate =
    if visits > 0
    then (float_of_int wins) /. (float_of_int visits)
    else 0. (* should not happend *)
end

let get_best_child root =
  let rec find best = function
    | None -> best
    | Some child -> if child#visits > best#visits then find child child#sibling
        else find best child#sibling
  in
  find root root.child

let uctSelect node =
  let rec select next best_uct res =
    match next with
    | None -> res
    | Some next ->
        let uctvalue =
          if next#visits > 0 then
            let uct = uctk *. (sqrt (log ((float_of_int node#visits) /. (float_of_int next#visits))))
            in
            next#getWinRate + uct
          else
            10000 + 1000 * (BatRandom.int 1000)
        in
        if uctvalue > best_uct then select next#sibling uctvalue next
        else select next#sibling best_uct res
  in
  select node#child 0 0

let playRandomGame node =
  let game_over n =
    (BatBitSet.count n#blacks) + (BatBitSet.count n#whites) >= 167
  in
  let rec play node =
    if not (game_over node) then play (random_node ()) else node
  in
  let (blk,wht) = Scoring.score node#blacks node#whites in
  let comp = if node#color = White then (<) else (>) in
  if comp blk wht then Win else Lose

let playSimulation (n: node) =
  let randomResult =
    if n#child = None && n#visits < 10 then
      (* 10 simulations until chilren are expanded (saves memory) *)
      playRandomGame node
    else
      (if n#child = None then n#expand else ();
        let next = uctSelect(n) in
        if next = None then raise failwith "next est a none, WTF ?!"
        else
          inv (playSimulation next))
  in
  n#update randomResult

let uctSearch numSim color blacks whites =
  let root = new node blacks whites in
  for i = 0 to numSim do
    playSimulation root
  done;
  get_best_child root
