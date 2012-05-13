(**
UCT est un algo de recherche basé sur Monte - Carlo, fait pour le GO

On verra bien ce que ca va donner...
**)

open BatRandom
open Entities
open Entities.Color
open Entities.Vertex
open Entities.Move

open AlgoUtils

let seed = BatRandom.self_init ()
let uctk = 0.44 (* sqrt (1/5) *)

let reader : (unit -> unit option) Global.global = new Global.global "reader"

class node color id blk wht =
object
  val mutable wins = 0
  val mutable visits = 0
  val mutable is_expanded = false
  val blacks = blk
  val whites = wht
  val color = color
  val id = id

  val mutable child = None
  method color = color
  method visits = visits
  method wins = wins
  method blacks = blk
  method whites = wht
  method is_expanded = is_expanded

  method sibling = match child with
    | None -> None
    | Some child ->
      let c = BatSet.choose child in
      let { color = _ ; vert = v } = c.mov in
      let id = int_of_v v in
      Some (new node (invert_color color) id c.blk c.wht )

  method expand =
    is_expanded <- true;
    let col = if color = Black then White else Black in
    let set = AlgoUtils.generate_next col {
      blk = blacks;
      wht = whites;
      grp =[];
      shp =[];
      mov = { color = col; vert =(vertex_of_id id)};
      }
    in
    child <- Some set

  method update result =
    visits <- visits + 1;
    if result = Win then wins <- wins + 1 else ()

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
  find root root#sibling


let uctSelect node =
  let rec select next best_uct res =
    Best.set_best (res#blacks,res#whites);
    match reader#get () with
      | Some _ -> (Thread.exit ();failwith "plop")
      | None ->
    match next with
    | None -> res
    | Some next ->
        let uctvalue =
          if next#visits > 0 then
            let uct = uctk *. (sqrt (log ((float_of_int node#visits) /. (float_of_int next#visits))))
            in
            (int_of_float next#getWinRate) + (int_of_float uct)
          else
            10000 + 1000 * (BatRandom.int 1000)
        in
        if uctvalue > best_uct then select next#sibling uctvalue next
        else select next#sibling best_uct res
  in
  select node#sibling 0 node

let playRandomGame node =
  let rec random_node node =
    match node#child with
      | None -> node#expand; random_node node
      | Some c -> c
  in
  let game_over n =
    (BatBitSet.count n#blacks) + (BatBitSet.count n#whites) >= 167
  in
  let rec play node =
    if not (game_over node) then play (random_node node) else node
  in
  let (blk,wht) = Scoring.score node#blacks node#whites in
  let comp = if node#color = White then (<) else (>) in
  if comp blk wht then Win else Lose

let rec playSimulation n =
  let randomResult =
    if not n#is_expanded && n#visits < 10 then
      (* 10 simulations until chilren are expanded (saves memory) *)
      playRandomGame n
    else
      (if not n#is_expanded then n#expand else ();
       match n#sibling with
        | None -> failwith "next est a none, WTF ?!" (* ne doit jamais arriver *)
        | Some c -> let next = uctSelect c in invert_gameStatus (playSimulation next))
  in
  n#update randomResult; randomResult

let uctSearch ~nbSim ~color ~last_move ~blacks ~whites ~channel () =
  reader#set channel;
  let root = new node (invert_color color) last_move blacks whites in
  for i = 0 to nbSim do
    playSimulation root
  done;
  get_best_child root
