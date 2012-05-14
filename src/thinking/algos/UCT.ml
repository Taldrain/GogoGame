(**
UCT est un algo de recherche basé sur Monte - Carlo, fait pour le GO

On verra bien ce que ca va donner...
**)

open BatPervasives
open BatRandom
open Entities
open Entities.Color
open Entities.Vertex
open Entities.Move

open AlgoUtils

let seed = BatRandom.self_init ()
let uctk = 0.44 (* sqrt (1/5) *)

let reader : (unit -> unit option) Global.global = new Global.global "reader"

module NodeDB =
struct
  let tbl = BatHashtbl.create 103
  
  let mem = (BatHashtbl.mem tbl)
  let add node = BatHashtbl.add tbl (node#blacks, node#whites) node
  let find (blk, wht) = BatHashtbl.find_option tbl (blk, wht)
end

class node color id blk wht father =
object(self)
  val mutable wins = 0
  val mutable visits = 0
  val mutable is_child_expanded = false
  val mutable is_sibling_expanded = false
  val blacks = blk
  val whites = wht
  val color = color
  val id = id
  val father = father
  
  val mutable children : node BatSet.t option = None
  
  method color = color
  method visits = visits
  method blacks = blk
  method whites = wht
  method is_child_expanded = is_child_expanded
  
  method child : node =
    match children with
    | None -> self#expand_children; self#child
    | Some set -> BatSet.choose set
  
  method get_sibling : node BatSet.t -> node =
    fun seen ->
        match children with
        | None -> failwith "daFuk ?"
        | Some set -> BatSet.choose (BatSet.diff set seen)
  
  method sibling : node BatSet.t -> node =
    fun seen -> father#get_sibling seen
  
  method expand_children : unit =
    let gen_node state =
      let { color = color; vert = v } = state.mov in
      let col = (invert_color color) and id = int_of_v v in
      let n = new node col id state.blk state.wht (self :> node) in
      NodeDB.add n; n
    in
    is_child_expanded <- true;
    let set = AlgoUtils.generate_next color {
          blk = blacks;
          wht = whites;
          mov = { color = color; vert = (vertex_of_id id) };
        } ()
    in
    children <- Some (BatSet.map gen_node set)
  
  method update : gameState -> unit =
    fun result ->
        visits <- visits + 1;
        if result = Win then wins <- wins + 1 else ()
  
  method getWinRate =
    if visits > 0
    then (float_of_int wins) /. (float_of_int visits)
    else 0. (* should not happend *)
end

let get_best_child (root: node) (seen: node BatSet.t) =
  let rec find best child seen =
    let sibling = (child#sibling seen) in
    if child#visits > best#visits
    then find child sibling (BatSet.add sibling seen)
    else find best sibling (BatSet.add sibling seen)
  in
  find root root#child seen

let uctSelect : node -> node BatSet.t -> node =
  fun node seen ->
      let rec select next best_uct res =
        Best.set_best (res#blacks, res#whites);
        match reader#get () with
        | Some _ -> (Thread.exit (); failwith "plop")
        | None ->
            let uctvalue =
              if next#visits > 0 then
                let uct = uctk *. (sqrt (log ((float_of_int node#visits) /. (float_of_int next#visits))))
                in
                (int_of_float next#getWinRate) + (int_of_float uct)
              else
                10000 + 1000 * (BatRandom.int 1000)
            in
            if uctvalue > best_uct
            then select (next#sibling (BatSet.add next seen)) uctvalue next
            else select (next#sibling (BatSet.add next seen)) best_uct res
      in
      select (node#sibling seen) 0 node

let playRandomGame: node -> Entities.gameState =
  fun node ->
      let game_over n =
        (BatBitSet.count n#blacks) + (BatBitSet.count n#whites) >= 100
      in
      let rec play node =
        if not (game_over node) then play (node#child) else node
      in
      let (blk, wht) = Scoring.score node#blacks node#whites in
      let comp = if node#color = White then (<) else (>) in
      if comp blk wht then Win else Lose

let playSimulation : node -> node BatSet.t -> unit =
  fun n seen ->
      let rec randomResult : node -> node BatSet.t -> gameState =
        fun n seen ->
            if not n#is_child_expanded && n#visits < 50 then
              playRandomGame n
            else
              let next = uctSelect n#child seen in
              randomResult next (BatSet.add n seen) |> invert_gameStatus
      in
      n#update (randomResult n seen)

let uctSearch ~nbSim ~color ~last_move ~blacks ~whites ~channel () =
  reader#set channel;
  let r = Obj.magic None in
  let rec root = new node (invert_color color) last_move blacks whites r in
  for i = 0 to nbSim do
    let _ = playSimulation root BatSet.empty in ()
  done;
  get_best_child root (BatSet.empty)
