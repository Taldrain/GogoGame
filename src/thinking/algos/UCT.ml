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

(* let reader : (unit -> unit option) Global.global = new Global.global "reader" *)
(* let file = BatFile.open_out ~mode:[`create;`append;`text;`nonblock] "log/UCT.log" *)
let trace str = ()(*(BatIO.nwrite file)*)

let clock = ref 0.
let start () = clock := Unix.time ()
let remaining () = Unix.time () -. !clock

let choose l = BatList.at l (BatRandom.int (List.length l))
let choose_diff l1 l2 =
  let c = ref (choose l1) in
  while List.mem !c l2 do
    c := choose l1
  done; !c

let compute_uct v1 visits winRate =
  if visits > 0 then
    let uct = uctk *. (sqrt (log ((float_of_int v1) /. (float_of_int visits))))
    in
    (int_of_float winRate) + (int_of_float uct)
  else
    10000 + 1000 * (BatRandom.int 1000)

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

  val mutable children : node list option = None

  method color = color
  method visits = visits
  method blacks = blk
  method whites = wht
  method is_child_expanded = is_child_expanded
  
  method child : node =
    match children with
    | None -> self#expand_children; self#child
    | Some set -> choose set
  
  method get_sibling : node list -> node =
    fun seen ->
        match children with
        | None -> failwith "daFuk ?"
        | Some set -> choose_diff set seen
  
  method sibling : node list -> node =
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
    children <- Some (List.map gen_node set)
  
  method update : gameState -> unit =
    fun result ->
        visits <- visits + 1;
        if result = Win then wins <- wins + 1 else ()

  method getWinRate =
    if visits > 0
    then (float_of_int wins) /. (float_of_int visits)
    else 0. (* should not happend *)

  method find_best_child =
    match children with
    | None -> self#child
    | Some set ->
      let first = List.hd set in
      let uct_base = compute_uct first#visits first#visits first#getWinRate in
      fst (List.fold_left
      (fun (b, buct) n ->
        let uctvalue = compute_uct b#visits n#visits n#getWinRate in
        if buct > uctvalue then (b, buct) else (n, uctvalue))
        (first,uct_base)
        set)

end

let playRandomGame: node -> Entities.gameState =
  fun node ->
      let game_over n =
        try
        (BatBitSet.count n#blacks) + (BatBitSet.count n#whites) >= 100
        with _ -> true
      in
      let rec play node =
        trace ".";
        if not (game_over node) then play (node#child) else node
      in
      trace "\n** Playing a game: ";
      let result = play node in
      trace "\t\t[SCORING]\n";
      let (blk, wht) = Scoring.score result#blacks result#whites trace in
      let comp = if node#color = White then (<) else (>) in
      trace "GAME OVER";
      if comp blk wht then (trace "/Win\n"; Win) else (trace "/Lose\n"; Lose)

let playSimulation : node -> node list -> unit =
  fun n seen ->
      let rec randomResult : node -> node list -> gameState =
        fun n seen ->
            if n#visits < 0
            then
              (trace "\t\t[RANDOM GAME]";
                playRandomGame n)
            else
              (trace "\t\t[RANDOM RESULT]\n";
                let next = n#find_best_child in
                randomResult next (n:: seen) |> invert_gameStatus)
      in
      trace "\nPlaying simulation...";
      n#update (randomResult n seen)

let uctSearch ~nbSim ~color ~last_move ~blacks ~whites () =
  start ();
  trace "\n************************************************\n";
  trace "Beginning UCT...\n\n";
  (* reader#set channel; *)
  let r = Obj.magic None in
  let rec root = new node (invert_color color) last_move blacks whites r in
  while true do
    let _ = playSimulation root [] in ()
  done;
  let best = root#find_best_child in
  (best#blacks,best#whites)
