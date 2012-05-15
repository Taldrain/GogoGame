(**
Implemente la logique de l'IA
**)
open BatPervasives
open Entities
open Entities.Color
open Entities.Move
open Entities.Vertex
open Board
open Globals
open Common
open AlgoUtils

let _ = BatRandom.self_init ()

let refresh_groups move =
  let { color = c; vert = v } = move in
  let { pass = p ; nb = _ ; letter = _ } = v in
  if p then ()
  else
    Group_again.make_group c board#get#blacks board#get#whites
      (int_of_v v)

let rec play_randomly c b =
  let n = BatRandom.int 14 and l = (letter_of_int (BatRandom.int 14)) in
  let id = int_of_v {pass=false; nb = n; letter = l} in
  if bitSet_is_set b#not_empty id then play_randomly c b
  else { color = c; vert = { pass = false; nb = n; letter = l}}


let use_uct c b =
  let (blk,wht) = UCT.uctSearch
      ~nbSim:500
      ~color: c
      ~last_move: (Globals.last_played#get)
      ~blacks: b#blacks
      ~whites: b#whites
      ()
  in
  let diff = 
  if c = Black then
    let diff = BatBitSet.copy b#blacks in
    BatBitSet.diff blk diff
  else
    let diff = BatBitSet.copy b#whites in
    BatBitSet.diff wht diff
  in
  match BatEnum.get (BatBitSet.enum diff) with
    | None -> play_randomly c b
    | Some i -> { color = c; vert = (vertex_of_id i)}

let use_negascout c b =
  let move = { color = (invert_color c); vert = (vertex_of_id Globals.last_played#get)} in
  let s = { blk = b#blacks; wht = b#whites; mov = move } in
  let (_,s) = Negascout.find_best c (AlgoUtils.generate_next (invert_color c) s ())
  in
  s.mov

let use_mirroring c b =
  let move = { color = (invert_color c); vert = (vertex_of_id Globals.last_played#get)} in
  Mirroring.genmove c move b

let rec genmove c =
  if !Fuseki.fuseki
  then (* premier coup, on joue D4 *)
  try
    Fuseki.genmove c
  with Fuseki.Not_Fuseki -> (Fuseki.fuseki := false; genmove c)
  else (
      let b = board#get in
      try
        use_mirroring c b
      with Mirroring.Not_efficient ->
        play_randomly c b
      )



let _ =
  let event_clear = Globals.event_clear#get in
  event_clear#add_handler (fun () -> Globals.board#get#clear)
