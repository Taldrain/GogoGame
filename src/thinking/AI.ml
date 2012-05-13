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


let refresh_groups move =
  let {color=c; vert={pass=p;nb=n;letter=l}} = move in
  if p then ()
  else
    (Group_again.make_group (int_of_vertex 13 ({pass=p;nb=n;letter=l})) (BatBitSet.union board#get#blacks board#get#whites))


let genmove c =
  let b = board#get in
  let best = UCT.uctSearch 50 c b#blacks b#whites in
  let bitset =
    if c = Black
    then
      (let bitset = BatBitSet.copy b#blacks in
      BatBitSet.differentiate_sym bitset best#blacks;
      bitset)
    else
      (let bitset = BatBitSet.copy b#whites in
      BatBitSet.differentiate_sym bitset best#whites;
      bitset)
  in
  match BatEnum.get (BatBitSet.enum bitset) with
    | None -> failwith "Pas de coup trouvé, WTF ?!"
    | Some id -> { color = c; vert = (vertex_of_id id) }

(* let genmove c =                                                               *)
(*   let b = board#get in                                                        *)
(*   let rec try_id i =                                                          *)
(*     match b#get i with                                                        *)
(*     | Corner (_, Empty, _) | Border (_, Empty, _) | Middle (_, Empty, _) -> i *)
(*     | _ -> try_id (i +1)                                                      *)
(*   in                                                                          *)
(*   { color = c; vert = (vertex_of_int b#size (try_id 0)) }                     *)

let _ =
  let event_clear = Globals.event_clear#get in
  event_clear#add_handler (fun () -> Globals.board#get#clear)
