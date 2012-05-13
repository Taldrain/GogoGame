(**
Contient le meilleur mouvement trouve jusqu'alors
**)

open Entities
open Entities.Vertex
open Entities.Color
open Entities.Move

open BatRMutex


let best = ref (BatBitSet.empty (), BatBitSet.empty ())

let set_best c =
  best := c

let get_best () = !best

let get_move board color =
  let (best_blacks,best_whites) = get_best () in
  let bitset =
    if color = Black
    then
      (let bitset = BatBitSet.copy board#blacks in
      BatBitSet.differentiate_sym bitset best_blacks;
      bitset)
    else
      (let bitset = BatBitSet.copy board#whites in
      BatBitSet.differentiate_sym bitset best_whites;
      bitset)
  in
  match BatEnum.get (BatBitSet.enum bitset) with
    | None -> failwith "Pas de coup trouve, WTF ?!"
    | Some id -> { color = color; vert = (vertex_of_id id) }