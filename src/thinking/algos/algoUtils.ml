(**
code commun aux algos de recherche
**)
open BatPervasives
open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

type state =
  {
    blk : BatBitSet.t;
    wht : BatBitSet.t;
    mov : Entities.Move.t;
  }

let state black white move =
  {
    blk = black;
    wht = white;
    mov = move;
  }

let groups_refresh grp = grp
let shapes_refresh shp = shp

let generate_next col s () =
  let col = invert_color col in
  let empties = BatBitSet.create_full 168 in
  (BatBitSet.differentiate empties (BatBitSet.union s.blk s.wht);
    let rec next_empty i =
      if not (BatBitSet.is_set empties (i +1)) then next_empty (i +2) else (i +1)
    in
    let rec gen i accu =
      if i = 168 then accu
      else
        let (blacks, whites) =
          if col = Black
          then (bitset_set_nth s.blk i, s.wht)
          else (s.blk, bitset_set_nth s.wht i)
        in
        gen (next_empty i) (BatSet.add
              (state blacks whites { color = col; vert = (vertex_of_id i) })
              accu)
    in
    gen (next_empty 0) (BatSet.empty))


