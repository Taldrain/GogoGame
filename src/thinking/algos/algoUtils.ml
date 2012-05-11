(**
code commun aux algos de recherche
**)

open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

type state =
  {
    blk : BatBitSet.t;
    wht : BatBitSet.t;
    grp : BatList.t;
    shp : BatList.t;
    mov : Move.t;
    (* scr : BatArray.t *)
  }

let state black white group shape move (*score*) =
  {
    blk = black;
    wht = white;
    grp = group;
    shp = shape;
    mov = move;
    (* scr = score; *)
  }


let generate_next s =
  let empties =
    (BatBitSet.union s.blk s.wht) |>
    (BatBitSet.differentiate (BatBitSet.create_full ss))
  in
  let rec next_empty i =
    if not (BatBitSet.is_set empties (i+1)) then next_empty (i+2) else (i+1)
  in
  let rec gen i accu =
    if i = ss then accu
    else
      let (blacks, whites) =
        if s.col = Black
        then (bitset_set_nth s.blk i, s.wht)
        else (s.bk, bitset_set_nth s.wht i)
      and g = groups_refresh s.grp
      and s = shapes_refresh s.shp
      and m = {color=s.col;vert=(vertex_of_int i)}
      (* in *)
      (* let score = eval (s.scr,blacks,whites,g,s) *)
      in
      gen (next_empty i) (BatSet.add
      (state black white g s {color=s.col;vert=(vertex_of_int i)})
       accu)
  in
  gen (next_empty 0) (BatSet.empty)