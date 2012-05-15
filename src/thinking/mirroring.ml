(**
Strategie de connard: le mirroir ! :)
**)

open Common
open Entities.Color
open Entities.Vertex
open Entities.Move

exception Not_efficient

let genmove c last_move =
  let { color=_;vert = v} = last_move in
  let { pass = p; nb = n; letter = l } = v in
  if p || (n = 7 && l = 'G') || (n = 13 && l = 'N') then raise Not_efficient
  else
    let n2 = 14 - n
    and l2 = letter_of_int (12 - (int_of_letter l)) in
    { color = c; vert = { pass = false; nb = n2; letter = l2 }}
