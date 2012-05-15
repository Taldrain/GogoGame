(**
Aide a debuter la partie
**)
open Entities.Color
  
open Entities.Vertex
  
open Entities.Move
  
exception Not_Fuseki
  
let fuseki = ref true
  
let h = ref false
  
let mov c v = { color = c; vert = v; }
  
let ver l n = { pass = false; nb = n; letter = l; }
  
let verr (l, n) = { pass = false; nb = n; letter = l; }
  
let first_move c =
  if c = Black
  then mov c (ver 'D' 4)
  else
    (let { color = _; vert = v } = List.hd !History.stack in
     let { pass = _; nb = n; letter = l } = v
     in
       match (l, n) with
       | (l, n) when n < 6 -> (h := true; mov c (ver 'D' 10))
       | (l, n) when n > 8 -> (h := false; mov c (ver 'D' 4))
       | _ -> (h := false; mov c (ver 'D' 4)))
  
let second_black m =
  let { color = _; vert = v } = m in
  let { pass = _; nb = n; letter = l } = v
  in
    if !h
    then
      (match (l, n) with
       | ('D', 4) | ('K', 10) -> ('K', 4)
       | ('K', 4) -> ('D', 3)
       | ('L', 3) -> ('D', 4)
       | ('K', 3) -> ('C', 3)
       | ('J', 3) -> ('L', 9)
       | ('H', 3) | (_, 2) | (_, 1) -> ('F', 11)
       | (l,n) when true -> ('F', 11)
       | _ -> ('D', 3))
    else ('K',4)
  
let genmove c =
  match !History.count with
  | 0 | 1 -> first_move c
  | 2 -> mov c (verr (second_black (History.peek ())))
  | _ -> raise Not_Fuseki
  
