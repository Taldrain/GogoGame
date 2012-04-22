(**
Implemente la logique de l'IA
**)
open Entities
  
open Entities.Color
  
open Entities.Move
  
open Entities.Vertex
  
open Board
  
open Globals

let b () = Globals.b ()

let genmove c =
  let b = b () in
  let rec try_id i =
  	match b#get i with
      | Corner (_,Empty, _) | Border (_,Empty, _) | Middle (_,Empty, _) -> i
      | _ -> try_id (i+1)
  in
  {color=c; vert=(vertex_of_int b#size (try_id 0))}
