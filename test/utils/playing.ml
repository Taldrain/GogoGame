(**
Ce Module simule un coup
**)

open Board
open Entities
open Entities.Move
open Entities.Vertex
open Entities.Color

let coloring v = { color=Black; vert=v }

let rec play ~moves = match moves with
  | [] -> ()
  | m::l -> Engine.play m

let rec play_v ~vertices = match vertices with
  | [] -> ()
  | v::l -> Engine.play (coloring v)