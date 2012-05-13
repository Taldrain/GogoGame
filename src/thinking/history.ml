(**
    Implemente l'historique des coups
**)
open Entities.Color
open Entities.Vertex
open Entities.Move

let stack = ref []

exception Cannot_undo

let play (m:Entities.Move.t) =
  stack := m::!stack;
  let {color=_;vert=v} = m in
  Globals.last_played#set (int_of_v v)

let undo () =
  match !stack with
    | [] -> raise Cannot_undo
    | h::t -> (
      stack := t;
      let {color=_;vert=v} = h in
      Globals.last_played#set (int_of_v v);
      h
      )