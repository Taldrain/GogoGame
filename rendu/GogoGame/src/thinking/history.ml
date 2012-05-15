(**
    Implemente l'historique des coups
**)
open Entities.Color
open Entities.Vertex
open Entities.Move

let stack = ref []
let count = ref 0

exception Cannot_undo

let peek () = List.hd !stack

let play (m:Entities.Move.t) =
  incr count;
  stack := m::!stack;
  let {color=_;vert=v} = m in
  Globals.last_played#set (int_of_v v)

let undo () =
  match !stack with
    | [] -> raise Cannot_undo
    | h::t -> (
      decr count;
      stack := t;
      let {color=_;vert=v} = h in
      Globals.last_played#set (int_of_v v);
      h
      )