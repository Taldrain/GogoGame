(**
    Implemente l'historique des coups
**)

let stack = ref []

exception Cannot_undo

let play (m:Entities.Move.t) = stack := m::!stack
let undo () =
  match !stack with
    | [] -> raise Cannot_undo
    | h::t -> stack := t; h