(**
Cette classe sert a representer un groupe de pierres sur la board
C'est une classe afin de permettre de l'etendre plus tard avec des
combinaisons et groupes speciaux
**)
open Entities.Move
open Entities.Color
open Entities.Vertex

let b = Globals.board
let c = Globals.color
let cur = ref 0
let get_id () = (incr cur; !cur)

class group color =
object (self)
  val mutable stones = []
  val mutable count = 0
  val c = color
  val id = get_id ()

  method add_stone v =
    (
      stones <- v:: stones;
      count <- count + 1;
      self#compute_liberties
    )

  method stones = stones
  method contains s = List.mem s stones

  method compute_liberties =
    let rec liberties v have_seen =
      if BatHashtbl.mem !have_seen v then 0
      else let { color = c; vert = _ } = v
        in
        BatHashtbl.add !have_seen v 0;
        match c with
        | Empty -> 1
        | _ -> 0
    in
    let rec compute have_seen l accu =
      match l with
      | [] -> accu
      | s:: l -> compute have_seen l (accu + (liberties s have_seen))
    in
    let rec iter l have_seen accu =
      match l with
      | [] -> accu
      | s:: l -> let n = Board.get_neighbours b#get s in
        let n = (BatList.map (fun i -> (b#get#get i) |> Board.color_of_node))
          iter l have_seen (accu + (compute have_seen n 0))
    in
    iter stones (ref (BatHashtbl.create 101)) 0
end
