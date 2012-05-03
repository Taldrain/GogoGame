(**
Le group manager s'occupe de mettre a jour les groupes et leurs informations
au fur et a mesure des coups
**)

let g = ref []
let count = ref 0
(** stocke les groupes **)
let add e = (incr count; g := e::!g)
let find e =
  let rec findd e = function
    | [] -> None
    | g:: l -> if g#contains e then g else findd e l
  in
  findd e !g
let del e = g:= (BatList.remove !g e)

let merge_groups color id l =
  let rec merge id accu = function
    | [] -> accu
    | e:: l -> (match find e with
          | Some g -> merge id (g:: accu) l
          | None -> merge id accu l)
  in
  let rec get_stones accu = function
    | [] -> BatList.flatten accu
    | g:: l -> get_stones (g#get_stones:: accu) l
  in
  let to_merge = merge id [] l in
  let stones = get_stones [] to_merge in
  (
    List.iter del to_merge;
    let g = new Group.group color in
    List.iter g#add_stone stones;
    add g
  )

let refresh_groups m =
  let b = board#get in
  let id = Vertex.int_of_vertex b#size m.vert in
  id
  |> (Board.get_neighbours b)
  |> (BatList.map (fun i -> (b#get i) |> Board.color_of_node))
  |> (BatList.filter (fun c -> c = m.color))
  |> (merge_groups m.color id)

let _ =
  let event_clear = Globals.event_clear#get in
  event_clear#add_handler (fun () -> g := []);