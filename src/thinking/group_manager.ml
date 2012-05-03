(**
Le group manager s'occupe de mettre a jour les groupes et leurs informations
au fur et a mesure des coups
**)

open Global

let g = new global "groups"
(** stocke les groupes **)
let count = ref 0

let add e =
  (if g#empty then g#set (BatDllist.create e)
    else g#set (BatDllist.add g#get e));
  incr count

let find e =
  let rec rec_find x orig l =
    let current_group = BatDllist.get l in
    if current_group = orig then None
    else if current_group#contains e then Some current_group
    else rec_find x orig (BatDllist.next l)
  in
  let current = (BatDllist.get g) in
  if current#contains e then Some current
  else rec_find e current (BatDllist.next g)

let del e =
  let rec search_and_destroy e l orig =
    let current = BatDllist.get l in
    if current = orig then false
    else
    if current#contains e then (decr count; BatDllist.remove l)
    else search_and_destroy e (BatDllist.next l) orig
  in
  let current = BatDllist.get g#get in
  if current#contains e then (decr count; g#set (BatDllist.drop g#get))
  else search_and_destroy e (BatDllist.next g#get) g#get

let compute_liberties () =
  let rec liberties have_seen v =
    if BatHashtbl.mem !have_seen v then 0
    else let { color = c; vert = _ } = v
      in
      BatHashtbl.add !have_seen v 0;
      match c with
      | Empty -> 1
      | _ -> 0
  in
  let rec iter l have_seen accu =
    match l with
    | [] -> accu
    | s:: l ->
        let n = Board.get_neighbours b#get s in
        (BatList.map (fun i -> (b#get#get i) |> Board.color_of_node))
        |> BatList.fold_right (liberties have_seen) 0
        |> ((+) accu)
        |> (iter l have_seen)
  in
  iter stones (ref (BatHashtbl.create 101)) 0


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
  event_clear#add_handler (fun () -> g#unset);