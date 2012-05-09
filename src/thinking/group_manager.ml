(**
Le group manager s'occupe de mettre a jour les groupes et leurs informations
au fur et a mesure des coups
**)

open Global
open BatPervasives

open Globals
open Entities
open Entities.Move
open Entities.Vertex
open Entities.Color
open Group

let g = new global "groups"
(** stocke les groupes **)
let count = ref 0

let add (e: group) =
  (if g#empty then g#set (BatDllist.create e)
    else BatDllist.add g#get e);
  incr count

let find e =
  let rec rec_find x orig l =
    let current_group = BatDllist.get l in
    if current_group = orig then None
    else if current_group#contains e then Some current_group
    else rec_find x orig (BatDllist.next l)
  in
  let current = (BatDllist.get g#get) in
  if current#contains e then Some current
  else rec_find e current (BatDllist.next g#get)

let compute_liberties () =
  let count = ref 0 in
  let rec liberties have_seen (c, i) =
    if BatHashtbl.mem !have_seen i then 0
    else
      (BatHashtbl.add !have_seen i 0;
        match c with
        | Empty -> 1
        | _ -> 0)
  in
  let tupplize i =
    let c = (board#get#get i |> Board.color_of_node) in (c, i)
  in
  let rec apply f accu l =
    match l with
    | [] -> accu
    | e:: l -> apply f (accu + (f e)) l
  in
  let iter (g: group) =
    let have_seen = (ref (BatHashtbl.create 101)) in
    BatList.Labels.iter g#stones
      ~f: (fun s -> let n = Board.get_neighbours board#get s in
            count :=
            ((BatList.map tupplize n)
              |> (apply (liberties have_seen) 0)
              |> ((+) !count)))
  in
  BatDllist.iter iter g#get

let rec del e =
  try ((BatDllist.find ((=) e) g#get) |> BatDllist.remove; decr count)
  with Not_found -> ()

let merge_groups color l =
  let groups_to_merge = BatList.filter_map identity l in
  let stones_to_group =
    groups_to_merge
    |> BatList.map (fun g -> g#stones)
    |> BatList.concat
  in
  (
    List.iter del groups_to_merge;
    let g = new Group.group color in
    List.iter g#add_stone stones_to_group;
    add g
  )

let find_common l =
  let rec find_groups ids groups =
    match ids with
    | [] -> groups
    | id:: ids ->
        try
          let g = find id in
          if List.mem g groups then find_groups ids groups
          else find_groups ids (g:: groups)
        with Not_found ->
            failwith "Group_manager.find_common [FATAL]: stone without group found"
  in
  find_groups l []

let refresh_groups m =
  let b = board#get in
  let id = Vertex.int_of_vertex b#size m.vert in
  id
  |> (Board.get_neighbours b)
  |> (BatList.filter_map (fun i ->
            if m.color = ((b#get i) |> Board.color_of_node)
            then Some i else None))
  |> (find_common)
  |> (merge_groups m.color)

let _ =
  let event_clear = Globals.event_clear#get in
  event_clear#add_handler (fun () -> g#unset);