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

let add e =
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

let del e =
  let rec search_and_destroy e l orig =
    let current = BatDllist.get l in
    if current = orig then ()
    else
    if current#contains e then (decr count; BatDllist.remove l; ())
    else search_and_destroy e (BatDllist.next l) orig
  in
  let current = BatDllist.get g#get in
  if current#contains e then (decr count; g#set (BatDllist.drop g#get))
  else search_and_destroy e (BatDllist.next g#get) current

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
  let iter (g:group) =
    let have_seen = (ref (BatHashtbl.create 101)) in
    BatList.Labels.iter g#stones
      ~f: (fun s -> let n = Board.get_neighbours board#get s in
            count :=
            ((BatList.map tupplize n)
              |> (apply (liberties have_seen) 0)
              |> ((+) !count)))
  in
  BatDynArray.iter iter g#get

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