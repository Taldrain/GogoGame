(**
nouvelle tentative d'implementation des groupes
**)
open BatPervasives

open Entities.Move

open Entities.Vertex

open Entities.Color

type group = { (* count: int; *) mutable lib : int; stones : int BatList.t }

module Groups =
struct
  let count_groups = ref 0
  
  class contener group =
    let this_id = (incr count_groups; !count_groups)
    in
    object val mutable id = (this_id : int)
      val mutable g = (group : group)
      
      method get = g
      method get_id = id
      method set_grp = fun grp -> g <- grp
      
      method set_id = fun id1 -> id <- id1
    end
  
  let dummy_group = { lib = 0; stones = []; }
  
  let groups = Array.make 168 (new contener dummy_group)
  
  let group_of_stone s = groups.(s)#get
  
  let get_group_id s = groups.(s)#get_id
  
  let change_group s1 id =
    let { lib = l; stones = stn } = group_of_stone s1
    in
    (List.iter
        (fun s ->
              (groups.(s)#set_grp (group_of_stone id);
                groups.(s)#set_id groups.(id)#get_id))
        stn;
      groups.(s1)#set_grp (group_of_stone id);
      groups.(s1)#set_id groups.(id)#get_id;
      count_groups := !count_groups - 1)
  
  let new_group s group = groups.(s) <- new contener group
  
  let clean_groups () =
    (Array.fill groups 0 168 (new contener dummy_group); count_groups := 0)
  
end

open Groups

let up i = i + 1

let down i = i - 1

let right i = i + 13

let left i = i - 13

let testUp id = (id mod 13) <> 12

let testDw id = (id mod 13) <> 0

let testLf id = id > 12

let testRg id = id < 156

let lookLf i stones = BatBitSet.is_set stones (left i)

let lookRg i stones = BatBitSet.is_set stones (right i)

let lookUp i stones = BatBitSet.is_set stones (up i)

let lookDw i stones = BatBitSet.is_set stones (down i)

(* version safe du lookup *)
let slookLf i stones = (testLf i) && (BatBitSet.is_set stones (left i))

let slookRg i stones = (testRg i) && (BatBitSet.is_set stones (right i))

let slookUp i stones = (testUp i) && (BatBitSet.is_set stones (up i))

let slookDw i stones = (testDw i) && (BatBitSet.is_set stones (down i))

let less_liberty s = (* Utilite a verifier *)
  if (group_of_stone s) <> dummy_group
  then
    (let unstone x =
        Globals.board#get#unset_stone
          { color = Black; vert = vertex_of_int 13 x; }
      in
      ((group_of_stone s).lib <- (group_of_stone s).lib - 1;
        if (group_of_stone s).lib <= 0
        then (List.iter unstone (group_of_stone s).stones; 
            Groups.count_groups := !Groups.count_groups-1)
        else ()))
  else ()

let rec make_group color blacks whites id =
  let stones = BatBitSet.union blacks whites in
  let rec lookup to_look (found, liberties) seen =
    match BatEnum.get to_look with
    | None -> { lib = liberties; stones = found; }
    | Some s ->
        if BatISet.mem s seen
        then lookup to_look (found, liberties) seen
        else
          (let seen = BatISet.add s seen
            in
            match color_of_blk_wht blacks whites s with
            | Empty -> lookup to_look (found, (liberties + 1)) seen
            | c when c <> color ->
                (less_liberty s; lookup to_look (found, liberties) seen)
            | _ ->
                let fnd = ref found
                
                and l = ref []
                in
                (if testLf s
                  then
                    (BatEnum.push to_look (left s);
                      if slookLf s stones then l := (left s) :: !l else ())
                  else ();
                  if testRg s
                  then
                    (BatEnum.push to_look (right s);
                      if slookRg s stones then l := (right s) :: !l else ())
                  else ();
                  if testUp s
                  then
                    (BatEnum.push to_look (up s);
                      if slookUp s stones then l := (up s) :: !l else ())
                  else ();
                  if testDw s
                  then
                    (BatEnum.push to_look (down s);
                      if slookDw s stones then l := (down s) :: !l else ())
                  else ();
                  List.iter (BatEnum.push to_look) !l;
                  List.iter (fun x -> fnd := x :: !fnd) !l;
                  lookup to_look ((!fnd), liberties) seen)) in
  let refresh_groups group =
    (new_group id group;
      ((Board.id_get_neighbours id) |>
        (BatList.filter (fun i -> color = (color_of_blk_wht blacks whites i))))
      |> (List.iter (fun s -> change_group s id))) in
  let enm = BatEnum.empty () in
  let seen = BatISet.empty
  in (BatEnum.push enm id; refresh_groups (lookup enm ([], 0) seen))
