(**
nouvelle tentative d'implementation des groupes
**)

open Entities.Move
open Entities.Vertex
open Entities.Color

type group = {
  (* count: int; *)
  mutable lib: int;
  stones: BatISet.t
  }

let b = Globals.board#get

let is_in stone group = BatISet.mem stone group.stones

let groups = BatDynArray.make 100      (* les groupes *)
let ref_groups = BatDynArray.make 100  (* talbeau d'id, comme des pointeurs *)
let idx_ref_groups = ref 0
let group_of_stone s = BatDynArray.get groups (BatDynArray.get (ref_groups) s)

let up i = i + 1
let down i = i - 1
let right i = i + 13
let left i = i - 13

let lookLf i stones = BatBitSet.is_set stones (left i)
let lookRg i stones = BatBitSet.is_set stones (right i)
let lookUp i stones = BatBitSet.is_set stones (up i)
let lookDw i stones = BatBitSet.is_set stones (down i)

(* version safe du lookup *)
let slookLf i stones = i > 12 && BatBitSet.is_set stones (left i)
let slookRg i stones = i < 156 && BatBitSet.is_set stones (right i)
let slookUp i stones = (i mod 13) <> 12 && BatBitSet.is_set stones (up i)
let slookDw i stones = (i mod 13) <> 0 && BatBitSet.is_set stones (down i)

let less_liberty s =
  (group_of_stone s).lib = (group_of_stone s).lib -1;
  if (group_of_stone s).lib < 0 then
  BatISet.iter (fun x -> (Globals.board#get#unset_stone {color = Black; vert = (vertex_of_int 13 x) })) (group_of_stone s).stones
  else
  ()


let make_group id stones =
  let color = Globals.color#get in
  let group_all stones liberties =
    let tbl = ref BatISet.empty in
    BatISet.iter (fun s -> tbl := BatISet.add s !tbl) stones;
    { lib = liberties; stones = !tbl }
  in
  let rec lookup to_look (found,liberties) seen =
    match BatEnum.get to_look with
      | None -> group_all found liberties
      | Some s -> if (BatISet.mem s seen) then lookup to_look (found,liberties) seen
	else
	  let new_seen = BatISet.add s seen in
	  match (Board.color_of_node (b#get s)) with
	    |Empty -> lookup to_look (found, liberties+1) new_seen
	    |c when c <> color -> less_liberty s; lookup to_look (found, liberties) new_seen
	    |_ -> 
				let fnd = ref found in
	      (begin
		let l = ref [] in
	      if slookLf id stones then
		l := (left id)::!l; 
	      if slookRg id stones then
		l := (right id)::!l;
	      if slookUp id stones then
		l := (up id)::!l;
	      if slookDw id stones then
		l := (down id)::!l;
	      List.iter (BatEnum.push to_look) !l;
				List.iter (fun x -> fnd := BatISet.add x !fnd) !l;
	       end;
	     lookup to_look (!fnd, liberties) seen) 
  in
  let rec add_grp grp = 
    BatDynArray.add groups grp; 
    idx_ref_groups := !idx_ref_groups + 1;
  in
  let enm = BatEnum.empty () in
  let seen = BatISet.empty in
	let found = BatISet.empty in
  BatEnum.push enm id;
  add_grp (lookup enm (found, 0) seen)
    

 
