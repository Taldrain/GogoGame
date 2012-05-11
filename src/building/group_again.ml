(**
nouvelle tentative d'implementation des groupes
**)

type group = {
  (* count: int; *)
  lib: mutable int;
  stones: BatISet.t
  }

let b = Globals.board#get

let is_in stone group = BatHashtbl.mem group.stones stone

let groups = BatDynArray.make 100      (* les groupes *)
let ref_groups = BatDynArray.make 100  (* talbeau d'id, comme des pointeurs *)
let idx_ref_groups = ref 0
let group_of_stone s = groups.(ref_groups.(s))

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
let slookRg i stones = i < 349 && BatBitSet.is_set stones (right i)
let slookUp i stones = (i mod 13) <> 12 && BatBitSet.is_set stones (up i)
let slookDw i stones = (i mod 13) <> 0 && BatBitSet.is_set stones (down i)

let less_liberty s =
  (group_of_stone s).lib =  (group_of_stone s).lib -1;
  if (group_of_stone s).lib < 0 then
  List.iter Globals.board#get#unset_stone (group_of_stone s).stones
  else
  ()

let make_group id stones =
  let color = Globals.color#get in
  let group_all stones liberties =
    let tbl = BatISet.empty in
    BatISet.iter (fun s -> BatISet.add s tbl) stones;
    { lib = liberties; stones = tbl }
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
	      (begin
		let l = ref [] in
	      if slookLf id stones then
		l := (left id)::l; 
	      if slookRg id stones then
		l := (right id)::l;
	      if slookUp id stones then
		l := (up id)::l;
	      if slookDw id stones then
		l := (down id)::l;
	      BatList.iter (BatEnum.push to_look) l;
	        l := BatList.flatten [found;l];
	       end
	     lookup to_look (l, liberties) new_seen) 
  in
  let rec add_grp grp = 
    BatDynArray.add ref_groups grp; 
    idx_ref_groups := idx_ref_groups + 1;
    idx_ref_groups;
  in
  let enm = BatEnum.empty () in
  let seen = BatISet.empty () in
  BatEnum.push e id;
  add_grp (lookup enm ([], 0) seen)
    

 
