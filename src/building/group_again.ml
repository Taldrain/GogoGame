(**
nouvelle tentative d'implementation des groupes
**)

type group = {
  (* count: int; *)
  lib: int;
  stones: BatHashtbl.t
  }

let is_in stone group = BatHashtbl.mem group.stones stone

let groups = BatDynArray.make 100      (* les groupes *)
let ref_groups = BatDynArray.make 100  (* talbeau d'id, comme des pointeurs *)
let group_of_stone s = groups.(ref_groups.(s))

let up i = i + 1
let down i = i - 1
let right i = i + 13
let left i = i - 13

let lookLf id stones = BatBitSet.is_set stones (left i)
let lookRg id stones = BatBitSet.is_set stones (right i)
let lookUp id stones = BatBitSet.is_set stones (up i)
let lookDwn id stones = BatBitSet.is_set stones (down i)

(* version safe du lookup *)
let slookLf id stones = i > 12 && BatBitSet.is_set stones (left i)
let slookRg id stones = i < 349 && BatBitSet.is_set stones (right i)
let slookUp id stones = i < 361 && BatBitSet.is_set stones (up i)
let slookDwn id stones = i > 0 && BatBitSet.is_set stones (down i)

let make_group id stones =
  let group_all stones liberties =
    let tbl = BatHashtbl.create 13 in
    List.iter (fun s -> BatHashtbl.add tbl s s) stones;
    { lib = liberties; stones = tbl }
  in
  let rec lookup to_look (found,liberties) =
    match to_look with
      | [] -> make_group found liberties
      | s::l -> 
    
