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

let is_in stone group = BatISet.mem stone group.stones

let groups = ref (Array.make 100 { lib = max_int; stones = BatISet.empty })    (* les groupes *)
let ref_groups = Array.make 169 0  (* talbeau d'id, comme des pointeurs *)
let idx_ref_groups = ref 0
let group_of_stone s = Array.get !groups (Array.get (ref_groups) (s))
let group_zero () = group_of_stone (0)

let clean_groups () = groups := Array.make 100 { lib = max_int; stones = BatISet.empty }; idx_ref_groups := 0

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
let slookLf i stones = testLf i && BatBitSet.is_set stones (left i)
let slookRg i stones = testRg i && BatBitSet.is_set stones (right i)
let slookUp i stones = testUp i && BatBitSet.is_set stones (up i)
let slookDw i stones = testDw i && BatBitSet.is_set stones (down i)

let less_liberty s = (*Utilitée a verifier*)
  let unstone x = (Globals.board#get#unset_stone { color = Black; vert = (vertex_of_int 13 x) }) in
  (group_of_stone s).lib <- (group_of_stone s).lib -1;
  if (group_of_stone s).lib < 0 then
    BatISet.iter unstone (group_of_stone s).stones
  else
    ()

let make_group id stones =
  let color = Globals.color#get in
  let group_all stones liberties =
    let tbl = ref BatISet.empty in
    BatISet.iter (fun s -> tbl := BatISet.add s !tbl) stones;
    { lib = liberties; stones = !tbl }
  in
  let rec lookup to_look (found, liberties) seen =
    match BatEnum.get to_look with
    | None -> group_all found liberties
    | Some s -> if (BatISet.mem s seen) then lookup to_look (found, liberties) seen
        else
          let seen = BatISet.add s seen in
          match (Board.color_of_node (Globals.board#get#get s)) with
          | Empty -> lookup to_look (found, liberties +1) seen
          | c when c <> color -> less_liberty s; lookup to_look (found, liberties) seen (*a checker*)
          | _ ->
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
                  (BatEnum.push to_look) (left id);
                  (BatEnum.push to_look) (right id);
                  (BatEnum.push to_look) (up id);
                  (BatEnum.push to_look) (down id);
                  List.iter (BatEnum.push to_look) !l;
                  List.iter (fun x -> fnd := BatISet.add x !fnd) !l;
                end;
                lookup to_look (!fnd, liberties) seen)
  in
  let enm = BatEnum.empty () in
  let seen = BatISet.empty in
  let found = BatISet.empty in
  let refresh_groups grp =
    idx_ref_groups := !idx_ref_groups + 1;
    Array.set ref_groups id !idx_ref_groups;
    Array.set !groups !idx_ref_groups grp;
  in
  BatEnum.push enm id;
  refresh_groups (lookup enm (found, 0) seen)