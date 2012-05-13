(**
nouvelle tentative d'implementation des groupes
**)

open Entities.Move
open Entities.Vertex
open Entities.Color

type group = {
  (* count: int; *)
  mutable lib: int;
  stones: int BatList.t
}

let groups = ref (Array.make 100 { lib = max_int; stones = [] })    (* les groupes *)
let ref_groups = Array.make 169 0  (* talbeau d'id, comme des pointeurs *)
let idx_ref_groups = ref 0
let group_of_stone s = !groups.(ref_groups.(s))
let group_zero () = group_of_stone (0)

let clean_groups () = groups := Array.make 100 { lib = max_int; stones = [] }; idx_ref_groups := 0

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

let less_liberty s = (*Utilit�e a verifier*)
  if (group_of_stone s <> group_zero ())
  then
    (let unstone x = (Globals.board#get#unset_stone { color = Black; vert = (vertex_of_int 13 x) }) in
      (group_of_stone s).lib <- (group_of_stone s).lib -1;
      if (group_of_stone s).lib < 0 then
        List.iter unstone (group_of_stone s).stones
    )
  else
    ()

let rec make_group id stones =
  let color = if BatBitSet.is_set Globals.board#get#blacks id then Black else White in
  let group_all stones liberties =
    let tbl = ref [] in
    List.iter (fun s -> tbl := s::!tbl) stones;
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
              let l = ref [] in
              begin
                begin
                  if testLf s then
                    (BatEnum.push to_look (left s);
                      if slookLf s stones then
                        l := (left s)::!l;)
                end;
                begin
                  if testRg s then
                    (BatEnum.push to_look (right s);
                      if slookRg s stones then
                        l := (right s)::!l;)
                end;
                begin
                  if testUp s then
                    (BatEnum.push to_look (up s);
                      if slookUp s stones then
                        l := (up s)::!l;)
                end;
                begin
                  if testDw s then
                    (BatEnum.push to_look (down s);
                      if slookDw s stones then
                        l := (down s)::!l;)
                end;
                List.iter (BatEnum.push to_look) !l;
                List.iter (fun x -> fnd := x::!fnd) !l;
              end;
              lookup to_look (!fnd, liberties) seen
  in
  let refresh_groups group =
    idx_ref_groups := 1 + !idx_ref_groups;
    let rec num_of_groups stones l =
      match stones with
      | [] -> 0
      | s:: t -> if List.exists (fun x -> x = (group_of_stone s)) l then
            (begin
                ((ref_groups.(s) <- ref_groups.(id));
                  num_of_groups t l)
                
              end;)
          else
            (begin
                (ref_groups.(s) <- ref_groups.(id);
                  1 + (num_of_groups t ((group_of_stone s):: l)))
              end;)
    in
    let { lib = libert; stones = stn } = group in
    Array.set ref_groups id !idx_ref_groups;
    idx_ref_groups := !idx_ref_groups - (num_of_groups stn []);
    Array.set !groups !idx_ref_groups group;
  in
  let enm = BatEnum.empty () in
  let seen = BatISet.empty in
  BatEnum.push enm id;
  refresh_groups (lookup enm ([], 0) seen)
