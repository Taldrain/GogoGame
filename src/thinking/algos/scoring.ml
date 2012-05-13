(**
Implémente les fonctions de scoring
**)
open Entities
  
open Entities.Color
  
open Entities.Vertex
  
open Entities.Move
  
open Board
  
open BatPervasives
  
let blacks = ref (BatBitSet.empty ())

and whites = ref (BatBitSet.empty ())

and deads = BatBitSet.create 168
  
let rec unset color =
  function
  | [] -> ()
  | s :: l ->
      (BatBitSet.unset (if color = Black then !blacks else !whites) s;
       unset color l)
  
let set_deads stones = List.iter (BatBitSet.set deads) stones
  
let get_color = Board.get_color !blacks !whites
  
(* let get_stones = (Board.get_stones !blacks !whites) *)
let mem = ref BatIMap.empty
  
let get_color_terr s =
  let rec search_color open_list closed_list =
    match BatEnum.get open_list with
    | None -> failwith "couleur de territoire non trouvée"
    | Some s ->
        if BatBitSet.is_set closed_list s
        then search_color open_list closed_list
        else
          (BatBitSet.set closed_list s;
           (let c = get_color s
            in
              if c <> Empty
              then c
              else
                (List.iter (BatEnum.push open_list) (id_get_neighbours s);
                 search_color open_list closed_list)))
  in
    if BatIMap.mem s !mem
    then BatIMap.find s !mem
    else
      (let c = get_color s
       in
         if c <> Empty
         then (mem := BatIMap.add s c !mem; c)
         else
           (let c = search_color (BatList.enum [ s ]) (BatBitSet.create 168)
            in (mem := BatIMap.add s c !mem; c)))
  
(* let set_dead s = BatBitSet.set deads s *)
let is_dead s = BatBitSet.is_set deads s
  
(** @return [(bool,stones)] avec bool si la region vit, et stones les pierres de cette region **)
let rec find :
  Color.t -> BatBitSet.t -> int list -> int -> (bool * (int list)) =
  fun color seen stones s ->
    let merge ll =
      let rec merge_all (life, stones) =
        function
        | [] -> (life, stones)
        | (b, s) :: l ->
            merge_all ((b && life), (List.rev_append s stones)) l
      in merge_all (true, []) ll
    in
      if BatBitSet.is_set seen s
      then (true, stones)
      else
        (BatBitSet.set seen s;
         (match get_color s with
          | c when c = (invert_color color) -> ((is_dead s), stones)
          | c when c = color ->
              merge
                (List.map (find color seen (s :: stones))
                   (Board.id_get_neighbours s))
          | _ ->
              merge
                (List.map (find color seen stones)
                   (Board.id_get_neighbours s))))
  
(** @return [(bool,stones)] avec bool si la region vit, et stones les pierres de cette region **)
let find_region color s = find color (BatBitSet.create 168) [] s
  
exception False_result
  
let is_territory color s =
  let rec ter : (BatBitSet.t -> int list -> int -> (bool*int list)) =
    fun seen accu s ->
    match get_color s with
    | c when (c = (invert_color color)) && (not (is_dead s)) -> (false, accu)
    | c when c = color -> ((not (is_dead s)), accu)
    | _ ->
        if BatBitSet.is_set seen s
        then (true, accu)
        else
          (BatBitSet.set seen s;
           (try
              (List.iter
                 (fun stone ->
                    let (b,stones) = (ter seen (s :: accu) stone) in
                    if not b
                    then raise False_result
                    else ())
                 (Board.id_get_neighbours s);
               (true, s::accu))
            with | False_result -> (false, accu)))
  in ter (BatBitSet.create 168) [] s
  
let set_score scored score color stone =
  let set = if color = White then BatBitSet.unset else BatBitSet.set
  in (BatBitSet.set scored stone; set score stone)
  
let set_all_score scored score color stones =
  List.iter (set_score scored score color) stones
  
let clear bitset ids = List.iter (BatBitSet.unset bitset) ids
  
let mark_deads () =
  let are_alive color stones =
    let one = ref false
    in
      List.exists
        (fun s ->
           if (get_color s) <> Empty
           then false
           else
             (let l = Board.id_get_neighbours s
              in
                if
                  List.for_all
                    (fun s ->
                       let c = get_color s in (c = color) || (c = Empty))
                    l
                then if !one then (one := true; false) else true
                else false))
        stones in
  let rec travel color seen s = find color seen [] s in
  let seen = BatBitSet.create 168
  in
    for i = 0 to 168 do
      let color =
        let c = get_color i in if c = Empty then get_color_terr i else c in
      let (_, stones) = travel color seen i
      in
        if BatList.is_empty stones
        then ()
        else
          if are_alive (get_color_terr i) stones
          then ()
          else set_deads stones
    done
  
let mark_score () =
  let rec mark_stone scored score s =
    if s = 169
    then (scored, score)
    else
      (let c = get_color s
       in
         (if (c <> Empty) && (not (is_dead s))
          then set_score scored score c s
          else ();
          mark_stone scored score (s + 1))) in
  let rec mark_territory seen scored score s =
    (if s = 169
     then (scored, score)
     else
       if BatBitSet.is_set seen s
       then mark_territory seen scored score (s + 1)
       else
         (let (territory, terr) = is_territory Black s
          in
            if territory
            then (set_all_score scored score Black terr; (scored,score))
            else
              (clear seen terr;
               (let (territory, terr) = is_territory White s
                in
                  if territory
                  then (set_all_score scored score White terr; (scored,score))
                  else (clear seen terr; (scored,score)))));
     mark_territory seen scored score (s + 1)) in
  let scored = BatBitSet.create 168 and score = BatBitSet.create 168 in
  let (scored, score) = mark_stone scored score 0
  in mark_territory (BatBitSet.create 168) scored score 0
  
(** @return le score (black,white) *)
let score blk wht =
  (blacks := blk;
   whites := wht;
   mark_deads ();
   let (score, scored) = mark_score ()

   and b = ref 0

   and w = ref 0
   in
     (for i = 0 to 168 do
        if BatBitSet.is_set scored i
        then if BatBitSet.is_set score i then incr b else incr w
        else ()
      done;
      ((!b), (!w))))
  
