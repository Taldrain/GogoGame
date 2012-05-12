(**
LA board

Un graphe, en fait. Compose de 3 types de noeuds (coins, bords, centre, doit - on
les orienter ?)
Il faudrait pouvoir lui superposer d'autes graphes comme celui de l'influence
**)

open Common
open Entities
open Entities.Move
open Entities.Color
open BatLazyList

type id = int

type node =
  | Corner of (id * Color.t * (id * id))
  | Border of (id * Color.t * (id * id * id))
  | Middle of (id * Color.t * (id * id * id * id))
(** ordre des nodes voisins : gauche, haut, bas, droite **)

let color_of_node = function
  | Corner (_, c, _) | Border (_, c, _) | Middle (_, c, _) -> c
let id_of_node = function
  | Corner (i, _, _) | Border (i, _, _) | Middle (i, _, _) -> i

exception Invalid_id of string

let down i = i - 1
let up i = i + 1
let right s i = i + s
let left s i = i - s

let node_of_int bsize i =
  let left = (left bsize)
  and right = (right bsize) in
  let predicates =
    [
    ((>=) (-1), fun x -> (raise (Invalid_id ((string_of_int i)^":to low"))));       (* on se protege des negatifs *)
    ((<=) (bsize * bsize +1), fun x -> (raise (Invalid_id ((string_of_int i)^":to high")))); (* et des trop positifs *)
    (((=) 0), fun x -> Corner(i, Empty, (up i, right i))); (* en bas a gauche *)
    (((=) (bsize -1)), fun x -> Corner(i, Empty, (left i, up i))); (* en bas a droite *)
    (((=) ((bsize -1) * bsize)), fun x -> Corner(i, Empty, (down i, right i))); (* en haut a gauche *)
    (((=) (bsize * bsize)), fun x -> Corner(i, Empty, (left i, down i))); (* en haut a droite *)
    ((>) bsize, fun x -> Border(i, Empty, (left i, up i, right i))); (* bordure basse *)
    ((<) (bsize * bsize), fun x -> Border(i, Empty, (left i, down i, right i)));  (* bordure haute *)
    ((fun i -> i mod bsize = 0), fun x -> Border(i, Empty, (up i, down i, right i))); (* bordure gauche *)
    ((fun i -> i mod bsize = (bsize -1)), fun x -> Border(i, Empty, (up i, down i, right i))); (* bordure droite *)
    ]
  and default = fun x -> Middle(i, Empty, (left i, up i, down i, right i))
  in
  switchF default predicates i

class board boardsize =
  let node_of_int = (node_of_int boardsize) and ss = (boardsize * boardsize) in
  let tmp_plateau = Array.init ss node_of_int
  in
  object (self)
    val size = boardsize
    val ssize = boardsize * boardsize
    val mutable is_clear = true
    val mutable plateau = tmp_plateau
    val blacks = BatBitSet.create ss
    val whites = BatBitSet.create ss
    
    method size = size
    method place_stone move =
      let id = (Vertex.int_of_vertex size (move.vert)) in
      try
        is_clear <- false;
        ((plateau.(id) <- match plateau.(id) with
            | Corner (i, _, n) -> Corner (i, move.color, n)
            | Border (i, _, n) -> Border (i, move.color, n)
            | Middle (i, _, n) -> Middle (i, move.color, n));
          match move.color with
          | Black -> BatBitSet.set blacks id
          | White -> BatBitSet.set whites id
          | Empty -> (BatBitSet.unset blacks id; BatBitSet.unset whites id))
      with Invalid_argument _ ->
          invalid_arg (Printf.sprintf "board.place_stone: index out of bounds (%d)" id)
    method get id = try plateau.(id)
      with Invalid_argument _ ->
          invalid_arg (Printf.sprintf "board.get: index out of bounds (%d)" id)
    method clear =
      plateau <- Array.init (boardsize * boardsize) node_of_int;
      is_clear <- true
    method is_clear = is_clear
    method unset_stone { color = _; vert = v } = self#place_stone { color = Empty; vert = v }
    method blacks = blacks
    method whites = whites
    method not_empty = (BatBitSet.union blacks whites)
    method empties = BatBitSet.differentiate_sym (BatBitSet.create ssize) self#not_empty
  end

let get_neighbours b s =
  match b#get s with
  | Corner(_, _, (c1, c2)) -> c1:: c2::[]
  | Border(_, _, (c1, c2, c3)) -> c1:: c2:: c3::[]
  | Middle(_, _, (c1, c2, c3, c4)) -> c1:: c2:: c3:: c4::[]

let id_get_neighbours s =
  let l = ref [] and m = s mod 13 in
  if s > 0 && m <> 0 then l := (s-1):: !l;
  if s < 168 && m <> 12 then l := (s+1):: !l;
  if s > 12 then l := (s-13):: !l;
  if s < 156 then l := (s+13):: !l;
  !l

let get_color blacks whites s =
  if BatBitSet.is_set blacks s 
  then Black
  else if BatBitSet.is_set whites s then White
       else Empty

(** renvoie la liste des vertices d'une zone **)
(* let rec get_stones blacks whites color seen accu s =                                          *)
(*   if BatBitSet.is_set seen s then accu                                                        *)
(*   else                                                                                        *)
(*     (BatBitSet.set seen s;                                                                    *)
(*       match get_color blacks whites s with                                                    *)
(*       | c when c = invert_color color -> accu                                                 *)
(*       | _ -> List.map (get_stones blacks whites color seen (s:: accu)) (id_get_neighbours s)) *)