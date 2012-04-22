(**
LA board

Un graphe, en fait. Compose de 3 types de noeuds (coins, bords, centre, doit - on
les orienter ?)
Il faudrait pouvoir lui superposer d'autes graphes comme celui de l'influence
**)

open Common

type case = | Empty | Black | White

type id = int

type node =
  | Corner of (id * (node * node))
  | Border of (id * (node * node * node))
  | Middle of (id * (node * node * node * node))
(** ordre des nodes voisins : gauche, haut, bas, droite **)

exception Invalid_id

let left i = i - 1
let right i = i + 1
let up s i = i + s
let down s i = i - s

let node_of_int bsize i =
  let up = (up bsize)
  and down = (down bsize) in
  let predicates =
    [
    ((<) 0, (raise Invalid_id));       (* on se protege des negatifs *)
    ((>=) ((bsize +1) * bsize), (raise Invalid_id)); (* et des trop positifs *)
    (((=) 0), Corner(i, (up i, right i))); (* en bas a gauche *)
    (((=) (bsize -1)), Corner(i, (left i, up i))); (* en bas a droite *)
    (((=) ((bsize-1) * bsize)), Corner(i, (down i, right i))); (* en haut a gauche *)
    (((=) (bsize* bsize)), Corner(i, (left i, down i))); (* en haut a droite *)
    ((<) bsize, Border(i, (left i, up i, right i))); (* bordure basse *)
    ((>) (bsize * bsize), Border(i, (left i, down i, right i)));  (* bordure haute *)
    ((fun i -> i mod bsize = 0), Border(i,(up i,down i,right i))); (* bordure gauche *)
    ((fun i -> i mod bsize = (bsize-1)), Border(i,(left i,up i,down i,right i))); (* bordure droite *)
    ]
  and default = Middle(i,(left i,up i,down i,right i))
  in
  switchF default predicates i

class board boardsize =
  let node_of_int = (node_of_int boardsize) in
  let tmp_plateau = Array.init (boardsize*boardsize) node_of_int
  in
  object (self)
    val size = boardsize
    val mutable is_clear = false
    val plateau = tmp_plateau
    
    method size = size
    
  end
