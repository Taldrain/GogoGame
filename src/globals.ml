(**
    Ce module contient toutes les variables gloables du programme

    Si si, des fois ca sert ;-)
**)
let komi = ref 0.

let board = BatGlobal.empty "board"

let b () = BatGlobal.get board
let set_board b = BatGlobal.set board b

let llast_played = BatGlobal.empty "last_played"
let last () = BatGlobal.get llast_played
let set_last l = BatGlobal.set llast_played l

(* ces fonctions ne sont la que pour aider le compilo a determiner le type *)
let init_board () = BatGlobal.set board (new Board.board 7)
  
let init_last_played () =
  BatGlobal.set llast_played 0
(*    {
      Entities.Vertex.letter = 'A';
      Entities.Vertex.pass = false;
      Entities.Vertex.nb = 0;
    }
*)
