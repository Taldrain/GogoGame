(**
Ce module contient toutes les variables gloables du programme

Si si, des fois ca sert ;-)
**)
open Global

let board = new global "board"
let last_played = new global "last_played"
let color = new global "color"

let komi = ref 0.

let event_clear = new global "event_clear"

module Hidden =
struct
  (* ces fonctions ne sont la que pour aider le compilo a determiner le    *)
  (* type                                                                  *)
  let init_board () = board#set (new Board.board 7)
  let init_last_played () = last_played#set 0
  let init_color () = color#set (Entities.Color.color_of_string "B")
end

let _ =
  event_clear#set (new Events.event) 