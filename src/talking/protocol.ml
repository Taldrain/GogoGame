(**
Tout ce qui concerne le protocole GTP
**)

open Entities

type command =
  (* administrative commands *)
  | Protocol_version    (* OUTPUT : version of the GTP protocol, 2 *)
  | Name                (* OUTPUT : nom de l'ia, Gogo Game *)
  | Version             (* OUTPUT : version de l'ia, 1.0 *)
  | Known_command of string (* OUTPUT : vrai si on connait la commande *)
  | List_commands       (* OUTPUT : les commandes connues *)
  | Quit                (* quitte *)
  
  (* setup commands *)
  | Boardsize of int
  (* regle la taille du plateau * FAIL: unacceptable size *)
  
  | Clear_board         (* vide le plateau *)
  
  | Komi of float
  (* nouvelle valeur du komi. DOIT etre accepte, meme si ridicule FAIL:    *)
  (* syntax error                                                          *)
  
  | Fixed_handicap of int
  (* Pierres a placer pour handicap a emplacement pre-fixe OUTPUT : la     *)
  (* liste des vertices ou ont ete placees les pierres FAIL : syntax       *)
  (* error, invalid number of stones, board not empty                      *)
  
  | Place_free_handicap of int
  (* Pierres a placer pour handicap a emplacement libre OUTPUT : la liste  *)
  (* des vertices ou ont ete placees les pierres FAIL : syntax error,      *)
  (* invalid number of stones, board not empty, bad vertex list            *)
  
  | Set_free_handicap of Vertex.t list
  (* Les pierres ont ete placees dans les vertices comme demandees FAIL :  *)
  (* syntax error, invalid number of stones, board not empty, bad vertex   *)
  (* list                                                                  *)
  
  (* core play commands *)
  | Play of Move.t
  (* Une pierre de la couleur demandee est jouee au Vertex demande. Le     *)
  (* nombre de pierres jouees est mis a jour et le mouvement est ajoute a  *)
  (* l'historique des nouvements FAIL : syntax error, illegal move. In the *)
  (* latter case, fails with the error message ``illegal move''.           *)
  
  | GenMove of Color.t
  (* Une pierre de la couleur demandee est jouee la ou le moteur le        *)
  (* decide. Le nombre de pierres capturees est mis a jour si besoin et le *)
  (* mouvement est ajoute a l'historique OUTPUT : la Vertex jouee ou       *)
  (* "resign" en cas d'abandon (note: "pass" est une vertex valide)        *)
  
  | Undo
(* La configuration du plateau et le nombre de pierres capturees sont    *)
(* remises a l'etat precedant. Le dernier mouvement est supprime de      *)
(* l'historique FAIL : If the engine is unable to take back the last     *)
(* move, fails with the error message "cannot undo"                      *)

(* tournament commands *)
(*| Time_settings*)
(*| Time_left*)
(*| Final_score*)
(*| Final_status_list*)

(* regression commands *)
(*| Loadgsf*)
(*| Reg_genmove*)

(* debug commands *)
  | Showboard

let string_of_command = function
  | Protocol_version -> "protocol_version"
  | Version -> "version"
  | Name -> "name"
  | Known_command s -> "known_command "^s
  | List_commands -> "list_commands"
  | Quit -> "quit"
  | Boardsize i -> "boardsize "^(string_of_int i)
  | Clear_board -> "clean_board"
  | Komi f -> "komi "^(string_of_float f)
  | Fixed_handicap i -> "fixed_handicap "^(string_of_int i)
  | Place_free_handicap i -> "place_free_handicap "^(string_of_int i)
  | Set_free_handicap l -> "set_free_handicap "
      ^(BatList.fold_right (fun v s -> s^" "^(Vertex.string_of_vertex v)) l "")
  | Play m -> "play "^(Move.string_of_move m)
  | GenMove c -> "genmove "^(Color.string_of_color c)
  | Undo -> "undo"
  | Showboard -> "showboard"


type message =
  | Command of command
  | CommandFull of (int * command)
  | Success
  | SuccessFull of (int * string)
  | SuccessID of int
  | SuccessSTR of string
  | FailureFull of (int * string)
  | Failure of string


let ok () =
  ()
