(** Parser implemente le protocole GTP **)
open Batteries_uni
open Entities
open Protocol

module Formatter =
struct
 (** le formatter s'occupe de bien formatter l'output **)

  open Protocol

  let catched_id = ref false
  let id = ref (-1)
  let set_id i = catched_id := true; id := i

  let _id () = if !catched_id then (catched_id := false; (string_of_int !id)) else "" 
  let rec iter_s = function
      [] -> ""
    | e::l -> e^(iter_s l)


  let format = function
    | Success | SuccessID _ -> "="^(_id ())^"\n\n"
    | SuccessSTR str | SuccessFull (_,str) -> "="^(_id ())^" "^str^"\n\n"
    | SuccessLST lst | SuccessLSTID (_,lst) -> "="^(_id ())^" "^(iter_s lst)^"\n\n"
    | FailureFull(_,str) | Failure str -> "?"^(_id ())^" "^str^"\n\n"
    | Command cmd -> (string_of_command cmd)^"\n"
    | CommandFull (id,cmd) -> (set_id id; (string_of_command cmd)^"\n")
end

exception Protocol_error
exception Unknown_command
exception Syntax_error

(** retire les chars invalides de l'input, a savoir:
- les chars de controle
- les lignes blanches
- les commentaires
**)
let preprocess str =
  let rec parse e last_is_space accu =
    match Enum.get e with
    | None -> accu
    | Some c ->
        let n = Char.code c
        in
        (match n with
          | (* fin de ligne ou comment*) 10 | 35 -> accu
          | (* espacements *) 9 | 32 ->
              if not last_is_space
              then parse e true (Common.enum_push accu ' ')
              else parse e true accu
          | (* char de controle *) x when (x < 32) || (x = 127) ->
              parse e true accu
          | (* char normal *) _ -> parse e false (Common.enum_push accu c))
  in
  ((parse (String.enum str) true (Enum.empty ())) |> String.of_backwards)
  |> String.enum

let slurp_enum e = (BatString.of_enum e) |> BatString.trim

let parse_vertex_list e =
  let rec parse str l =
    try
      let (h, body) = BatString.split str " " in
      let v = Vertex.vertex_of_string h in
      parse body (v:: l)
    with Not_found -> (Vertex.vertex_of_string str):: l
  in
  if (BatEnum.count e) = 0 then raise Syntax_error
  else parse (BatString.of_enum e) []

let get_nb e =
  let rec get_nb e nb =
    match Enum.get e with
    | None -> nb
    | Some c ->
        (match c with
          | ('0' .. '9' as n) -> get_nb e ((nb * 10) + (Common.int_of_char n))
          | ' ' | '\n' -> nb
          | _ -> raise Syntax_error)
  in
  if BatEnum.count e = 0 then raise Syntax_error
  else get_nb e 0

let drop_one e = (BatEnum.drop 1 e; e)

let verify_cmd_name e cmd =
  let i = ref (-1) and len = String.length cmd in
  let e' = BatEnum.take_while (fun c -> (incr i; !i < len && c = cmd.[!i])) e
  in len = (BatEnum.count e')

let rec parse_cmd e =
  let get e =
    match BatEnum.get e with | None -> raise Protocol_error | Some c -> c
  
  and verify e cmd success =
    if verify_cmd_name e cmd then success e else raise Unknown_command
  in
  match get e with
  | 'b' -> verify e "oardsize" (fun e -> Boardsize (get_nb (drop_one e)))
  | 'c' -> verify e "lear_board" (fun e -> Clear_board)
  | 'f' ->
      verify e "ixed_handicap"
        (fun e -> Fixed_handicap (get_nb (drop_one e)))
  | 'g' ->
      (match get e with
        | 'e' -> verify e "nmove"
              (fun e ->
                    GenMove
                    (((drop_one e) |> BatString.of_enum) |> Color.color_of_string))
        | 'g' -> verify e "g-plop" (fun e -> List_commands)
        | _ -> raise Unknown_command)
  | 'k' ->
      (match get e with
        | 'n' ->
            verify e "own_command"
              (fun e -> Known_command ((drop_one e) |> BatString.of_enum))
        | 'o' ->
            verify e "mi"
              (fun e ->
                    Komi
                    (((drop_one e) |> BatString.of_enum) |> BatFloat.
                      of_string))
        | _ -> raise Unknown_command)
  | 'l' -> verify e "ist_commands" (fun e -> List_commands)
  | 'n' -> verify e "ame" (fun e -> Name)
  | 'p' ->
      (match get e with
        | 'r' -> verify e "otocol_version" (fun e -> Protocol_version)
        | 'l' ->
            if not ((get e) = 'a')
            then raise Unknown_command
            else
              (match get e with
                | 'c' ->
                    verify e "e_free_handicap"
                      (fun e -> Place_free_handicap (get_nb (drop_one e)))
                | 'y' ->
                    if (get e) = ' '
                    then Play (Move.move_of_string (BatString.of_enum e))
                    else raise Unknown_command
                | _ -> raise Unknown_command)
        | _ -> raise Unknown_command)
  | 'q' -> verify e "uit" (fun e -> Quit)
  | 's' ->
      (match get e with
        | 'e' -> verify e "t_free_handicap " (* wut *)
              (fun e -> Set_free_handicap (parse_vertex_list e))
        | 'h' -> verify e "owboard"
              (fun e -> Showboard)
        | _ -> raise Unknown_command)
  | 'u' -> verify e "ndo" (fun e -> Undo)
  | 'v' -> verify e "ersion" (fun e -> Version)
  | _ -> raise Unknown_command

(** extrait le type de communication et son id si specifie **)
let decode_line e =
  let peek e =
    match BatEnum.peek e with | None -> raise Protocol_error | Some c -> c
  in
  match peek e with
  | '=' ->
      let e = drop_one e
      in
      (match peek e with
        | '\n' -> Success
        | ' ' -> SuccessSTR (BatString.trim (BatString.of_enum e))
        | '0' .. '9' -> SuccessID (get_nb e)
        | _ -> raise Protocol_error)
  | '?' ->
      let e = drop_one e
      in
      (match peek e with
        | ' ' -> Failure ""
        | '0' .. '9' -> FailureFull ((get_nb e), (slurp_enum e))
        | _ -> raise Protocol_error)
  | '0' .. '9' -> Formatter.set_id (get_nb e); Command (parse_cmd e)
  | _ -> Command (parse_cmd e)

(** parse la ligne **)
let parse_line str =
  let str = preprocess str
  in (decode_line str)
