(** Parser implemente le protocole GTP **)
open Batteries_uni
  
type t_action =
  | Command of string
  | CommandFull of (int * string)
  | Success
  | SuccessFull of (int * string)
  | SuccessID of int
  | SuccessSTR of string
  | FailureFull of (int * string)
  | Failure of string

exception Invalid_end
  
exception Protocol_error
  
(** retire les chars invalides de l'input, a savoir:
- les chars de controle
- les lignes blanches
- les commentaires
**)
let preprocess str =
  let rec parse e last_is_space accu =
    match Enum.get e with
    | None -> accu
    | (* TODO: gestion d'erreur ? *) Some c ->
        let n = Char.code c
        in
          (match n with
           | 10 | 35 -> accu
           | (* fin de ligne ou comment*) 9 | 32 ->
               if not last_is_space
               then (* espacements *)
                 parse e true (Common.enum_push accu ' ')
               else parse e true accu
           | x when (x < 32) || (x = 127) -> parse e true accu
           | (* char de controle *) _ ->
               parse e false (Common.enum_push accu c))
  in
    (* char normal *)
    ((parse (String.enum str) true (Enum.empty ())) |> String.
       of_backwards)
      |> String.enum
  
(** extrait le type de communication et son id si specifie **)
let decode_line e =
  let rec get_id e id =
    match Enum.get e with
    | None -> raise Invalid_end
    | Some c ->
        (match c with
         | ('0' .. '9' as n) -> get_id e ((id * 10) + (Char.code n))
         | ' ' | '\n' -> id
         | _ -> raise Invalid_end)

  and get_str e = (BatString.of_enum e) |> BatString.trim
  and drop_one e = BatEnum.drop 1 e;e
  and peek e =
    match BatEnum.peek e with | None -> raise Protocol_error | Some c -> c
  in
    match peek e with
    | '=' ->
        let e = drop_one e
        in
          (match peek e with
           | '\n' -> Success
           | ' ' -> SuccessSTR (get_str e)
           | '0' .. '9' -> SuccessID (get_id e 0)
           | _ -> raise Protocol_error)
    | '?' ->
        let e = drop_one e
        in
          (match peek e with
           | ' ' -> Failure ""
           | '0' .. '9' -> FailureFull ((get_id e 0), (get_str e))
           | _ -> raise Protocol_error)
    | '0' .. '9' -> CommandFull ((get_id e 0), (get_str e))
    | _ -> Command (get_str e)
  
(** parse la ligne **)
let parse_line str =
  let str = preprocess str
  in
    match decode_line str with
    | Success -> ()
    | SuccessID id -> ()
    | Command cmd -> ()
    | CommandFull (id, cmd) -> ()
    | SuccessFull (id, resp) -> ()
    | SuccessSTR resp -> ()
    | FailureFull (id, resp) -> ()
    | Failure resp -> ()
  
