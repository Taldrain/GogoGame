(**
    Implemente les differentes entites qui sont utilisees pour GTP
**)
open Batteries_uni

module Color =
struct
  type t = | White | Black
  
  exception Color_invalid_color
  
  let color_of_string str =
  match str.[0] with
    | 'w' | 'W' -> White
    | 'b' | 'B' -> Black
    | _ -> raise Color_invalid_color
  
  let string_of_color = function | White -> "White" | Black -> "Black"
  
  let str_of_col = function | White -> "W" | Black -> "B"
  
end

module Vertex =
struct
  type t = { letter : Char.t; nb : BatInt.t; pass : BatBool.t }
  
  exception Vertex_letter_error
  exception Vertex_number_error
  
  let string_of_vertex { letter = l; nb = n; pass = b } =
    if b then "pass" else (Char.escaped l) ^ (string_of_int n)
  
  let vertex_of_string str =
    let rec parse e i =
      match BatEnum.get e with
      | None -> i
      | Some c ->
          (match c with
            | '0' .. '9' -> parse e ((i * 10) + (Common.int_of_char c))
            | _ -> raise Vertex_number_error)
    in
    match Char.uppercase str.[0] with
    | 'I' -> raise Vertex_letter_error
    | ('A' .. 'Z' as c) ->
        { letter = c; pass = false; nb = parse (BatString.enum str) 0; }
    | _ -> raise Vertex_letter_error
  
  let is_valid boardSize { letter = l; nb = n; pass = b } =
    b ||
    ((n > 0) &&
      ((n < boardSize) &&
        (Common.int_of_letter l)< boardSize))
  
end

module Move = struct
  type t = { color: Color.t; vert: Vertex.t }
  
  let string_of_move m =
    (Color.string_of_color m.color)^" "^(Vertex.string_of_vertex m.vert)
  
  let move_of_string str =
    let (col, vert) = BatString.split str " " in
    {
      color = (Color.color_of_string col);
      vert = (Vertex.vertex_of_string vert)
    }
end

type entity =
  | Bool of BatBool.t
  | Int of BatInt.t
  | Float of BatFloat.t
  | String of String.t
  | Vertex of Vertex.t
  | Color of Color.t
  | Move of Move.t
