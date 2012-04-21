open Batteries

module Color =
struct
  type t = White | Black
  
  exception Color_invalid_color
  
  let color_of_string = function
    | "w" | "W" | "white" | "White" -> White
    | "b" | "B" | "black" | "Black" -> Black
    | _ -> raise Color_invalid_color
  
  let string_of_color = function
    | White -> "White"
    | Black -> "Black"
  
  let str_of_col = function
    | White -> "W"
    | Black -> "B"
end

module Vertex =
struct
  type t = {
    letter: BatChar.t;
    nb: BatInt.t;
    pass: BatBool.t
  }
  
  exception Vertex_letter_error
  exception Vertex_number_error
  
  let string_of_vertex {letter=l;nb=n;pass=b} =
    if b then "pass"
    else (Char.escaped l)^(string_of_int n)

  let vertex_of_string str =
    let rec parse e i =
      match BatEnum.get e with
        | None -> i
        | Some c ->
          match c with
            | '0'..'9' -> parse e (i*10+(Common.int_of_char c))
            | _ -> raise Vertex_number_error
    in
    match Char.uppercase str.(0) with
      | 'I' -> raise Vertex_letter_error
      | 'A'..'Z' as c -> { letter=c; pass=false; nb=(parse (BatString.enum str) 0)}
      | _ -> raise Vertex_letter_error
  
  let is_valid boardSize {letter=l;nb=n;pass=b} =
    b || (n > 0 && n < boardSize && true)
end
module Move =
struct
  type t
end

type entity =
  | Bool of BatBool.t
  | Int of BatInt.t
  | Float of BatFloat.t
  | String of String.t
  | Vertex of Vertex.t
  | Color of Color.t
  | Move of Move.t
