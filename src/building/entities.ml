(**
Implemente les differentes entites qui sont utilisees pour GTP
**)
open Common

module Color =
struct
  type t = | White | Black | Empty
  
  exception Color_invalid_color
  
  let color_of_string str =
    if BatString.is_empty str
    then invalid_arg "Entities.Vertex.color_of_string: string empty !"
    else
      match str.[0] with
      | 'w' | 'W' -> White
      | 'b' | 'B' -> Black
      | _ -> raise Color_invalid_color
  
  let string_of_color =
    function
    | White -> "white"
    | Black -> "black"
    | Empty -> raise Color_invalid_color
  
  let str_of_col =
    function
    | White -> "W"
    | Black -> "B"
    | Empty -> raise Color_invalid_color

  let invert_color = function
    | White -> Black
    | Black -> White
    | Empty -> Empty
end

module Vertex =
struct
  type t = { letter : Char.t; nb : BatInt.t; pass : BatBool.t }
  
  exception Vertex_letter_error of string
  
  exception Vertex_number_error of char
  
  let v ?(p=false) ~n ~l = { pass=p; nb=n; letter=l }
  
  let string_of_vertex { letter = l; nb = n; pass = b } =
    if b then "pass" else (Char.escaped l) ^ (string_of_int (n + 1))
  
  let vertex_of_string str =
    if BatString.is_empty str
    then failwith "Entities.Vertex.vertex_of_string: string empty !"
    else
      let rec parse e i =
        (match BatEnum.get e with
          | None -> i
          | Some c ->
              (match c with
                | '0' .. '9' -> parse e ((i * 10) + (Common.int_of_char c))
                | c -> raise (Vertex_number_error c)))
      in
      match Char.uppercase str.[0] with
      | 'P' -> { letter ='A'; nb =1; pass = true }
      | 'I' -> raise (Vertex_letter_error "I")
      | ('A' .. 'Z' as c) ->
          { letter = c; pass = false; nb = parse (Common.drop_one (BatString.enum str)) 0; }
      | c -> raise (Vertex_letter_error (Char.escaped c))
  
  let is_valid boardSize { letter = l; nb = n; pass = b } =
    b ||
    ((n > 0) &&
      ((n < boardSize) && ((Common.int_of_letter l) < boardSize)))
  
  let int_of_vertex bsize v =
    if v.pass then (-1) else (bsize * (int_of_letter v.letter)) + v.nb
  
  let int_of_v = (int_of_vertex 13)
  
  let vertex_of_int bsize i =
    let (q, r) = div_eucl i bsize
    in
    try { pass = false; letter = letter_of_int q; nb = r; }
    with
    | Invalid_alphabet_of_int ->
        raise (Vertex_letter_error (string_of_int q))
  
  let vertex_is_a_pass { nb = _; letter = _; pass = p } = p
  
end

module Move =
struct
  type t = { color : Color.t; vert : Vertex.t }
  
  let string_of_move m =
    (Color.string_of_color m.color) ^
    (" " ^ (Vertex.string_of_vertex m.vert))
  
  let str_of_m m =
    (Color.str_of_col m.color) ^ (" " ^ (Vertex.string_of_vertex m.vert))
  
  let move_of_string str =
    let (col, vert) = BatString.split str " "
    in
    {
      color = Color.color_of_string col;
      vert = Vertex.vertex_of_string vert;
    }
  
  let move_is_a_pass { color = _; vert = v } = Vertex.vertex_is_a_pass v
  
  (* let move_of_string_plus str = let { color = c; vert = { Vertex.letter = *)
  (* l; Vertex.nb = n; Vertex.pass = p } } = move_of_string str in { color = *)
  (* c; vert = { Vertex.letter = l; Vertex.nb = n - 1; Vertex.pass = p; }; } *)
  
end

type color = Color.t

type vertex = Vertex.t

type move = Move.t

type entity =
  | Bool of BatBool.t
  | Int of BatInt.t
  | Float of BatFloat.t
  | String of String.t
  | Vertex of Vertex.t
  | Color of Color.t
  | Move of Move.t
