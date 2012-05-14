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
  
  let color_of_blk_wht blk wht s =
    if BatBitSet.is_set blk s then Black
    else if BatBitSet.is_set wht s then White
    else Empty
end

module Vertex =
struct
  type t = { letter : Char.t; nb : BatInt.t; pass : BatBool.t }
  
  exception Vertex_letter_error of string
  
  exception Vertex_number_error of char
  
  let v ?(p = false) ~n ~l = { pass = p; nb = n; letter = l }
  
  let string_of_vertex { letter = l; nb = n; pass = b } =
    if b then "pass" else (Char.escaped l) ^ (string_of_int (n))
  
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
  
  exception Int_of_pass
  let int_of_vertex bsize v =
    (assert (v.nb - 1 >= 0));
    (assert (v.nb - 1 <= 12));
    if v.pass then raise Int_of_pass else (bsize * (int_of_letter v.letter)) + (v.nb - 1)
  
  let int_of_v = (int_of_vertex 13)
  
  let vertex_of_int bsize i =
    let (q, r) = div_eucl i bsize
    in
    try { pass = false; letter = letter_of_int q; nb = r +1; }
    with
    | Invalid_alphabet_of_int ->
        raise (Vertex_letter_error (string_of_int q))
  
  let vertex_of_id = (vertex_of_int 13)
  
  let vertex_is_a_pass { nb = _; letter = _; pass = p } = p
  
  let decomp {pass = p; nb= n; letter = l} = (l,n,p)
  
  let dist v1 v2 =
    let (l1,n1,p1) = decomp v1 and (l2,n2,p2) = decomp v2 in
    if p1 || p2 then max_int
    else
      (abs ((int_of_letter l2) - (int_of_letter l1))) + (abs (n2-n1))
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
  
  (* let move_of_string_plus str = let { color = c; vert = { Vertex.letter *)
  (* = l; Vertex.nb = n; Vertex.pass = p } } = move_of_string str in {     *)
  (* color = c; vert = { Vertex.letter = l; Vertex.nb = n - 1; Vertex.pass *)
  (* = p; }; }                                                             *)
  
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

type gameState = | Win | Lose
let invert_gameStatus = function | Win -> Lose | Lose -> Win
