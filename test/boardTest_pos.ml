(**
De petits tests concernant la classe board
**)
open BatPervasives
open Common
open OUnit

open Entities
open Entities.Move
open Entities.Vertex
open Entities.Color

let default_size = 13
let size_square = 13*13

let r = BatRandom.self_init ()

let fill_board i =
  let b = new Board.board default_size in
  let next_col =
    function
    | Entities.Color.Black -> Entities.Color.White
    | _ -> Entities.Color.Black
  and random_move col =
    {
      Entities.Move.color = col;
      Entities.Move.vert =
        {
          Entities.Vertex.letter = letter_of_int (BatRandom.int default_size);
          Entities.Vertex.nb = BatRandom.int default_size;
          Entities.Vertex.pass = false
        };
    } in
  let rec fill i col =
    if i = 0
    then b
    else (b#place_stone (random_move col); fill (i - 1) (next_col col))
  in fill i Entities.Color.Black

let test_fill_board () = (fill_board size_square)#clear

let empty_board b = (b#clear; b#is_clear)

let get_all b =
  let dummy_ref = ref (Board.Corner (0, Entities.Color.Empty, (0, 0)))
  in
  (for i = 0 to (size_square - 1) do dummy_ref := b#get i done;
    b)

let print list =
  let rec print_stone accu = function
    | [] -> accu
    | e::[] -> accu^(string_of_int e)^"]"
    | e::l -> print_stone (accu^(string_of_int e)^", ") l
  in
  print_stone "[" list

let test_eq ~msg ~stone ~expected =
  assert_equal (Board.get_neighbours Globals.board#get stone) expected
    ~msg:("voisins de "^msg^" differents") ~printer:print

let test_neighbours () =
  (* setup *)
  Board_init.self_init ();
  let middle = { pass = false; nb = 7; letter = 'F' }
  and border = { pass = false; nb = 1; letter = 'F' }
  and corner = { pass = false; nb = 1; letter = 'A' }

  and m1 = { pass = false; nb = 6; letter = 'F' }
  and m2 = { pass = false; nb = 8; letter = 'F' }
  and m3 = { pass = false; nb = 7; letter = 'E' }
  and m4 = { pass = false; nb = 7; letter = 'G' }

  and b1 = { pass = false; nb = 1; letter = 'E' }
  and b2 = { pass = false; nb = 1; letter = 'G' }
  and b3 = { pass = false; nb = 2; letter = 'F' }

  and c1 = { pass = false; nb = 2; letter = 'A' }
  and c2 = { pass = false; nb = 1; letter = 'B' }
  in
  let lst = Array.map int_of_v [|middle;border;corner|]
  and m_neigh = List.map int_of_v [m3;m2;m1;m4]
  and b_neigh = List.map int_of_v [b1;b3;b2]
  and c_neigh = List.map int_of_v [c1;c2]
  in
  (* test *)
  skip_if true "test incomprehensible";

  test_eq ~msg:"milieu" ~stone:lst.(0) ~expected:m_neigh;
  test_eq ~msg:"bordure" ~stone:lst.(1) ~expected:b_neigh;
  test_eq ~msg:"coin" ~stone:lst.(2) ~expected:c_neigh

let suite () =
  "Board class" >:::
  [
    "remplissage" >:: test_fill_board;
    "get_neighbour" >:: test_neighbours;
    "vider" >:: (fun () ->
        assert_bool "vider"
        (bracket (fun () -> fill_board 30) empty_board (fun b -> b#is_clear) ()));
    "get_all" >:: (fun () ->
        assert_bool "get_all"
        (bracket (fun () -> fill_board size_square) get_all (fun b -> true) ()));
  ]
