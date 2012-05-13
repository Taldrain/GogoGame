
open OUnit
open Entities
open Entities.Color
open Entities.Vertex
open Entities.Move

let color_test () =
  let c1 = Black and c2 = White
  in
  List.iter (fun c -> "HUGE conversion prob (color)" @?
          (color_of_string (string_of_color c) = c)) [c1; c2]

let vertex_test1 () =
  let v1 = { pass = true; nb =1; letter ='A'}
  and v2 = { pass = false; nb =5; letter ='C'}
  and v3 = { pass = false; nb =11; letter ='K'}
  in
  List.iter (fun v ->
          let sv = string_of_vertex v and v2 = vertex_of_string (string_of_vertex v) in
          let sv2 = string_of_vertex (v2)
          in
          assert_bool
            (Printf.sprintf "HUGE conversion prob (vertex_string: %s <> %s)" sv sv2)
            (v2 = v)) [v1; v2; v3]

let vertex_test2 () =
  let v2 = { pass = false; nb =5; letter ='C'}
  and v3 = { pass = false; nb =11; letter ='K'}
  in
  List.iter (fun v ->
          let id1 = int_of_v v and sv = string_of_vertex v in
          let v2 = vertex_of_id id1 and sv2 = string_of_vertex v in
          let id2 = int_of_v v2 in
          assert_bool
            (Printf.sprintf "HUGE conversion prob (vertex_id: %s/%d <> %s/%d)" sv id1 sv2 id2)
            (v2 = v)) [v2; v3]

let move_test () =
  let m1 = { color = Black; vert ={ pass = true; nb =1; letter ='A'}}
  and m2 = { color = White; vert ={ pass = false; nb =5; letter ='C'}}
  and m3 = { color = Black; vert ={ pass = false; nb =11; letter ='K'}}
  in
  List.iter (fun m ->
          let sm1 = string_of_move m in
          let m2 = move_of_string sm1 in
          let sm2 = string_of_move m2 in
          assert_bool
            (Printf.sprintf "HUGE conversion prob (move: %s <> %s)" sm1 sm2)
            (m2 = m)) [m1; m2; m3]

let suite () =
  "Entities" >:::
  [
  "Color" >:: color_test;
  "Vertex_string" >:: vertex_test1;
  "Vertex_id" >:: vertex_test2;
  "Move" >:: move_test
  ]