(**
TA TA TAAAAAM le test des groupes
**)
open OUnit
open Printf
open Entities
open Entities.Move
open Entities.Vertex
open Entities.Color
open Group_again
open Board
open Globals
open Group
open BatPervasives

let test_count ~expected =
  let count = !Group_again.idx_ref_groups
  in
  assert_bool
    (sprintf "nombre de groupes detectes incorrect (count a %d, attendu %d)" count expected)
    (count = expected)

let test_monoids ~vertices =
  let vertices = List.map (int_of_vertex 13) vertices
  in
  assert_bool "groupes mal detecte (pierres non trouvable dans la liste)"
    (List.for_all (fun m -> (Group_again.group_of_stone m) <> (Group_again.group_zero () ) ) vertices)

let are_in_same_group ~color ~vertices =
  let head = List.hd vertices in
  let g = ref ((int_of_v |- Group_again.group_of_stone) head) in
  List.iter
    (fun m ->
          let s = int_of_v m in
          let find = Group_again.group_of_stone s
          in
          assert_bool
            (Printf.sprintf
                "{%s} est dans le groupe zero" (string_of_vertex m))
            (find <> Group_again.group_zero ());
          assert_bool
            (Printf.sprintf "{%s} n'est pas dans le groupe de {%s}"
                (string_of_vertex m) (string_of_vertex head))
            (!g = find))
    vertices

let stupid_monoid () = (* setup *)
  (Board_init.self_init ();
    let v = { pass = false; nb = 7; letter = 'D'; } in
    let m = { color = Black; vert = v; }
    in
    (* tests *)
    (Engine.play m; test_count ~expected: 1; test_monoids ~vertices: [ v ]))

let multiples_monoids () = (* setup *)
  (Board_init.self_init ();
    let v1 = { pass = false; nb = 7; letter = 'D'; }
    
    and v2 = { pass = false; nb = 4; letter = 'K'; }
    and v3 = { pass = false; nb = 8; letter = 'J'; }
    and v4 = { pass = false; nb = 5; letter = 'F'; }
    and v5 = { pass = false; nb = 7; letter = 'F'; }
    
    and v6 = { pass = false; nb = 7; letter = 'H'; } in
    let m1 = { color = Black; vert = v1; }
    
    and m2 = { color = White; vert = v2; }
    and m3 = { color = Black; vert = v3; }
    and m4 = { color = White; vert = v4; }
    and m5 = { color = Black; vert = v5; }
    
    and m6 = { color = White; vert = v6; } in
    let l = [ v1; v2; v3; v4; v5; v6 ]
    in
    (* tests *)
    (Engine.play m1;
      Engine.play m2;
      Engine.play m3;
      Engine.play m4;
      Engine.play m5;
      Engine.play m6;
      test_count ~expected: 6;
      test_monoids ~vertices: l))

let large_multiple_monoids () =
  let count = ref 1 in
  let next_id id = id + 2 in
  let fill_board () =
    let my_id = ref 0 in
    (Engine.play { color = Black; vert = vertex_of_id !my_id; });
    (while !my_id < 167 do
        my_id := next_id !my_id;
        (Engine.play { color = Black; vert = vertex_of_id !my_id; });
        incr count done)
  in
  (* setup *)
  (Board_init.self_init ();
    fill_board ();
    (* tests *)
    test_count ~expected: !count)

let simple_allongement () = (* setup *)
  (Board_init.self_init ();
    let v1 = { pass = false; nb = 7; letter = 'F'; }
    and v2 = { pass = false; nb = 7; letter = 'G'; }
    in
    (* tests *)
    (Engine.play { color = Black; vert = v1; };
      Engine.play { color = Black; vert = v2; };
      test_count ~expected: 1;
      are_in_same_group ~color: Black ~vertices: [ v1; v2 ]))

let zigzag_allongement () = (* setup *)
  (Board_init.self_init ();
    let v1 = { pass = false; nb = 7; letter = 'D'; }
    
    and v2 = { pass = false; nb = 8; letter = 'D'; }
    and v3 = { pass = false; nb = 8; letter = 'E'; }
    and v4 = { pass = false; nb = 9; letter = 'E'; }
    
    and v5 = { pass = false; nb = 9; letter = 'F'; }
    in
    (* tests *)
    (Engine.play { color = Black; vert = v1; };
    Engine.play { color = Black; vert = v2; };
    Engine.play { color = Black; vert = v3; };
    Engine.play { color = Black; vert = v4; };
    Engine.play { color = Black; vert = v5; };
      (*Playing.play_v ~vertices: [ v1; v2; v3; v4; v5 ];*)
      test_count ~expected: 1;
      are_in_same_group ~color: Black ~vertices: [ v1; v2; v3; v4; v5 ]))

let reverse_allongement () = (* setup *)
  (Board_init.self_init ();
    let v1 = { pass = false; nb = 7; letter = 'D'; }
    
    and v2 = { pass = false; nb = 8; letter = 'D'; }
    
    and v3 = { pass = false; nb = 6; letter = 'D'; }
    in
    (* tests *)
    (Engine.play { color = Black; vert = v1; };
    Engine.play { color = Black; vert = v2; };
    Engine.play { color = Black; vert = v3; };
    (* (Playing.play_v ~vertices: [ v1; v2; v3 ];*)
      test_count ~expected: 1;
      are_in_same_group ~color: Black ~vertices: [ v1; v2; v3 ]))

let test_fusion () = todo "not yet"

let suite () =
  "groupes" >::: [ "groupes monoides" >::: [ "stupides" >::
  stupid_monoid; "multiples" >:: multiples_monoids; "large" >::
  large_multiple_monoids ];
  "allongement de groupes" >:::
  [ "simple" >:: simple_allongement;"zigzag" >:: zigzag_allongement;
  "renversement" >:: reverse_allongement ; "fusion de deux groupes"
  >:: test_fusion]]
