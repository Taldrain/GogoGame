(**
TA TA TAAAAAM le test des groupes
**)

open OUnit
open Printf
open Entities
open Entities.Move
open Entities.Vertex
open Entities.Color
open Group

let test_count ~expected =
  assert_bool (sprintf "nombre de groupes detectes incorrect (count a %d)" count)
    (count = expected)

let test_monoids ~vertices =
  assert_bool "groupes mal detecte (pierres non trouvable dans la liste)"
    (List.for_all (fun m -> Group_manager.find m <> None) vertices)

let are_in_same_group ~color ~vertices =
  let g = ref (new group color) and set = ref false in
  assert_bool "groupes mal fusionnes (pierres ne sont pas dans le meme groupe)"
    (List.for_all
    (fun m -> let find = Group_manager.find m in
        find <> None && (if !set then (g := find; true) else !g = find)))

let stupid_monoid () =
  (* setup *)
  Board_init.self_init ();
  let v = { pass = false; nb =7; letter ='D' } in
  let m = { color = Black; vert = v } in
  Engine.play m;
  (* tests *)
  test_count ~expected:1;
  test_monoids ~vertices:[m]

let multiples_monoids () =
  (* setup *)
  Board_init.self_init ();
  let v1 = { pass = false; nb =7; letter ='D' } and v2 = { pass = false; nb =4; letter ='K'}
  and v3 = { pass = false; nb =8; letter ='J' } and v4 = { pass = false; nb =5; letter ='F'}
  and v5 = { pass = false; nb =7; letter ='F' } and v6 = { pass = false; nb =7; letter ='H'}
  in
  let m1 = { color = Black; vert = v1 } and m2 = { color = White; vert = v2 }
  and m3 = { color = Black; vert = v3 } and m4 = { color = White; vert = v4 }
  and m5 = { color = Black; vert = v5 } and m6 = { color = White; vert = v6 }
  in
  let l = [ v1; v2; v3; v4; v5; v6 ]
  in
  Engine.play m1; Engine.play m2; Engine.play m3; Engine.play m4;
  Engine.play m5; Engine.play m6;
  (* tests *)
  test_count ~expected:6;
  test_monoids ~vertices: l

let large_multiple_monoids () =
  let count = ref 0 in
  let rec generate_vertices { pass = _; nb = n; letter = l } =
    match l with
      | 'N' -> if n <> 1 then { pass = false; nb = n -1; letter ='B'} else { pass = true; nb =1; letter ='A'}
      | 'M' -> { pass = false; nb = n -1; letter ='A'}
      | c -> { pass = false; nb = n; letter = (Char.chr (2 + (Char.code c))) }
  in
  let fill_board () =
    let v = ref { pass = false; nb =1; letter ='A' } and count = ref 0 in
    Engine.play { color = Black; vert = !v };
    while (not !v.pass) do
      v := generate_vertices !v
      Engine.play { color = Black; vert = !v };
      incr count
    done
  in
  (* setup *)
  Board_init.self_init ();
  fill_board ();
  (* tests *)
  test_count ~expected:!count

let simple_allongement () =
  (* setup *)
  Board_init.self_init ();
  let v1 = { pass = false; nb =7; letter ='F'} and v2 = { pass = false; nb =7; letter ='G'}
  in
  Engine.play { color = Black; vert = v1 };
  Engine.play { color = Black; vert = v2 };
  (* tests *)
  test_count ~expected:1;
  are_in_same_group ~vertices:[v1; v2]

let zigzag_allongement () =
  (* setup *)
  Board_init.self_init ();
  let v1 = { pass = false; nb =7; letter ='D'} and v2 = { pass = false; nb =8; letter ='D'}
  and v3 = { pass = false; nb =8; letter ='E'} and v4 = { pass = false; nb =9; letter ='E'}
  and v5 = { pass = false; nb =9; letter ='F'}
  in
  Playing.play_v ~vertices:[v1; v2; v3; v4; v5];
  (* tests *)
  test_count ~expected:1;
  are_in_same_group ~vertices:[v1; v2; v3; v4; v5]

let reverse_allongement () =
    (* setup *)
  Board_init.self_init ();
  let v1 = { pass = false; nb =7; letter ='D'} and v2 = { pass = false; nb =8; letter ='D'}
  and v3 = { pass = false; nb =6; letter ='D'}
  in
  Playing.play_v ~vertices:[v1; v2; v3]
  (* tests *)
  test_count ~expected:1;
  are_in_same_group ~vertices:[v1; v2; v3]

let test_fusion () = todo "not yet"

let suite () =
  "groupes" >:::
    [ "groupes monoides" >::: [ stupid_monoid; multiples_monoids; large_multiple_monoids ];
      "allongement de groupes" >::: [ simple_allongement; zigzag_allongement; reverse_allongement ]
      "fusion de deux groupes" >:: test_fusion ]

