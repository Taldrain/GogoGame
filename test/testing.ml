(**
Tests de regression effectues a chaque build

@see http://ounit.forge.ocamlcore.org/api/
**)

(* Objectif : *)
(*  - Deux suites de tests : *)
(*      * tests rapides de regression, a chaque build*)
(*      * tests longs et plus intensifs, sur des parties du code *)
(*  - Tester beaucoup, mais pas tout non plus *)

open OUnit

let simple_test () = assert_equal 1 1

let suite = "Dummy Suite" >:::
[
  "simple_test" >:: simple_test
]

let _ =
  run_test_tt_main suite

