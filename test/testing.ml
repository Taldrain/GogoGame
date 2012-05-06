(**
Tests de regression effectues a chaque build

@see http:// ounit.forge.ocamlcore.org / api /
**)

(* Objectif : - Deux suites de tests : * tests rapides de regression, a    *)
(* chaque build * tests longs et plus intensifs, sur des parties du code - *)
(* Tester beaucoup, mais pas tout non plus                                 *)

open OUnit

let main_suite = "main test suite" >:::
  [
  ParserTest_pos.suite ();
  BoardTest_pos.suite ()
  ]

let _ =
  run_test_tt_main main_suite

