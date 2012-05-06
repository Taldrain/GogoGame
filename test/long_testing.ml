(**
Les tests, les vrais, ceux des hommes, des vrais
**)

open OUnit

let simple_test () = assert_equal 1 1

let suite = "Dummy Suite" >:::
[
  "simple_test" >:: simple_test
]

let _ =
  run_test_tt_main suite

