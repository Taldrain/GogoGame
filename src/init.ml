(*******************************************************************************
 * 
 *                             Gogo Game
 * 
 * 
 *                      Yet Another AI for the Go Game
 * 
 * 
 ******************************************************************************)

(** fonction d'exemple 1 **)
let f x = print_string "f is applied to "; print_int x; print_newline()
(** fonction d'exemple 2 **)
let rec fib n = if n < 2 then 1 else fib(n-1) + fib(n-2)
(** fonction d'exemple 3 **)
let format_result n = Printf.sprintf "Result is: %d\n" n

let _ =
  (* Ici on doit enregistrer toutes les fonctions qui seront appellees par le C
     Comme ca : *)
  Callback.register "Nom arbitraire" f;
  Callback.register "fib" fib;
  Callback.register "format_result" format_result;
  (* Allez dans caml_func.c et .h pour voir ce que donne l'appel d'une fonction
     caml depuis le C *)
