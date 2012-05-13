(**
Gere l'allocation du temps
**)

open Unix

let period = ref 9.5
let set_timer i = period := i

let get_timer () = (ITIMER_REAL, { it_interval = !period; it_value = !period })

let run () =
  let (which,t) = get_timer () in
  Unix.setitimer which t
