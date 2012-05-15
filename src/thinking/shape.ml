open Common

let rec fold_left f a = function
  | [] -> a
  | h::t -> fold_left f (f a h) t

let rec are_set bs = function
    | [] -> true
    | x::l -> (bitSet_is_set bs x) && are_set bs l

let shapes : (int -> int array) array =
[|
 (*lion*)
  (fun x -> [| (x+2); (x+16)  |]) ;
  (fun x -> [| (x+10); (x+12) |] );
  (fun x -> [| (x+2); (x+11)  |] );
  (fun x -> [| (x+12); (x+38) |] );
  (fun x -> [| (x+26); (x+38) |] );
  (fun x -> [| (x+14); (x+16) |] );
  (*bamboo*)
  (fun x -> [| (x+1); (x+13); (x+14) |]);
  (fun x -> [| (x+2); (x+13); (x+15) |]);
(*big bulge*)
  (fun x -> [| (x+10); (x+25) |]);
  (fun x -> [| (x+25); (x+40) |]);
  (fun x -> [| (x+15); (x+40) |]);
  (fun x -> [| (x+15); (x+25) |]);
  (fun x -> [| (x+11); (x+27) |]);
  (fun x -> [| (x+27); (x+38) |]);
  (fun x -> [| (x+11); (x+38) |]);
  (fun x -> [| (x+16); (x+27) |]);
(*botsugi*)
  (fun x -> [| (x+13); (x+26) |]);
  (fun x -> [| (x+1); (x+2)   |]);
(*dango*)
  (fun x -> [| (x+13); (x+1); (x+14) |]);
(*diamond*)
  (fun x -> [| (x+12); (x+14); (x+26) |]);
(*tiger's mouth*)
  (fun x -> [| (x+12); (x+26) |]);
  (fun x -> [| (x+14); (x+26) |]);
  (fun x -> [| (x+12); (x+14) |]);
  (fun x -> [| (x+2); (x+14)  |]);
(*honeycomb*)
  (fun x -> [| (x+14); (x+26); (x+28) |]);
  (fun x -> [| (x+2); (x+14); (x+26)  |]);
  (fun x -> [| (x+2); (x+14); (x+28)  |]);
  (fun x -> [| (x+12); (x+24); (x+26) |]);
(*(*lantern procession*)                        *)
(*(  fun x -> [| (x+3); (x+6); (x+9)          |];*)
(*(  fun x -> [| (x+3*13); (x+6*13); (x+9*13) |];*)
(*parallelogram*)
  (fun x -> [| (x+2); (x+25); (x+27)  |]);
  (fun x -> [| (x+2); (x+27); (x+29)  |]);
  (fun x -> [| (x+15); (x+26); (x+41) |]);
  (fun x -> [| (x+11); (x+26); (x+37) |]);
(*table*)
  (fun x -> [| (x+15); (x+26); (x+28) |]);
  (fun x -> [| (x+2); (x+15); (x+26) |]);
  (fun x -> [| (x+2); (x+26); (x+27) |]);
  (fun x -> [| (x+2); (x+27); (x+28) |]);
  (fun x -> [| (x+1); (x+25); (x+27) |]);
  (fun x -> [| (x+1); (x+26); (x+28) |]);
  (fun x -> [| (x+2); (x+13); (x+28) |]);
  (fun x -> [| (x+11); (x+24); (x+26) |]);
(*triangle vide*)
  (fun x -> [| (x+1); (x+13) |]);
  (fun x -> [| (x+13); (x+14) |]);
  (fun x -> [| (x+12); (x+13) |]);
  (fun x -> [| (x+1); (x+14) |]);
(*throwing star*)
  (fun x -> [| (x+11); (x+27); (x+38) |]);
(*tiny table*)
  (fun x -> [| (x+16); (x+27); (x+29) |]);
  (fun x -> [| (x+1); (x+27); (x+38) |]);
  (fun x -> [| (x+11); (x+37); (x+38) |]);
  (fun x -> [| (x+2); (x+13); (x+29) |]);
 |]
