(*
 * Keep Testing,
 * For Science !
 *
 * -- 
 * GLaDOS
 *)


open Connecting


(* Parameter *)
let nbrCycle = 1
let nbrOutput = 1
let nbrInterLayout = 0
let nbrNodePerLayout = 0
let bias = 0(**)


let filename = "/home/taldrain/projets/GogoGame/test/testInput" 
let nbrInput = 168 (*base 0*)


(* Input *)
(*let biTest = BatBitSet.empty()*)




(*let nbrInput = BatBitSet.clone biTest*)
let nbrP = nbrInput + nbrOutput + nbrInterLayout * nbrNodePerLayout
  
let foo = BatDynArray.create()

let gLaDOS = 
    (new assuming_control
                nbrInput
                nbrOutput
                nbrInterLayout
                nbrNodePerLayout
                bias
                nbrP
                nbrCycle
                foo)

let head =
  "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         The list of weight found with the best result is :\n" 
let tail =
  "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n"

let rec printW ar =
  let rec cer yar i s =
    if i = s then
      ()
    else
      (print_float(BatDynArray.get yar i);
       cer yar (i+1) s)
  in cer ar 0 (BatDynArray.length ar)


let _ =
  gLaDOS#read filename;
  gLaDOS#complete;
  print_string head;
  let goodW = gLaDOS#gBestW in
    printW goodW;
  print_string tail

