(*
 * Keep Testing,
 * For Science !
 *
 * -- 
 * GLaDOS
 *)




open Connecting


(* Parameter *)
let nbrCycle = 500
let nbrOutput = 1
let nbrInterLayout = 0
let nbrNodePerLayout = 0
let bias = 0(**)

(* Input *)
let biTest = BatBitSet.empty()




let nbrInput = BatBitSet.clone biTest
let nbrP = nbrInput + nbr0utput + nbrInterLayout * nbrNodePerLayout


let gLaDOS = (new assuming_control nbrInput
                nbrOutput
                nbrInterLayout
                nbrNodePerLayout
                bias
                nbrP
                nbrCycle)

gLaDOS#read
gLaDOS#complete

let goodW = gLaDOS#gBestW

