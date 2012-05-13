(**
Ce module a pour but d'initialiser le plateau de jeu a des etats donnes
**)

open Entities
open Entities.Color
open Entities.Vertex
open Entities.Move

let self_init () =
  Engine.set_boardsize 13;
  Engine.set_komi 6.5;
  ()

let classic_openning () =
  let s = 13 in
  let q = Common.letter_of_int (s -3) in
  [
  { color = Black; vert ={ pass = false; letter ='D'; nb = (s -3) }};
  { color = White; vert ={ pass = false; letter = q; nb = (s -3) }};
  { color = Black; vert ={ pass = false; letter ='D'; nb =4 }};
  { color = Black; vert ={ pass = false; letter = q; nb =4 }};
  ]

let init_std board =
  List.iter (Engine.play) (classic_openning ())