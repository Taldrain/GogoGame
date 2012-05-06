(**
Ce Module simule un coup
**)

open Board

let play board move =
  board#place_stone m;
  History.play m;
  AI.refresh_groups m
