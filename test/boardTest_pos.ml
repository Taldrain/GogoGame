(**
De petits tests concernant la classe board
**)
open Common
open OUnit

let default_size = 13

let r = BatRandom.self_init ()

let fill_board i =
  let b = new Board.board default_size in
  let next_col =
    function
    | Entities.Color.Black -> Entities.Color.White
    | _ -> Entities.Color.Black
  and random_move col =
    {
      Entities.Move.color = col;
      Entities.Move.vert =
        {
          Entities.Vertex.letter = letter_of_int (BatRandom.int default_size);
          Entities.Vertex.nb = BatRandom.int default_size;
          Entities.Vertex.pass = false
        };
    } in
  let rec fill i col =
    if i = 0
    then b
    else (b#place_stone (random_move col); fill (i - 1) (next_col col))
  in fill i Entities.Color.Black

let test_fill_board () = (fill_board (default_size * default_size))#clear

let empty_board b = (b#clear; b#is_clear)

let get_all b =
  let dummy_ref = ref (Board.Corner (0, Entities.Color.Empty, (0, 0)))
  in
  (for i = 0 to (default_size * default_size - 1) do dummy_ref := b#get i done;
    b)

let suite () =
  "Board class" >:::
  [ "remplissage" >::
  (fun () -> "exception lors du remplissage" @? (test_fill_board (); true));
  "vider" >::
  (fun () -> "board n'a pas été vidée" @?
        (bracket (fun () -> fill_board 30) empty_board (fun b -> b#is_clear) ()));
  "get_all" >::
  (fun () -> "impossible d'acceder a la totalite du plateau" @?
        (bracket (fun () -> fill_board (default_size * default_size))
            get_all (fun b -> true) ())) ]
