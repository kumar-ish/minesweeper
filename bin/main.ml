open! Core
open! Minesweeper_multi.Board

let () =
  let b = create { width = 15; height = 15; mines = 10 } in
  let _b = click b { Coordinates.x = 5; y = 5 } Click_type.Flag in
  print_endline @@ For_testing.surrounding_mines_to_string b;
  print_endline @@ For_testing.mine_to_string b
;;
