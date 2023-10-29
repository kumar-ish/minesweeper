open! Core
open! Minesweeper_multi.Board

let () =
  let b = create { width = 10; height = 10; mines = 100 } in
  let _b = click b { Coordinates.x = 5; y = 5 } Click_type.Flag in
  print_endline @@ For_testing.status_to_string b
;;
