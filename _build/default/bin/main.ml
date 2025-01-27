open Game_2048
open! Base
open! Stdio

let run_game () =
  let rec game_loop board =
    Board.board_to_string board |> print_endline;
    if Board.check_has_won board then print_endline "You Have Won!"
    else if Board.check_has_lost board then print_endline "Try again..."
    else
      let dir = Input.get_dir () in
      let next_board = Board.move dir board in
      if Board.board_equal board next_board then game_loop board
      else Board.generate_new_tile next_board |> game_loop
  in
  Board.initialize_board 4 |> Board.generate_new_tile |> Board.generate_new_tile
  |> game_loop
;;

run_game ()
