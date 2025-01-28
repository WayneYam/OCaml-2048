type direction = Up | Left | Down | Right

let rec get_dir () =
  Out_channel.output_string stdout {|Pick one of "wasd" to move:|};
  Out_channel.flush stdout;
  match In_channel.(input_char stdin) with
  | None -> assert false
  | Some 'w' -> Up
  | Some 'a' -> Left
  | Some 's' -> Down
  | Some 'd' -> Right
  | _ -> get_dir ()
