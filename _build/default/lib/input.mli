type direction =
  | Up
  | Left
  | Down
  | Right

(** [get_dir ()] asks user which direction to move *)
val get_dir : unit -> direction
