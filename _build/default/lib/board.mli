type t

val equal : t -> t -> bool
(** [equal x y] test if [x] and [y] are equal*)

val string_of : t -> string
(** [string of x] returns a text representation of [x] *)

val init : int -> t
(** [init x] initializes a board of size [x] *)

val main : unit -> unit
(** [main ()] runs the game *)
