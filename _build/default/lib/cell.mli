type t = Empty | Value of int

val equal : t -> t -> bool
(** [equal x y] test if [x] and [y] are equal*)

val string_of : t -> string
(** [string of x] returns a text representation of [x] *)

val int_option_of : t -> int option
(** [int_option_of x] converts a [cell] into an [int option] *)

val of_int_option : int option -> t
(** [int_option_of x] converts an [int option] into a [cell] *)

val get_random : unit -> t
(** [get_random ()] returns a cell according to the rules of 2048 *)
