type t = Empty | Value of int

val equal : t -> t -> bool
(** [equal x y] test if [x] and [y] are equal*)

val string_of : t -> string
val int_option_of : t -> int option
val of_int_option : int option -> t
val get_random : unit -> t
