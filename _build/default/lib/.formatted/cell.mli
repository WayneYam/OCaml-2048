type t =
  | Empty
  | Value of int

(** [equal x y] test if [x] and [y] are equal*)
val equal : t -> t -> bool

(** [string of x] returns a text representation of [x] *)
val string_of : t -> string

(** [int_option_of x] converts a [cell] into an [int option] *)
val int_option_of : t -> int option

(** [int_option_of x] converts an [int option] into a [cell] *)
val of_int_option : int option -> t

(** [get_random ()] returns a cell according to the rules of 2048 *)
val get_random : unit -> t
