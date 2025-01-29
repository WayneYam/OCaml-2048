type t

(** [equal x y] test if [x] and [y] are equal*)
val equal : t -> t -> bool

(** [string of x] returns a text representation of [x] *)
val string_of : t -> string

(** [init x] initializes a board of size [x] *)
val init : int -> t

(** [main ()] runs the game *)
val main : unit -> unit
