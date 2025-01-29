open! Base

type t = int option [@@deriving eq]

let string_of = function
  | None -> "_"
  | Some x -> Int.to_string x
;;

let int_option_of x =
  match x with
  | None -> None
  | Some x -> Some x
;;

let of_int x = Some x
let empty = None
let get_random () = if Random.int 9 = 0 then Some 4 else Some 2
