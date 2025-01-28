open! Base

type t =
  | Empty
  | Value of int

let equal x y =
  match x, y with
  | Empty, Empty -> true
  | Value x, Value y -> x = y
  | Empty, Value _ | Value _, Empty -> false
;;

let string_of cel =
  match cel with
  | Empty -> "_"
  | Value x -> Int.to_string x
;;

let int_option_of x =
  match x with
  | Empty -> None
  | Value x -> Some x
;;

let of_int_option x =
  match x with
  | None -> Empty
  | Some x -> Value x
;;

let get_random () = if Random.int 9 = 0 then Value 4 else Value 2
