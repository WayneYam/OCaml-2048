open! Base
open! Stdio

type t =
  { size : int
  ; data : Cell.t list list
  }

let equal x y =
  if x.size = y.size
  then List.for_all2_exn ~f:(List.equal Cell.equal) x.data y.data
  else false
;;

let string_of_row row =
  List.map ~f:(fun x -> Cell.string_of x |> String.pad_left ~len:5) row |> String.concat
;;

let string_of { data; _ } =
  List.map ~f:(fun x -> String.append (string_of_row x) "\n") data |> String.concat
;;

let init size =
  { size; data = List.init size ~f:(fun _ -> List.init size ~f:(fun _ -> Cell.Empty)) }
;;

let rotate_board { size; data } =
  let split_first_column grid =
    List.map ~f:(fun l -> List.hd_exn l, List.tl_exn l) grid |> List.unzip
  in
  let rec aux acc grid =
    match grid with
    | [] -> assert false
    | hd :: _ ->
      (match hd with
       | [] -> acc
       | _ ->
         let first_col, rest = split_first_column grid in
         aux (first_col :: acc) rest)
  in
  { size; data = aux [] data }
;;

let compress_row size lst =
  let rec aux acc (buf : Cell.t) (lst : Cell.t list) =
    match lst with
    | [] ->
      (match buf with
       | Cell.Empty -> acc
       | _ -> buf :: acc)
    | hd :: tl ->
      (match hd with
       | Empty -> aux acc buf tl
       | Value x ->
         (match buf with
          | Empty -> aux acc hd tl
          | Value y ->
            if x = y then aux (Value (x * 2) :: acc) Empty tl else aux (buf :: acc) hd tl))
  in
  let rec pad size lst : Cell.t list =
    if size = 0
    then []
    else (
      match lst with
      | [] -> Empty :: pad (size - 1) []
      | hd :: tl -> hd :: pad (size - 1) tl)
  in
  aux [] Empty lst |> List.rev |> pad size
;;

let get_rotate_count (dir : Input.direction) =
  match dir with
  | Left -> 0
  | Up -> 1
  | Right -> 2
  | Down -> 3
;;

let move dir board =
  let rec rotate num board =
    if num = 0 then board else rotate (num - 1) (rotate_board board)
  in
  let compress_board { size; data } =
    { size; data = List.map ~f:(compress_row size) data }
  in
  let count = get_rotate_count dir in
  rotate count board |> compress_board |> rotate (4 - count)
;;

let has_won board =
  List.exists ~f:(fun x -> List.exists ~f:(Cell.equal (Cell.Value 2048)) x) board.data
;;

let has_lost board =
  let is_row_stuck row =
    let rec aux last row =
      match row with
      | [] -> true
      | hd :: tl ->
        if Cell.equal hd last || Cell.equal hd Cell.Empty then false else aux hd tl
    in
    aux Cell.Empty row
  in
  let rotated_board = rotate_board board in
  List.for_all ~f:is_row_stuck board.data
  && List.for_all ~f:is_row_stuck rotated_board.data
;;

let has_free_space board =
  List.exists ~f:(List.exists ~f:(Cell.equal Cell.Empty)) board.data
;;

let get_free_spaces board =
  let get_free_space_row =
    List.filter_mapi ~f:(fun id cel ->
      if Cell.equal cel Cell.Empty then Some id else None)
  in
  List.concat_mapi
    ~f:(fun id row -> get_free_space_row row |> List.map ~f:(fun x -> id, x))
    board.data
;;

let get_random_free_space board =
  let free_spaces = get_free_spaces board in
  let random_index = Random.int_incl 0 @@ (List.length free_spaces - 1) in
  List.nth_exn free_spaces random_index
;;

let set_space cell (x, y) board =
  { board with
    data =
      (let set_pos cell row =
         List.mapi ~f:(fun idx original -> if idx = y then cell else original) row
       in
       List.mapi
         ~f:(fun idx original -> if idx = x then set_pos cell original else original)
         board.data)
  }
;;

let generate_new_tile board =
  if has_free_space board
  then set_space (Cell.get_random ()) (get_random_free_space board) board
  else board
;;

let main () =
  let rec game_loop board =
    string_of board |> print_endline;
    if has_won board
    then print_endline "You Have Won!"
    else if has_lost board
    then print_endline "Try again..."
    else (
      let dir = Input.get_dir () in
      let next_board = move dir board in
      if equal board next_board
      then game_loop board
      else generate_new_tile next_board |> game_loop)
  in
  init 4 |> generate_new_tile |> generate_new_tile |> game_loop
;;
