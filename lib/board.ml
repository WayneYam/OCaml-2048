open! Base
open! Stdio

type cell = Empty | Value of int
type board = { size : int; data : cell list list }

let cell_equal x y =
  match (x, y) with
  | Empty, Empty -> true
  | Value x, Value y -> x = y
  | _ -> false

let board_equal x y =
  if x.size = y.size then
    List.for_all2_exn ~f:(List.equal cell_equal) x.data y.data
  else false

let cell_to_string cel =
  match cel with Empty -> "_" | Value x -> Int.to_string x

let row_to_string row =
  List.map ~f:(fun x -> cell_to_string x |> String.pad_left ~len:5) row
  |> String.concat

let board_to_string { data; _ } =
  List.map ~f:(fun x -> String.append (row_to_string x) "\n") data
  |> String.concat

let initialize_board size =
  {
    size;
    data = List.init size ~f:(fun _ -> List.init size ~f:(fun _ -> Empty));
  }

let rotate_board { size; data } =
  let split_first_column grid =
    List.map ~f:(fun l -> (List.hd_exn l, List.tl_exn l)) grid |> List.unzip
  in
  let rec aux acc grid =
    match grid with
    | [] -> assert false
    | hd :: _ -> (
        match hd with
        | [] -> acc
        | _ ->
            let first_col, rest = split_first_column grid in
            aux (first_col :: acc) rest)
  in
  { size; data = aux [] data }

let compress_row size lst =
  let rec aux acc buf lst =
    match lst with
    | [] -> ( match buf with Empty -> acc | _ -> buf :: acc)
    | hd :: tl -> (
        match hd with
        | Empty -> aux acc buf tl
        | Value x -> (
            match buf with
            | Empty -> aux acc hd tl
            | Value y ->
                if x = y then aux (Value (x * 2) :: acc) Empty tl
                else aux (buf :: acc) hd tl))
  in
  let rec pad size lst =
    if size = 0 then []
    else
      match lst with
      | [] -> Empty :: pad (size - 1) []
      | hd :: tl -> hd :: pad (size - 1) tl
  in
  aux [] Empty lst |> List.rev |> pad size

type direction = Up | Left | Down | Right

let get_rotate_number dir =
  match dir with Left -> 0 | Up -> 1 | Right -> 2 | Down -> 3

let move dir board =
  let rec rotate num board =
    if num = 0 then board else rotate (num - 1) (rotate_board board)
  in
  let compress_board { size; data } =
    { size; data = List.map ~f:(compress_row size) data }
  in
  let count = get_rotate_number dir in
  rotate count board |> compress_board |> rotate (4 - count)

let check_has_won board =
  List.exists
    ~f:(fun x -> List.exists ~f:(cell_equal (Value 2048)) x)
    board.data

let check_has_lost board =
  let is_row_stuck row =
    let rec aux last row =
      match row with
      | [] -> true
      | hd :: tl ->
          if cell_equal hd last || cell_equal hd Empty then false else aux hd tl
    in
    aux Empty row
  in
  let rotated_board = rotate_board board in
  List.for_all ~f:is_row_stuck board.data
  && List.for_all ~f:is_row_stuck rotated_board.data

let check_has_free_space board =
  List.exists ~f:(List.exists ~f:(cell_equal Empty)) board.data

let get_all_free_space board =
  let get_free_space_row =
    List.filter_mapi ~f:(fun id cel ->
        if cell_equal cel Empty then Some id else None)
  in
  List.concat_mapi
    ~f:(fun id row -> get_free_space_row row |> List.map ~f:(fun x -> (id, x)))
    board.data

let get_random_free_space board =
  let free_spaces = get_all_free_space board in
  let random_index = Random.int_incl 0 @@ (List.length free_spaces - 1) in
  List.nth_exn free_spaces random_index

let get_random_cell () = if Random.int 9 = 0 then Value 4 else Value 2

let set_space cell (x, y) board =
  {
    board with
    data =
      (let set_pos cell row =
         List.mapi
           ~f:(fun idx original -> if idx = y then cell else original)
           row
       in
       List.mapi
         ~f:(fun idx original ->
           if idx = x then set_pos cell original else original)
         board.data);
  }

let generate_new_tile board =
  if check_has_free_space board then
    set_space (get_random_cell ()) (get_random_free_space board) board
  else board
