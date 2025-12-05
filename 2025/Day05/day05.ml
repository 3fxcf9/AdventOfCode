let read_lines filename =
  let f = open_in filename in
  let rec read () =
    try
      let next = input_line f in
      let a, b = read () in
      if next = "" then (b, a) else (next :: a, b)
    with End_of_file ->
      close_in f;
      ([], [])
  in
  read ()

let parse_input ((ranges_str, id_str) : string list * string list) :
    (int * int) list * int list =
  ( ranges_str
    |> List.map (fun r ->
           let l = String.split_on_char '-' r in
           (int_of_string (List.hd l), int_of_string @@ List.hd (List.tl l))),
    id_str |> List.map int_of_string )

let rec test_id id = function
  | [] -> false
  | (a, b) :: tl -> (a <= id && id <= b) || test_id id tl

let rec count_fresh_ids (ranges, ids) : int =
  match ids with
  | [] -> 0
  | id :: tl -> Bool.to_int (test_id id ranges) + count_fresh_ids (ranges, tl)

let fresh_id = read_lines "input" |> parse_input |> count_fresh_ids

(* part 2 *)

let rec try_merge (ranges : (int * int) list) ((a, b) : int * int) =
  match ranges with
  | [] -> (a, b) :: []
  | (x, y) :: tl -> (
      match (a < x, a <= y, b < x, b <= y) with
      | false, _, _, true -> (x, y) :: tl (* inside (large) *)
      | true, _, _, false -> try_merge tl (a, b) (* outside (strict) *)
      | true, _, false, true -> try_merge tl (a, y) (* left (strict) *)
      | false, true, _, false -> try_merge tl (x, b) (* right (strict) *)
      | _ -> (x, y) :: try_merge tl (a, b))

let fresh_range =
  read_lines "input" |> parse_input |> fst
  |> List.fold_left (fun acc cur -> try_merge acc cur) []
  |> List.fold_left (fun acc (a, b) -> acc + b - a + 1) 0
;;

Printf.printf "Part 1: %d\nPart 2: %d\n" fresh_id fresh_range
