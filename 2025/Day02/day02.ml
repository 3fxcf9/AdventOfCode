let read_line filename =
  let f = open_in filename in
  let line = input_line f in
  close_in f;
  line

let get_ranges (input : string) : int list list =
  input |> String.split_on_char ','
  |> List.map (fun r -> String.split_on_char '-' r |> List.map int_of_string)

let rec test_cut (digit_count : int) (len : int) (n : int) : bool =
  let mask =
    List.flatten
    @@ List.init (digit_count / len) (fun _ -> List.init len (fun x -> x))
  in
  let reg = Array.make len None in
  let rec test (m : int) (mask : int list) : bool =
    match mask with
    | [] -> true
    | hd :: tl -> (
        match reg.(hd) with
        | None ->
            reg.(hd) <- Some (m mod 10);
            test (m / 10) tl
        | Some d -> if d = m mod 10 then test (m / 10) tl else false)
  in
  test n mask

let if_invalid_2 (n : int) : int =
  let digit_count = int_of_float @@ (floor (log10 @@ float_of_int n) +. 1.) in
  let rec loop len =
    if len > digit_count - 1 then 0
    else if digit_count mod len <> 0 then loop (len + 1)
    else if test_cut digit_count len n then n
    else loop (len + 1)
  in
  loop 1

let if_invalid_1 (n : int) : int =
  let digit_count = int_of_float @@ (floor (log10 @@ float_of_int n) +. 1.) in
  if digit_count mod 2 = 0 && test_cut digit_count (digit_count / 2) n then n
  else 0

let sum_in_range (filter_function : int -> int) (range : int list) : int =
  let a, b = (List.hd range, List.hd (List.tl range)) in
  List.init (b - a + 1) (( + ) a)
  |> List.map filter_function |> List.fold_left ( + ) 0

let part_1 =
  read_line "input" |> get_ranges
  |> List.map (sum_in_range if_invalid_1)
  |> List.fold_left ( + ) 0

let part_2 =
  read_line "input" |> get_ranges
  |> List.map (sum_in_range if_invalid_2)
  |> List.fold_left ( + ) 0
;;

Printf.printf "Part 1: %d\nPart 2: %d" part_1 part_2
