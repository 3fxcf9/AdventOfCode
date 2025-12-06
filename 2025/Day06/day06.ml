let read_lines filename =
  let f = open_in filename in
  let rec read () =
    try
      let next = input_line f in
      next :: read ()
    with End_of_file ->
      close_in f;
      []
  in
  read ()

let split_on_whitespace (parse_function : string -> 'a) (line : string) :
    'a list =
  line |> String.split_on_char ' '
  |> List.filter_map (fun s -> if s = "" then None else Some (parse_function s))

let parse_op_list (lines : string list) : ((int -> int -> int) * int) list =
  lines |> List.rev |> List.hd
  |> split_on_whitespace (fun s -> if s = "+" then (( + ), 0) else (( * ), 1))

(* If part_2 in true, operators are applied line by line, and if part_2 is false, column by column. Then the results are summed *)
let rec compute (part_2 : bool) (operators : ((int -> int -> int) * int) list)
    (numbers : int list list) : int =
  match operators with
  | [] -> 0
  | (op, default) :: tl ->
      let r =
        compute part_2 tl
          (if part_2 then List.tl numbers else List.map List.tl numbers)
      in
      r
      + ((if part_2 then List.hd numbers else List.map List.hd numbers)
        |> List.fold_left op default)

(* part_1 specific *)
let parse_int_matrix (lines : string list) : int list list =
  lines |> List.rev |> List.tl |> List.rev
  |> List.map (split_on_whitespace int_of_string)

(* part 2 specific *)
let ( << ) f g x = f (g x)

let parse_char_matrix (lines : string list) : char list list =
  lines |> List.rev |> List.tl |> List.rev
  |> List.map (List.of_seq << String.to_seq)

let int_of_char = Fun.flip ( - ) (Char.code '0') << Char.code
let hd_or_space = function [] -> ' ' | hd :: _ -> hd
let tl_or_empty = function [] -> [] | _ :: tl -> tl

(* Returns a list of chunks, each containing the numbers parsed in column *)
let rec chunks (current_chunk : int list) (matrix : char list list) :
    int list list =
  if List.for_all (Fun.flip ( = ) []) matrix then [ current_chunk ]
  else
    let column = List.map hd_or_space matrix in
    let rest = List.map tl_or_empty matrix in
    if List.for_all (Fun.flip ( = ) ' ') column then
      current_chunk :: chunks [] rest
    else
      Fun.flip chunks rest
      @@ (column
         |> List.filter_map (fun c -> if c = ' ' then None else Some c)
         |> List.to_seq |> String.of_seq |> int_of_string)
         :: current_chunk

(* display *)
let filename = "input"
let operator_list = read_lines filename |> parse_op_list

let part_1 =
  read_lines filename |> parse_int_matrix |> compute false operator_list

let part_2 =
  read_lines filename |> parse_char_matrix |> chunks []
  |> compute true operator_list
;;

Printf.printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
