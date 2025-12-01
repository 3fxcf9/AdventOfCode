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

let compute_offset (line : string) =
  match
    (line.[0], int_of_string @@ String.sub line 1 (String.length line - 1))
  with
  | 'L', n -> -n
  | 'R', n -> n
  | _ -> failwith "Invalid line"

let ( % ) a b = ((a mod b) + b) mod b

let _, part_1, part_2 =
  read_lines "input"
  |> List.fold_left
       (fun (acc_pos, acc_count_part_1, acc_count_part_2) curr ->
         let next_unreduced = acc_pos + compute_offset curr in
         let next = next_unreduced % 100 in
         let count =
           (abs next_unreduced / 100)
           + Bool.to_int (acc_pos * next_unreduced < 0 || next_unreduced = 0)
           (* Add one if crossed zero left to right from a non-zero and add one if landed exactly at 0 *)
         in
         ( next,
           acc_count_part_1 + Bool.to_int (next = 0),
           acc_count_part_2 + count ))
       (50, 0, 0)
;;

Printf.printf "Part 1: %d\nPart 2: %d" part_1 part_2
