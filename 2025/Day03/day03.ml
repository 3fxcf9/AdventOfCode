let read_lines filename =
  let f = open_in filename in
  let rec read () =
    try
      let next =
        input_line f |> String.to_seq
        |> Seq.map (fun c -> Char.code c - Char.code '0')
        |> List.of_seq
      in
      next :: read ()
    with End_of_file ->
      close_in f;
      []
  in
  read ()

let generate_pairs (bank : int list) : (int * int) list =
  bank
  |> List.mapi (fun i x -> (List.length bank - 1 - i, x))
  |> List.sort (fun (_, a) (_, b) -> b - a)

let rec find_max (len : int) (before_index : int) (current_num : int)
    (bank : (int * int) list) : int =
  if len = 0 then current_num
  else
    let rec loop = function
      | [] -> (0, 0)
      | (i, x) :: tl when i < before_index && i >= len - 1 -> (i, x)
      | (i, x) :: tl -> loop tl
    in
    let i, x = loop bank in
    find_max (len - 1) (min before_index i) ((current_num * 10) + x) bank

let solve len =
  read_lines "input"
  |> List.fold_left
       (fun acc bank ->
         acc + find_max len (List.length bank) 0 (generate_pairs bank))
       0
;;

Printf.printf "Part 1: %d\nPart 2: %d\n" (solve 2) (solve 12)
