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

let get_roll_positions (input : string list) =
  input
  |> List.mapi (fun row line ->
         String.to_seq line
         |> Seq.mapi (fun col char ->
                if char = '@' then (row, col) else (-1, -1))
         |> Seq.filter (fun (r, c) -> r > -1 && c > -1)
         |> List.of_seq)
  |> List.flatten

let rec get_accessible (positions : (int * int, unit) Hashtbl.t)
    (remain : (int * int) list) =
  match remain with
  | [] -> []
  | (row, col) :: tl ->
      let offsets =
        [ (-1, 1); (0, 1); (1, 1); (-1, 0); (1, 0); (-1, -1); (0, -1); (1, -1) ]
      in
      let rec count_free = function
        | [] -> 0
        | (dr, dc) :: tl ->
            Bool.to_int (Hashtbl.mem positions (row + dr, col + dc))
            + count_free tl
      in
      if count_free offsets < 4 then (row, col) :: get_accessible positions tl
      else get_accessible positions tl

let rec solve (part2 : bool) (positions : (int * int) list) =
  let table = Hashtbl.create 100 in
  List.iter (fun key -> Hashtbl.add table key ()) positions;
  match get_accessible table positions with
  | [] -> 0
  | removed ->
      List.iter (fun c -> Hashtbl.remove table c) removed;
      List.length removed
      +
      if part2 then
        Hashtbl.fold (fun k _ acc -> k :: acc) table [] |> solve true
      else 0

let positions = read_lines "input" |> get_roll_positions;;

Printf.printf "Part 1: %d\nPart 2: %d\n" (solve false positions)
  (solve true positions)
