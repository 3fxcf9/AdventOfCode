(* Index of the start and a list of lines, each being a list of the splitter indexes *)
let read_lines filename =
  let f = open_in filename in
  let rec read () =
    try
      let line =
        input_line f |> String.to_seq
        |> Seq.mapi (fun col char ->
               if char = '^' || char = 'S' then col else -1)
        |> Seq.filter (fun r -> r > -1)
        |> List.of_seq
      in
      if line = [] then read () else line :: read ()
    with End_of_file ->
      close_in f;
      []
  in
  let ((start_index :: _) :: tl) = read () in
  (start_index, tl)

(* Part 1 *)

let rec split_rays_mult (splitters : int list) (rays : (int * int) list) :
    (int * int) list =
  match rays with
  | [] -> []
  | (ray, mult) :: tl ->
      (if List.mem ray splitters then [ (ray - 1, mult); (ray + 1, mult) ]
       else [ (ray, mult) ])
      @ split_rays_mult splitters tl

let sum_mult (rays : (int * int) list) =
  List.fold_left (fun acc (_, m) -> acc + m) 0 rays

let rec solve (splitters : int list list) (rays : (int * int) list) i :
    int * int =
  match splitters with
  | [] -> (0, 0)
  | splitter :: tl ->
      let splitted = split_rays_mult splitter rays in
      let new_rays =
        List.fold_left
          (fun acc (r, m) ->
            match List.assoc_opt r acc with
            | None -> (r, m) :: acc
            | Some mult ->
                (r, m + mult) :: List.filter (fun (k, _) -> k <> r) acc)
          [] splitted
      in
      let new_splits = List.length splitted - List.length rays in
      let new_timelines = sum_mult splitted - sum_mult rays in
      let split_count, timeline_count = solve tl new_rays (i + 1) in
      (split_count + new_splits, timeline_count + new_timelines)

let initial, splitters = read_lines "input"
let part_1, part_2_minus_1 = solve splitters [ (initial, 1) ] 1;;

Printf.printf "Part 1: %d\nPart 2: %d\n" part_1 (part_2_minus_1 + 1)
