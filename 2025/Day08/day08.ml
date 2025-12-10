let read_lines filename =
  let f = open_in filename in
  let rec read () =
    try
      let (a :: b :: c :: _) = input_line f |> String.split_on_char ',' in
      (int_of_string a, int_of_string b, int_of_string c) :: read ()
    with End_of_file ->
      close_in f;
      []
  in
  read ()

let rec semi_product = function
  | [] -> []
  | a :: tl -> List.map (fun b -> (a, b)) tl @ semi_product tl

type point = int * int * int

let distance_squared (((a, b, c), (x, y, z)) : point * point) : int =
  let sq n = n * n in
  sq (x - a) + sq (y - b) + sq (z - c)

let rec are_connected (a : point) (b : point) components =
  match components with
  | [] -> false
  | hd :: tl -> (List.mem a hd && List.mem b hd) || are_connected a b tl

let rec merge (components : point list list) (points : point list) :
    point list list =
  match components with
  | [] -> points :: []
  | component :: tl ->
      let rec try_merge pts : point list list =
        match pts with
        | [] -> component :: merge tl points
        | p :: ps when List.mem p component ->
            merge tl (component @ List.filter (Fun.flip ( <> ) p) points)
        | p :: ps -> try_merge ps
      in
      try_merge points

(* Part 1 specific *)

let rec first n components segments =
  match segments with
  | [] -> components
  | _ when n <= 0 -> components
  | (a, b) :: tl when are_connected a b components ->
      first (n - 1) components
        tl (* do not connect 1000 pairs but check the 1000 first *)
  | (a, b) :: tl -> first (n - 1) (merge components [ a; b ]) tl

(* part 2 specific *)

let rec until_fully_connected box_count components segments =
  match segments with
  | [] -> failwith "error"
  | (a, b) :: tl when are_connected a b components ->
      until_fully_connected box_count components tl
  | (a, b) :: tl ->
      let new_components = merge components [ a; b ] in
      if
        List.length new_components > 0
        && List.length (List.hd new_components) = box_count
      then
        let x1, _, _ = a and x2, _, _ = b in
        x1 * x2
      else until_fully_connected box_count new_components tl

(* Display *)
let sorted_by_distance : (point * point) list =
  read_lines "input" |> semi_product
  |> List.filter_map (fun boxes ->
         match distance_squared boxes with 0 -> None | d -> Some (boxes, d))
  |> List.stable_sort (fun (_, d1) (_, d2) -> compare d1 d2)
  |> List.map (fun ((a, b), _) -> (a, b))

let part_1 =
  sorted_by_distance |> first 1000 []
  |> List.sort (fun c1 c2 -> compare (List.length c2) (List.length c1))
  |> List.mapi (fun i x -> (i, x))
  |> List.fold_left
       (fun acc (i, x) -> if i >= 3 then acc else acc * List.length x)
       1

let box_count =
  sorted_by_distance
  |> List.concat_map (fun (a, b) -> [ a; b ])
  |> List.fold_left
       (fun acc cur -> if List.mem cur acc then acc else cur :: acc)
       []
  |> List.length

let part_2 = sorted_by_distance |> until_fully_connected box_count [];;

Printf.printf "Part 1: %d\nPart 2: %d\n" part_1 part_2
