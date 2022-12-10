open Batteries

let priority ch =
  if Char.is_lowercase ch then Char.code ch - Char.code 'a' + 1
  else Char.code ch - Char.code 'A' + 27

let rec part_one contents =
  get_rucksacks contents
  |> List.map (fun group ->
         let first, second = Tuple2.mapn (Set.of_seq % String.to_seq) group in
         let result = Set.intersect first second |> Set.map priority in
         Set.fold ( + ) result 0)
  |> List.sum |> Printf.printf "Sum: %d\n"

and get_rucksacks contents =
  String.split_on_char '\n' contents
  |> List.map (fun str ->
         let mid = Int.div (String.length str) 2 in
         (String.sub str 0 mid, String.sub str mid mid))

let rec part_two contents =
  String.split_on_char '\n' contents
  |> group_three
  |> List.map (fun group ->
         let first, second, third =
           Tuple3.mapn (Set.of_seq % String.to_seq) group
         in
         Set.intersect first second |> Set.intersect third |> Set.any)
  |> List.map priority |> List.sum |> Printf.printf "Sum: %d\n"

and group_three rucksacks =
  match rucksacks with
  | first :: second :: third :: rest ->
      List.cons (first, second, third) (group_three rest)
  | [] -> []
  | _ -> assert false

let arg = try Some Sys.argv.(1) with Invalid_argument _ -> None

let () =
  let contents = File.with_file_in "input.txt" IO.read_all in
  match arg with
  | Some ("one" | "1") -> part_one contents
  | Some ("two" | "2") -> part_two contents
  | _ -> print_endline "Usage: day_3_rucksack_reorganization <part>"