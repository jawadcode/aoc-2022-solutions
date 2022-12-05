open Batteries

let get_rucksacks contents =
  String.split_on_char '\n' contents
  |> List.map (fun str ->
         let mid = Int.div (String.length str) 2 in
         (String.sub str 0 mid, String.sub str mid mid))

let rec part_one contents =
  get_rucksacks contents
  |> List.map (fun (first, second) ->
         let first = String.to_seq first |> Set.of_seq in
         let second = String.to_seq second |> Set.of_seq in
         let result = Set.intersect first second |> Set.map priority in
         Set.fold ( + ) result 0)
  |> List.sum |> Printf.printf "Sum: %d\n"

and priority ch =
  if Char.is_lowercase ch then Char.code ch - Char.code 'a' + 1
  else Char.code ch - Char.code 'A' + 27

let part_two _ = ()
let arg = try Some Sys.argv.(1) with Invalid_argument _ -> None

let () =
  let contents = File.with_file_in "input.txt" IO.read_all in
  match arg with
  | Some ("one" | "1") -> part_one contents
  | Some ("two" | "2") -> part_two contents
  | _ -> print_endline "Usage: day_3_rucksack_reorganization <part>"