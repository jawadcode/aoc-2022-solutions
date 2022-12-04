open Batteries

let rec part_one contents =
  let calories = get_calories contents in
  List.fold max 0 calories |> Printf.printf "First: %d\n"

and part_two contents =
  let calories = get_calories contents in
  let leaderboard =
    LazyList.of_list calories
    |> LazyList.sort ~cmp:(flip compare)
    |> LazyList.take 3 |> LazyList.to_array
  in
  match leaderboard with
  | [| first; second; third |] ->
      Printf.printf "First: %d\nSecond: %d\nThird: %d\nTotal: %d\n" first second
        third (Array.sum leaderboard)
  | _ -> assert false

and get_calories contents =
  let chunks = String.split_on_string ~by:"\n\n" contents in
  List.map
    (fun chunk ->
      String.split_on_char '\n' chunk |> List.map String.to_int |> List.sum)
    chunks

let task = try Some (Array.get Sys.argv 1) with Invalid_argument _ -> None

let () = 
  let contents = File.with_file_in "input.txt" IO.read_all
in match task with
| Some ("one" | "1") -> part_one contents
| Some ("two" | "2") -> part_two contents
| _ -> print_endline "Usage: day_1_calorie_counting <part>"