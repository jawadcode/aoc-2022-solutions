open Batteries

type shape = Rock | Paper | Scissors

let shape_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> assert false

let play (opp, you) =
  (match you with Rock -> 1 | Paper -> 2 | Scissors -> 3)
  +
  match (opp, you) with
  | Scissors, Rock | Paper, Scissors | Rock, Paper -> 6
  | Rock, Scissors | Scissors, Paper | Paper, Rock -> 0
  | _, _ -> 3

let part_one contents =
  let round = String.split_on_char '\n' contents in
  List.map (Tuple2.mapn shape_of_string % String.split ~by:" ") round
  |> List.map play |> List.sum
  |> Printf.printf "Your score: %d\n"

let arg = try Some (Array.get Sys.argv 1) with Invalid_argument _ -> None

let () = let contents = File.with_file_in "input.txt" IO.read_all in
match arg with
| Some ("one"|"1") -> part_one contents
(* | Some ("two"|"2") -> part_two contents *)
| _ -> print_endline "Usage: day_2_rock_paper_scissors <part>"