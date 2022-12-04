open Batteries

let get_scores contents =
  String.split_on_char '\n' contents
  |> List.map (fun str ->
         ( Char.code (String.get str 0) - Char.code 'A',
           Char.code (String.get str 2) - Char.code 'X' ))

let part_one scores =
  List.map (fun (opp, you) -> ((you - opp + 4) mod 3 * 3) + you + 1) scores
  |> List.sum
  |> Printf.printf "Your score: %d\n"

and part_two scores =
  List.map (fun (opp, you) -> ((you + opp + 2) mod 3) + (3 * you) + 1) scores
  |> List.sum
  |> Printf.printf "Total score: %d\n"

let arg = try Some (Array.get Sys.argv 1) with Invalid_argument _ -> None

let () =
  let scores = File.with_file_in "input.txt" IO.read_all |> get_scores in
  match arg with
  | Some ("one" | "1") -> part_one scores
  | Some ("two" | "2") -> part_two scores
  | _ -> print_endline "Usage: day_2_rock_paper_scissors <part>"