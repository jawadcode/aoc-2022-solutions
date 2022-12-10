open Batteries

let part_one pairs =
  List.fold
    (fun acc ((a_start, a_end), (b_start, b_end)) ->
      if
        (a_start <= b_start && a_end >= b_end)
        || (b_start <= a_start && b_end >= a_end)
      then acc + 1
      else acc)
    0 pairs
  |> Printf.printf "Pairs: %d\n"

let part_two pairs =
  List.fold
    (fun acc ((a_start, a_end), (b_start, b_end)) ->
      if a_end < b_start || b_end < a_start then acc else acc + 1)
    0 pairs
  |> Printf.printf "Pairs: %d\n"

let get_pairs contents =
  String.split_on_char '\n' contents
  |> List.map (String.split ~by:",")
  |> List.map
       (Tuple2.mapn (fun sect ->
            Tuple2.mapn Int.of_string (String.split ~by:"-" sect)))

let () =
  let pairs = File.with_file_in "input.txt" IO.read_all |> get_pairs in
  let arg = try Some Sys.argv.(1) with Invalid_argument _ -> None in
  match arg with
  | Some ("one" | "1") -> part_one pairs
  | Some ("two" | "2") -> part_two pairs
  | _ -> print_endline "Usage: day_4_camp_cleanup <part>"