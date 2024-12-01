type input_t = int list * int list
type output_t = int

(** Helper functions *)
let rec pair_of_lists_maker lst =
  match lst with
  | [] -> ([], [])
  | _ :: [] -> failwith "Odd number of elements"
  | h1 :: h2 :: t ->
      let t1, t2 = pair_of_lists_maker t in
      (h1 :: t1, h2 :: t2)

let rec diff_between_lists = function
  | [], [] -> 0
  | h1 :: t1, h2 :: t2 -> abs (h1 - h2) + diff_between_lists (t1, t2)
  | _ -> failwith "Lists are of different sizes"

let rec find_n_occurrences lst x =
  match lst with
  | [] -> 0
  | h :: t ->
      if x = h then 1 + find_n_occurrences t x else find_n_occurrences t x

let rec mul_lists lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1 * h2) :: mul_lists t1 t2
  | _ -> failwith "Lists are of different sizes"

(** Solver functions *)
let parse_input (s : string) : input_t =
  s
  |> Str.split (Str.regexp "[ \n]+")
  |> List.map int_of_string |> pair_of_lists_maker

let solve1 (inp : input_t) : output_t =
  let l1 = List.sort Int.compare (fst inp) in
  let l2 = List.sort Int.compare (snd inp) in
  diff_between_lists (l1, l2)

let solve2 (inp : input_t) : output_t =
  let l1, l2 = inp in
  List.map (find_n_occurrences l2) l1 |> mul_lists l1 |> List.fold_left ( + ) 0

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions*)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
