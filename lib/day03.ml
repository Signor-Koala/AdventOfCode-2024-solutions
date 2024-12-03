type input_t = string
type output_t = int

(** Helper functions *)
let rec get_all_matches_aux regexp i s =
  match Str.search_forward regexp s i with
  | i ->
      let res = Str.matched_string s in
      res :: get_all_matches_aux regexp (i + String.length res) s
  | exception Not_found -> []

let get_all_matches re s =
  let regexp = Str.regexp re in
  get_all_matches_aux regexp 0 s

let contains ss s =
  let re = Str.regexp_string ss in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found -> false

(** Solver functions *)
let parse_input (s : string) : input_t = s

let solve1 (inp : input_t) : output_t =
  inp
  |> get_all_matches {|mul([0-9]+,[0-9]+)|}
  |> List.map (get_all_matches {|[0-9]+|})
  |> List.map (fun l -> List.map int_of_string l |> List.fold_left ( * ) 1)
  |> List.fold_left ( + ) 0

let solve2 (inp : input_t) : output_t =
  inp |> Str.split (Str.regexp {|don't()|}) |> fun l ->
  List.hd l :: List.filter (contains {|do()|}) (List.tl l) |> fun l ->
  List.hd l
  :: List.map
       (fun x ->
         let i = Str.search_forward (Str.regexp {|do()|}) x 0 in
         Str.string_after x i)
       (List.tl l)
  |> String.concat " " |> solve1

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
