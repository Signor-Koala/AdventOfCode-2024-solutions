module CmpTable = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end

module CmpTableMap = Map.Make (CmpTable)

type input_t = int list list * int CmpTableMap.t
type output_t = int

(** Helper functions *)
let to_pairs s =
  match String.split_on_char '|' s with
  | [ h; t ] -> (int_of_string h, int_of_string t)
  | _ -> failwith "Not a pair"

let rec build_table = function
  | [] -> CmpTableMap.empty
  | (x, y) :: t ->
      CmpTableMap.add (x, y) (-1) (build_table t) |> CmpTableMap.add (y, x) 1

let page_compare tbl x y = CmpTableMap.find (x, y) tbl

(** Solver functions *)
let parse_input (s : string) : input_t =
  let lines_temp = String.split_on_char '\n' s in
  let lines = List.filter (fun s -> not (String.equal "" s)) lines_temp in
  let cmptbl, lists =
    List.partition (fun s -> String.contains_from s 0 '|') lines
  in
  ( lists
    |> List.map (fun s ->
           s |> String.split_on_char ',' |> List.map int_of_string),
    cmptbl |> List.map to_pairs |> build_table )

let solve1 (inp : input_t) : output_t =
  let lists, cmptbl = inp in
  let sorted_lists = List.map (List.sort (page_compare cmptbl)) lists in
  let correct_lists =
    List.map2
      (fun slst lst -> if List.equal Int.equal slst lst then slst else [])
      sorted_lists lists
    |> List.filter (fun l -> not (List.is_empty l))
  in
  List.map (fun l -> List.nth l (List.length l / 2)) correct_lists
  |> List.fold_left ( + ) 0

let solve2 (inp : input_t) : output_t =
  let lists, cmptbl = inp in
  let sorted_lists = List.map (List.sort (page_compare cmptbl)) lists in
  let corrected_lists =
    List.map2
      (fun slst lst -> if List.equal Int.equal slst lst then [] else slst)
      sorted_lists lists
    |> List.filter (fun l -> not (List.is_empty l))
  in
  List.map (fun l -> List.nth l (List.length l / 2)) corrected_lists
  |> List.fold_left ( + ) 0

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
