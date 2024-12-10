type input_t = int array array
type output_t = int
type pos = int * int

module XY = struct
  type t = pos

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end

module XYSet = Set.Make (XY)

(** Helper functions *)
let rec find_height_pos_aux i y = function
  | [] -> []
  | h :: t ->
      if h = i then y :: find_height_pos_aux i (y + 1) t
      else find_height_pos_aux i (y + 1) t

let rec find_height_pos i x = function
  | [] -> []
  | h :: t ->
      List.map (fun y -> (x, y)) (find_height_pos_aux i 0 h)
      @ find_height_pos i (x + 1) t

let check_point mat (boundx, boundy) (x, y) target =
  x >= 0 && x < boundx && y >= 0 && y < boundy && mat.(x).(y) = target

let traversable_points_aux mat bound visited (x, y) =
  let cur = mat.(x).(y) in
  let ans, visited =
    if
      check_point mat bound (x - 1, y) (cur + 1)
      && not (XYSet.mem (x - 1, y) visited)
    then ((x - 1, y) :: [], XYSet.add (x - 1, y) visited)
    else ([], visited)
  in
  let ans, visited =
    if
      check_point mat bound (x + 1, y) (cur + 1)
      && not (XYSet.mem (x + 1, y) visited)
    then ([ (x + 1, y) ] @ ans, XYSet.add (x + 1, y) visited)
    else (ans, visited)
  in
  let ans, visited =
    if
      check_point mat bound (x, y - 1) (cur + 1)
      && not (XYSet.mem (x, y - 1) visited)
    then ([ (x, y - 1) ] @ ans, XYSet.add (x, y - 1) visited)
    else (ans, visited)
  in
  if
    check_point mat bound (x, y + 1) (cur + 1)
    && not (XYSet.mem (x, y + 1) visited)
  then ([ (x, y + 1) ] @ ans, XYSet.add (x, y + 1) visited)
  else (ans, visited)

let rec traversable_points mat bound visited = function
  | [] -> ([], visited)
  | h :: t ->
      let new_points, visited = traversable_points_aux mat bound visited h in
      let ans, new_visited = traversable_points mat bound visited t in
      (new_points @ ans, new_visited)

let rec bfs mat bound visited prev_gen =
  let new_gen, visited = traversable_points mat bound visited prev_gen in
  if List.is_empty new_gen then visited else bfs mat bound visited new_gen

let count_score_aux mat bound pos =
  bfs mat bound (XYSet.empty |> XYSet.add pos) [ pos ]
  |> XYSet.filter (fun (x, y) -> mat.(x).(y) = 9)
  |> XYSet.cardinal

let count_score mat =
  let bound = (Array.length mat, Array.length mat.(0)) in
  List.map
    (count_score_aux mat bound)
    (find_height_pos 0 0
       (mat
       |> Array.map (fun arr -> arr |> Array.to_seq |> List.of_seq)
       |> Array.to_seq |> List.of_seq))
  |> List.fold_left ( + ) 0

let rec distinct_dfs mat bound visited (x, y) =
  if mat.(x).(y) = 9 then 1
  else
    let new_points, _ = traversable_points_aux mat bound visited (x, y) in
    if List.is_empty new_points then 0
    else
      List.map (distinct_dfs mat bound (visited |> XYSet.add (x, y))) new_points
      |> List.fold_left ( + ) 0

let count_rating mat =
  let bound = (Array.length mat, Array.length mat.(0)) in
  List.map
    (distinct_dfs mat bound XYSet.empty)
    (find_height_pos 0 0
       (mat
       |> Array.map (fun arr -> arr |> Array.to_seq |> List.of_seq)
       |> Array.to_seq |> List.of_seq))
  |> List.fold_left ( + ) 0

(** Solver functions *)
let parse_input (s : string) : input_t =
  s |> String.split_on_char '\n'
  |> List.map (fun s ->
         s |> String.to_seq
         |> Seq.map (fun c -> int_of_char c - 48)
         |> Array.of_seq)
  |> List.filter (fun a -> Array.length a > 0)
  |> List.to_seq |> Array.of_seq

let solve1 (inp : input_t) : output_t = inp |> count_score
let solve2 (inp : input_t) : output_t = inp |> count_rating
let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
