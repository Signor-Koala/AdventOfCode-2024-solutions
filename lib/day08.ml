type pos = int * int

module Antennas = struct
  type t = char

  let compare = Char.compare
end

module AntennaMap = Map.Make (Antennas)

type input_t = pos list AntennaMap.t * pos
type output_t = int

module XY = struct
  type t = pos

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end

module XYSet = Set.Make (XY)

(** Helper functions *)
let rec make_map = function
  | [] -> AntennaMap.empty
  | (x, y, c) :: t -> AntennaMap.add_to_list c (x, y) (make_map t)

let rec make_pairs_aux = function
  | [] -> [ [] ]
  | h :: t ->
      let ll = make_pairs_aux t in
      List.map (fun l -> h :: l) ll @ ll

let make_pairs lst =
  make_pairs_aux lst
  |> List.filter (fun l -> List.length l = 2)
  |> List.map (fun l -> (List.hd l, List.hd (List.tl l)))

let is_in_bounds (boundx, boundy) (x, y) =
  x < boundx && x >= 0 && y < boundy && y >= 0

let antinodes bound ((x1, y1), (x2, y2)) =
  (if is_in_bounds bound ((2 * x2) - x1, (2 * y2) - y1) then
     [ ((2 * x2) - x1, (2 * y2) - y1) ]
   else [])
  @
  if is_in_bounds bound ((2 * x1) - x2, (2 * y1) - y2) then
    [ ((2 * x1) - x2, (2 * y1) - y2) ]
  else []

let rec find_antinodes bound = function
  | [] -> []
  | h :: t -> antinodes bound h @ find_antinodes bound t

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let get_interval (x1, y1) (x2, y2) =
  let dx, dy = (x2 - x1, y2 - y1) in
  let div = gcd (abs dx) (abs dy) in
  (dx / div, dy / div)

let rec all_points_on_line_aux bound (x, y) (dx, dy) =
  if is_in_bounds bound (x + dx, y + dy) then
    (x + dx, y + dy) :: all_points_on_line_aux bound (x + dx, y + dy) (dx, dy)
  else []

let all_points_on_line bound p (dx, dy) =
  (p :: all_points_on_line_aux bound p (dx, dy))
  @ all_points_on_line_aux bound p (-dx, -dy)

let rec find_antinodes2 bound = function
  | [] -> []
  | (p1, p2) :: t ->
      let intervals = get_interval p1 p2 in
      all_points_on_line bound p1 intervals @ find_antinodes2 bound t

(** Solver functions *)
let parse_input (s : string) : input_t =
  let lines = String.split_on_char '\n' s in
  ( lines
    |> List.map (fun s -> s |> String.to_seqi |> List.of_seq)
    |> List.mapi (fun x l ->
           l
           |> List.map (fun (y, c) -> (x, y, c))
           |> List.filter (fun (_, _, c) -> not (c = '.')))
    |> List.flatten |> make_map,
    (List.length lines - 1, List.nth lines 0 |> String.length) )

let solve1 (inp : input_t) : output_t =
  let antennas, bound = inp in
  AntennaMap.map make_pairs antennas
  |> AntennaMap.map (find_antinodes bound)
  |> AntennaMap.to_list
  |> List.map (fun (_, pos) -> pos)
  |> List.flatten |> XYSet.of_list |> XYSet.cardinal

let solve2 (inp : input_t) : output_t =
  let antennas, bound = inp in
  AntennaMap.map make_pairs antennas
  |> AntennaMap.map (find_antinodes2 bound)
  |> AntennaMap.to_list
  |> List.map (fun (_, pos) -> pos)
  |> List.flatten |> XYSet.of_list |> XYSet.cardinal

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
