type input_t = char array array
type output_t = int
type pos = int * int

module XY = struct
  type t = pos

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end

module XYMap = Map.Make (XY)
module XYSet = Set.Make (XY)

(* Helper functions *)
let side_exist (x, y) (boundx, boundy) =
  x >= 0 && x < boundx && y >= 0 && y < boundy

let n_of_common_sides arr c (x, y) bound =
  (if side_exist (x - 1, y) bound && arr.(x - 1).(y) = c then 1 else 0)
  + (if side_exist (x + 1, y) bound && arr.(x + 1).(y) = c then 1 else 0)
  + (if side_exist (x, y - 1) bound && arr.(x).(y - 1) = c then 1 else 0)
  + if side_exist (x, y + 1) bound && arr.(x).(y + 1) = c then 1 else 0

let perimeter_grid arr bound =
  Array.mapi
    (fun x row ->
      Array.mapi (fun y c -> 4 - n_of_common_sides arr c (x, y) bound) row)
    arr

let rec get_perimeter_of_set per_arr = function
  | [] -> 0
  | (x, y) :: t -> per_arr.(x).(y) + get_perimeter_of_set per_arr t

let rec explore_connected c arr (x, y) bound acc =
  let acc = XYSet.add (x, y) acc in
  let acc =
    if
      side_exist (x - 1, y) bound
      && arr.(x - 1).(y) = c
      && not (XYSet.mem (x - 1, y) acc)
    then explore_connected c arr (x - 1, y) bound acc
    else acc
  in
  let acc =
    if
      side_exist (x + 1, y) bound
      && arr.(x + 1).(y) = c
      && not (XYSet.mem (x + 1, y) acc)
    then explore_connected c arr (x + 1, y) bound acc
    else acc
  in
  let acc =
    if
      side_exist (x, y - 1) bound
      && arr.(x).(y - 1) = c
      && not (XYSet.mem (x, y - 1) acc)
    then explore_connected c arr (x, y - 1) bound acc
    else acc
  in
  if
    side_exist (x, y + 1) bound
    && arr.(x).(y + 1) = c
    && not (XYSet.mem (x, y + 1) acc)
  then explore_connected c arr (x, y + 1) bound acc
  else acc

let rec init_map_aux arr bound lst =
  match lst with
  | [] -> (XYMap.empty, [])
  | (x, y) :: t ->
      let discovered_sets, acc = init_map_aux arr bound t in
      if XYMap.mem (x, y) discovered_sets then (discovered_sets, acc)
      else
        let connected =
          explore_connected arr.(x).(y) arr (x, y) bound XYSet.empty
        in
        ( XYMap.add_seq
            (connected |> XYSet.to_list
            |> List.map (fun p -> (p, (x * snd bound) + y))
            |> List.to_seq)
            discovered_sets,
          connected :: acc )

let init_map arr bound =
  let lst =
    arr
    |> Array.mapi (fun x a -> Array.mapi (fun y _ -> (x, y)) a)
    |> Array.map (fun a -> a |> Array.to_seq |> List.of_seq)
    |> Array.to_seq |> List.of_seq |> List.flatten
  in
  lst |> init_map_aux arr bound |> snd

let sidedness_of_point arr bound (x, y) : bool * bool * bool * bool =
  let c = arr.(x).(y) in
  ( (not (side_exist (x - 1, y) bound)) || not (arr.(x - 1).(y) = c),
    (not (side_exist (x + 1, y) bound)) || not (arr.(x + 1).(y) = c),
    (not (side_exist (x, y - 1) bound)) || not (arr.(x).(y - 1) = c),
    (not (side_exist (x, y + 1) bound)) || not (arr.(x).(y + 1) = c) (*E*) )

let effective_sides sideness_arr set (x, y) =
  let n, s, w, e = sideness_arr.(x).(y) in
  (if
     n
     &&
     if XYSet.mem (x, y - 1) set then
       let nn, _, _, _ = sideness_arr.(x).(y - 1) in
       not nn
     else true
   then 1
   else 0)
  + (if
       s
       &&
       if XYSet.mem (x, y - 1) set then
         let _, ns, _, _ = sideness_arr.(x).(y - 1) in
         not ns
       else true
     then 1
     else 0)
  + (if
       w
       &&
       if XYSet.mem (x - 1, y) set then
         let _, _, nw, _ = sideness_arr.(x - 1).(y) in
         not nw
       else true
     then 1
     else 0)
  +
  if
    e
    &&
    if XYSet.mem (x - 1, y) set then
      let _, _, _, ne = sideness_arr.(x - 1).(y) in
      not ne
    else true
  then 1
  else 0

let total_sides sidedness_arr set =
  let eff_sides =
    set |> XYSet.to_list |> List.map (effective_sides sidedness_arr set)
  in
  List.fold_left ( + ) 0 eff_sides

let get_cost arr =
  let bound = (Array.length arr, Array.length arr.(0)) in
  let per_grid = perimeter_grid arr bound in
  init_map arr bound |> List.map XYSet.to_list
  |> List.map (fun lst -> (List.length lst, get_perimeter_of_set per_grid lst))
  |> List.map (fun (area, peri) -> area * peri)
  |> List.fold_left ( + ) 0

let get_cost2 arr =
  let bound = (Array.length arr, Array.length arr.(0)) in
  let sidedness_arr =
    Array.mapi
      (fun x a -> Array.mapi (fun y _ -> sidedness_of_point arr bound (x, y)) a)
      arr
  in
  init_map arr bound
  |> List.map (fun set -> (XYSet.cardinal set, total_sides sidedness_arr set))
  |> List.map (fun (area, peri) -> area * peri)
  |> List.fold_left ( + ) 0

(* Solver functions *)
let parse_input (s : string) : input_t =
  s |> String.split_on_char '\n'
  |> List.map (fun s -> s |> String.to_seq |> Array.of_seq)
  |> List.filter (fun a -> Array.length a > 0)
  |> List.to_seq |> Array.of_seq

let solve1 (inp : input_t) : output_t = inp |> get_cost
let solve2 (inp : input_t) : output_t = inp |> get_cost2
let parse_output (out : output_t) : string = string_of_int out

(* End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
