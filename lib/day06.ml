type pos = int * int

module XY = struct
  type t = pos

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c
end

module XYSet = Set.Make (XY)

type current_pos =
  | UP of pos
  | DOWN of pos
  | LEFT of pos
  | RIGHT of pos
  | OUT_OF_BOUNDS

let unwrap_pos = function
  | UP (x, y) | DOWN (x, y) | LEFT (x, y) | RIGHT (x, y) -> (x, y)
  | OUT_OF_BOUNDS -> failwith "cant unwrap"

let unwrap_dir = function
  | UP (_, _) -> 1
  | DOWN (_, _) -> 2
  | LEFT (_, _) -> 3
  | RIGHT (_, _) -> 4
  | OUT_OF_BOUNDS -> failwith "cant unwrap"

module XYDir = struct
  type t = current_pos

  let compare p1 p2 =
    let x1, y1 = unwrap_pos p1 in
    let x2, y2 = unwrap_pos p2 in
    match Stdlib.compare x1 x2 with
    | 0 -> (
        match Stdlib.compare y1 y2 with 0 -> Stdlib.compare p1 p2 | c' -> c')
    | c -> c
end

module XYDirSet = Set.Make (XYDir)

type input_t = XYSet.t * current_pos * pos
type output_t = int

(** Helper functions *)
let rec find_pos_of_aux re i s =
  match Str.search_forward re s i with
  | i -> i :: find_pos_of_aux re (i + 1) s
  | exception Not_found -> []

let find_pos_of ss s = find_pos_of_aux (Str.regexp_string ss) 0 s

let rec build_table = function
  | [] -> XYSet.empty
  | (x, y) :: t -> XYSet.add (x, y) (build_table t)

let rec find_current_pos x ls =
  match ls with
  | [] -> failwith "Not in list"
  | h :: t -> (
      match Str.search_forward (Str.regexp_string "^") h 0 with
      | y -> UP (x, y)
      | exception Not_found -> find_current_pos (x + 1) t)

let rec goto_next_obstacle obstbl (boundx, boundy) = function
  | UP (x, y) ->
      if x - 1 = -1 then (OUT_OF_BOUNDS, (x, y) :: [])
      else if XYSet.mem (x - 1, y) obstbl then (RIGHT (x, y), (x, y) :: [])
      else
        let pos, list =
          goto_next_obstacle obstbl (boundx, boundy) (UP (x - 1, y))
        in
        (pos, (x, y) :: list)
  | DOWN (x, y) ->
      if x + 1 = boundx then (OUT_OF_BOUNDS, (x, y) :: [])
      else if XYSet.mem (x + 1, y) obstbl then (LEFT (x, y), (x, y) :: [])
      else
        let pos, list =
          goto_next_obstacle obstbl (boundx, boundy) (DOWN (x + 1, y))
        in
        (pos, (x, y) :: list)
  | LEFT (x, y) ->
      if y - 1 = -1 then (OUT_OF_BOUNDS, (x, y) :: [])
      else if XYSet.mem (x, y - 1) obstbl then (UP (x, y), (x, y) :: [])
      else
        let pos, list =
          goto_next_obstacle obstbl (boundx, boundy) (LEFT (x, y - 1))
        in
        (pos, (x, y) :: list)
  | RIGHT (x, y) ->
      if y + 1 = boundy then (OUT_OF_BOUNDS, (x, y) :: [])
      else if XYSet.mem (x, y + 1) obstbl then (DOWN (x, y), (x, y) :: [])
      else
        let pos, list =
          goto_next_obstacle obstbl (boundx, boundy) (RIGHT (x, y + 1))
        in
        (pos, (x, y) :: list)
  | OUT_OF_BOUNDS -> failwith "Shouldn't get here"

let rec pathfind obstbl boundary cur_pos =
  let next_pos, visited = goto_next_obstacle obstbl boundary cur_pos in
  match next_pos with
  | OUT_OF_BOUNDS -> XYSet.empty |> XYSet.add_seq (List.to_seq visited)
  | n -> pathfind obstbl boundary n |> XYSet.add_seq (List.to_seq visited)

let point_to_pos dir point =
  match dir with
  | OUT_OF_BOUNDS -> failwith "Invalid dir"
  | UP (_, _) -> UP point
  | DOWN (_, _) -> DOWN point
  | LEFT (_, _) -> LEFT point
  | RIGHT (_, _) -> RIGHT point

let rec cycle_checker_aux prev_obs obstbl (boundx, boundy) cur_pos =
  match cur_pos with
  | UP (x, y) ->
      if x - 1 = -1 then false
      else if XYSet.mem (x - 1, y) obstbl then
        if XYDirSet.mem cur_pos prev_obs then true
        else
          cycle_checker_aux
            (prev_obs |> XYDirSet.add cur_pos)
            obstbl (boundx, boundy)
            (RIGHT (x, y))
      else cycle_checker_aux prev_obs obstbl (boundx, boundy) (UP (x - 1, y))
  | DOWN (x, y) ->
      if x + 1 = boundx then false
      else if XYSet.mem (x + 1, y) obstbl then
        if XYDirSet.mem cur_pos prev_obs then true
        else
          cycle_checker_aux
            (prev_obs |> XYDirSet.add cur_pos)
            obstbl (boundx, boundy)
            (LEFT (x, y))
      else cycle_checker_aux prev_obs obstbl (boundx, boundy) (DOWN (x + 1, y))
  | LEFT (x, y) ->
      if y - 1 = -1 then false
      else if XYSet.mem (x, y - 1) obstbl then
        if XYDirSet.mem cur_pos prev_obs then true
        else
          cycle_checker_aux
            (prev_obs |> XYDirSet.add cur_pos)
            obstbl (boundx, boundy)
            (UP (x, y))
      else cycle_checker_aux prev_obs obstbl (boundx, boundy) (LEFT (x, y - 1))
  | RIGHT (x, y) ->
      if y + 1 = boundy then false
      else if XYSet.mem (x, y + 1) obstbl then
        if XYDirSet.mem cur_pos prev_obs then true
        else
          cycle_checker_aux
            (prev_obs |> XYDirSet.add cur_pos)
            obstbl (boundx, boundy)
            (DOWN (x, y))
      else cycle_checker_aux prev_obs obstbl (boundx, boundy) (RIGHT (x, y + 1))
  | OUT_OF_BOUNDS -> failwith "Shouldn't get here"

let rec cycle_counter prev_pos obstbl (boundx, boundy) cur_pos =
  match cur_pos with
  | UP (x, y) ->
      let prev_pos = XYSet.add (x, y) prev_pos in
      if x - 1 = -1 then XYSet.empty
      else if XYSet.mem (x - 1, y) obstbl then
        cycle_counter prev_pos obstbl (boundx, boundy) (RIGHT (x, y))
      else if
        if not (XYSet.mem (x - 1, y) prev_pos) then
          cycle_checker_aux XYDirSet.empty
            (obstbl |> XYSet.add (x - 1, y))
            (boundx, boundy)
            (RIGHT (x, y))
        else false
      then
        XYSet.add
          (x - 1, y)
          (cycle_counter prev_pos obstbl (boundx, boundy) (UP (x - 1, y)))
      else cycle_counter prev_pos obstbl (boundx, boundy) (UP (x - 1, y))
  | DOWN (x, y) ->
      let prev_pos = XYSet.add (x, y) prev_pos in
      if x + 1 = boundx then XYSet.empty
      else if XYSet.mem (x + 1, y) obstbl then
        cycle_counter prev_pos obstbl (boundx, boundy) (LEFT (x, y))
      else if
        if not (XYSet.mem (x + 1, y) prev_pos) then
          cycle_checker_aux XYDirSet.empty
            (obstbl |> XYSet.add (x + 1, y))
            (boundx, boundy)
            (LEFT (x, y))
        else false
      then
        XYSet.add
          (x + 1, y)
          (cycle_counter prev_pos obstbl (boundx, boundy) (DOWN (x + 1, y)))
      else cycle_counter prev_pos obstbl (boundx, boundy) (DOWN (x + 1, y))
  | LEFT (x, y) ->
      let prev_pos = XYSet.add (x, y) prev_pos in
      if y - 1 = -1 then XYSet.empty
      else if XYSet.mem (x, y - 1) obstbl then
        cycle_counter prev_pos obstbl (boundx, boundy) (UP (x, y))
      else if
        if not (XYSet.mem (x, y - 1) prev_pos) then
          cycle_checker_aux XYDirSet.empty
            (obstbl |> XYSet.add (x, y - 1))
            (boundx, boundy)
            (UP (x, y))
        else false
      then
        XYSet.add
          (x, y - 1)
          (cycle_counter prev_pos obstbl (boundx, boundy) (LEFT (x, y - 1)))
      else cycle_counter prev_pos obstbl (boundx, boundy) (LEFT (x, y - 1))
  | RIGHT (x, y) ->
      let prev_pos = XYSet.add (x, y) prev_pos in
      if y + 1 = boundy then XYSet.empty
      else if XYSet.mem (x, y + 1) obstbl then
        cycle_counter prev_pos obstbl (boundx, boundy) (DOWN (x, y))
      else if
        if not (XYSet.mem (x, y + 1) prev_pos) then
          cycle_checker_aux XYDirSet.empty
            (obstbl |> XYSet.add (x, y + 1))
            (boundx, boundy)
            (DOWN (x, y))
        else false
      then
        XYSet.add
          (x, y + 1)
          (cycle_counter prev_pos obstbl (boundx, boundy) (RIGHT (x, y + 1)))
      else cycle_counter prev_pos obstbl (boundx, boundy) (RIGHT (x, y + 1))
  | OUT_OF_BOUNDS -> failwith "Shouldn't get here"

(** Solver functions *)
let parse_input (s : string) : input_t =
  let lines = s |> String.split_on_char '\n' in
  ( lines
    |> List.mapi (fun x s -> find_pos_of "#" s |> List.map (fun y -> (x, y)))
    |> List.flatten |> build_table,
    find_current_pos 0 lines,
    (List.length lines - 1, String.length (List.hd lines)) )

let solve1 (inp : input_t) : output_t =
  let obstbl, current_pos, bound = inp in
  pathfind obstbl bound current_pos |> XYSet.cardinal

let solve2 (inp : input_t) : output_t =
  let obstbl, current_pos, bound = inp in
  cycle_counter XYSet.empty obstbl bound current_pos |> XYSet.cardinal

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
