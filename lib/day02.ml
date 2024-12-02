type input_t = int list list
type output_t = int
type order = Increasing | Decreasing

(** Helper functions *)
let check_order = function
  | x :: xs :: _ -> if x < xs then Increasing else Decreasing
  | _ -> failwith "List too short"

let rec check_safety_aux ord lst =
  match lst with
  | x :: xs :: t -> (
      match ord with
      | Increasing ->
          if x >= xs || xs > x + 3 then false else check_safety_aux ord (xs :: t)
      | Decreasing ->
          if xs >= x || x > xs + 3 then false else check_safety_aux ord (xs :: t)
      )
  | _ -> true

let check_safety lst =
  let ord = check_order lst in
  check_safety_aux ord lst

let rec get_list_of_lists lst : int list list =
  match lst with
  | [] -> []
  | h :: t -> List.map (fun x -> h :: x) (get_list_of_lists t) @ [ t ]

let check_safety_of_all lst =
  lst |> get_list_of_lists |> List.map check_safety
  |> List.exists (fun x -> if x then true else false)

(** Solver functions *)
let parse_input (s : string) : input_t =
  s |> String.split_on_char '\n'
  |> List.filter (fun x -> not (String.equal String.empty x))
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)

let solve1 (inp : input_t) : output_t =
  inp |> List.map check_safety
  |> List.map (fun x -> if x = true then 1 else 0)
  |> List.fold_left ( + ) 0

let solve2 (inp : input_t) : output_t =
  inp
  |> List.map check_safety_of_all
  |> List.map (fun x -> if x = true then 1 else 0)
  |> List.fold_left ( + ) 0

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
