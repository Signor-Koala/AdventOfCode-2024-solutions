type tree = Leaf of int | Branch of tree * tree
type input_t = tree
type output_t = int

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

(* Helper functions *)
let rec split_list_at_n n lst =
  if n = 0 then ([], lst)
  else
    let h, t = split_list_at_n (n - 1) (List.tl lst) in
    (List.hd lst :: h, t)

let split_half lst = split_list_at_n (List.length lst / 2) lst

let rec tree_of_list lst : tree =
  if List.length lst = 0 then failwith "Empty list"
  else if List.length lst = 1 then Leaf (List.hd lst)
  else
    let left, right = split_half lst in
    Branch (tree_of_list left, tree_of_list right)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let rec digits a = if a / 10 = 0 then 1 else 1 + digits (a / 10)

let update_leaf value =
  if value = 0 then Leaf 1
  else if digits value mod 2 = 0 then
    let temp = pow 10 (digits value / 2) in
    let left = value / temp in
    let right = value - (left * temp) in
    Branch (Leaf left, Leaf right)
  else Leaf (value * 2024)

let rec update_tree = function
  | Leaf i -> update_leaf i
  | Branch (left, right) -> Branch (update_tree left, update_tree right)

let rec update_tree_n_times n t =
  if n = 1 then update_tree t else update_tree (update_tree_n_times (n - 1) t)

let rec list_of_tree = function
  | Leaf i -> [ i ]
  | Branch (left, right) -> list_of_tree left @ list_of_tree right

let add_val_to_map value count map =
  if IntMap.mem value map then
    IntMap.add value (IntMap.find value map + count) map
  else IntMap.add value count map

let add_val_to_map_once value map = add_val_to_map value 1 map

let rec init_map_of_list = function
  | [] -> IntMap.empty
  | h :: t -> add_val_to_map_once h (init_map_of_list t)

let tree_to_map t = list_of_tree t |> init_map_of_list

let rec update_map_aux lst map =
  match lst with
  | [] -> map
  | (value, count) :: t -> (
      match update_leaf value with
      | Leaf i -> map |> add_val_to_map i count |> update_map_aux t
      | Branch (Leaf x, Leaf y) ->
          map |> add_val_to_map x count |> add_val_to_map y count
          |> update_map_aux t
      | _ -> failwith "Shouldnt be here")

let update_map m = update_map_aux (IntMap.to_list m) IntMap.empty

let rec update_map_n_times n m =
  if n = 1 then update_map m else update_map (update_map_n_times (n - 1) m)

(* Solver functions *)
let parse_input (s : string) : input_t =
  s |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  |> tree_of_list

let solve1 (inp : input_t) : output_t =
  inp |> update_tree_n_times 25 |> list_of_tree |> List.length

let solve2 (inp : input_t) : output_t =
  inp |> tree_to_map |> update_map_n_times 75 |> IntMap.to_list
  |> List.map (fun (_, count) -> count)
  |> List.fold_left ( + ) 0

let parse_output (out : output_t) : string = string_of_int out

let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
