type input_t = char array array
type output_t = int

(** Helper functions *)
let get_directions (arr : input_t) =
  let rows = Array.length arr in
  let cols = Array.length arr.(0) in
  let row_str = Array.map (fun s -> s |> Array.to_seq |> String.of_seq) arr in
  let col_str =
    Array.map String.of_seq
      (Array.to_seq (Array.map Array.to_seq arr)
      |> Seq.transpose |> Array.of_seq)
  in
  let diag_down = Array.init (rows + cols) (fun _ -> "") in
  let diag_up = Array.init (rows + cols) (fun _ -> "") in
  for x = -rows to 2 * rows do
    for y = 0 to cols - 1 do
      if x + y < rows && x + y >= 0 then
        diag_down.(x + rows) <-
          diag_down.(x + rows) ^ String.make 1 arr.(x + y).(y)
      else ();
      if x - y < rows && x - y >= 0 then
        diag_up.(x) <- diag_up.(x) ^ String.make 1 arr.(x - y).(y)
      else ()
    done
  done;
  [ row_str; col_str; diag_down; diag_up ]

let rec count_substr_aux re s i =
  match Str.search_forward re s i with
  | i -> 1 + count_substr_aux re s (i + 1)
  | exception Not_found -> 0

let count_substr ss s =
  let re = Str.regexp_string ss in
  count_substr_aux re s 0

let find_cross arr =
  let res = ref [] in
  for x = 1 to Array.length arr - 2 do
    for y = 1 to Array.length arr.(0) - 2 do
      if arr.(x).(y) = 'A' then res := (x, y) :: !res else ()
    done
  done;
  !res

let extract_cross_aux arr (x, y) =
  if
    x < 1 || x >= Array.length arr - 1 || y < 1 || y >= Array.length arr.(0) - 1
  then []
  else
    [
      [ arr.(x - 1).(y - 1); arr.(x - 1).(y); arr.(x - 1).(y + 1) ];
      [ arr.(x).(y - 1); arr.(x).(y); arr.(x).(y + 1) ];
      [ arr.(x + 1).(y - 1); arr.(x + 1).(y); arr.(x + 1).(y + 1) ];
    ]

let rec extract_cross arr = function
  | [] -> []
  | h :: t -> extract_cross_aux arr h :: extract_cross arr t

let check_cross cross =
  match cross with
  | [] -> failwith "Empty"
  | [ [ 'M'; _; 'M' ]; [ _; 'A'; _ ]; [ 'S'; _; 'S' ] ]
  | [ [ 'S'; _; 'M' ]; [ _; 'A'; _ ]; [ 'S'; _; 'M' ] ]
  | [ [ 'M'; _; 'S' ]; [ _; 'A'; _ ]; [ 'M'; _; 'S' ] ]
  | [ [ 'S'; _; 'S' ]; [ _; 'A'; _ ]; [ 'M'; _; 'M' ] ] ->
      true
  | [ [ _; _; _ ]; [ _; _; _ ]; [ _; _; _ ] ] -> false
  | _ -> failwith "Not proper format"

(** Solver functions *)
let parse_input (s : string) : input_t =
  s
  |> Str.split (Str.regexp "\n")
  |> List.map (fun s -> s |> String.to_seq |> Array.of_seq)
  |> Array.of_list

let solve1 (inp : input_t) : output_t =
  inp |> get_directions
  |> List.map
       (Array.fold_left
          (fun acc s -> acc + count_substr "XMAS" s + count_substr "SAMX" s)
          0)
  |> List.fold_left ( + ) 0

let solve2 (inp : input_t) : output_t =
  inp |> find_cross |> extract_cross inp |> List.filter check_cross
  |> List.length

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
