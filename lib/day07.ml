type input_t = (int * int list) list
type output_t = int

let process_lines s =
  match String.split_on_char ':' s with
  | [ test; eqn ] ->
      ( int_of_string test,
        eqn |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
        |> List.rev )
  | l ->
      Util.print_list print_string l;
      failwith "Invalid line"

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let rec digits a = if a / 10 = 0 then 1 else 1 + digits (a / 10)
let ( ||? ) h t = (h * pow 10 (digits t)) + t

let rec get_possibilities ?(concat = false) eqn =
  match eqn with
  | [] -> failwith "Empty list"
  | h :: [] -> h :: []
  | h :: t ->
      let t = get_possibilities ~concat t in
      List.map (fun x -> x * h) t
      @ List.map (fun x -> x + h) t
      @ if concat then List.map (fun x -> x ||? h) t else []

let check_eqn ?(concat = false) (test, eqn) =
  List.exists (Int.equal test) (get_possibilities ~concat eqn)

let add_passes ?(concat = false) lst =
  lst
  |> List.filter (check_eqn ~concat)
  |> List.map (fun (test, _) -> test)
  |> List.fold_left ( + ) 0

let parse_input (s : string) : input_t =
  print_int (123 ||? 456);
  s |> String.split_on_char '\n'
  |> List.filter (fun s -> not (String.equal String.empty s))
  |> List.map process_lines

let solve1 (inp : input_t) : output_t = add_passes inp
let solve2 (inp : input_t) : output_t = add_passes ~concat:true inp
let parse_output (out : output_t) : string = string_of_int out

let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
