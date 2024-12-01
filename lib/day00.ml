type input_t = string
type output_t = string

let parse_input (s : string) : input_t = s
let solve1 (inp : input_t) : output_t = inp
let solve2 (inp : input_t) : output_t = inp
let parse_output (out : output_t) : string = out

let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
