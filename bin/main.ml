let usage = "aocaml2024 -d <day> -p <part> <input_file>"
let day = ref 0
let input_file = ref ""
let part = ref 0
let anon_fun filename = input_file := filename

let speclist =
  [
    ("-d", Arg.Set_int day, "Day number of the puzzle");
    ("-p", Arg.Set_int part, "Part 1 or 2 of the puzzle");
  ]

exception Invalid_day of string
exception File_empty of string

let () =
  let () = Arg.parse speclist anon_fun usage in
  let f1, f2 =
    match !day with
    | 0 -> (AOC_2024.Day00.part1, AOC_2024.Day00.part2)
    | 1 -> failwith "Unimplimented"
    | 2 -> failwith "Unimplimented"
    | 3 -> failwith "Unimplimented"
    | 4 -> failwith "Unimplimented"
    | 5 -> failwith "Unimplimented"
    | 6 -> failwith "Unimplimented"
    | 7 -> failwith "Unimplimented"
    | 8 -> failwith "Unimplimented"
    | 9 -> failwith "Unimplimented"
    | 10 -> failwith "Unimplimented"
    | 11 -> failwith "Unimplimented"
    | 12 -> failwith "Unimplimented"
    | 13 -> failwith "Unimplimented"
    | 14 -> failwith "Unimplimented"
    | 15 -> failwith "Unimplimented"
    | 16 -> failwith "Unimplimented"
    | 17 -> failwith "Unimplimented"
    | 18 -> failwith "Unimplimented"
    | 19 -> failwith "Unimplimented"
    | 20 -> failwith "Unimplimented"
    | 21 -> failwith "Unimplimented"
    | 22 -> failwith "Unimplimented"
    | 23 -> failwith "Unimplimented"
    | 24 -> failwith "Unimplimented"
    | 25 -> failwith "Unimplimented"
    | d ->
        raise
          (Invalid_day
             (Printf.sprintf "%d is not a valid day number between 1-25" d))
  in
  let input_text =
    if String.equal !input_file "" then
      raise (File_empty "Filename is requried")
    else
      let ch = open_in !input_file in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      s
  in
  let ans1 = input_text |> f1 in
  let ans2 = input_text |> f2 in
  match !part with
  | 0 ->
      Printf.printf
        "\n\
         ------\n\
         Part 1 solution is\n\
         %s\n\
         ------\n\
         Part 2 solution is\n\
         %s\n\
         ------"
        ans1 ans2
  | 1 -> Printf.printf "\n------\nPart 1 solution is\n%s\n------" ans1
  | 2 -> Printf.printf "\n------\nPart 2 solution is\n%s\n------" ans2
  | _ -> print_endline "\n Invalid_part given \n"
