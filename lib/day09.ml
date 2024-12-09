type block_id = USED of int | FREE
type block = { id : block_id; pos : int; size : int }
type input_t = block list
type output_t = int

(** Helper functions *)
let rec parser_aux cur_id cur_pos empty = function
  | [] -> []
  | h :: t ->
      if empty then
        { id = FREE; pos = cur_pos; size = h }
        :: parser_aux cur_id (cur_pos + h) false t
      else
        { id = USED cur_id; pos = cur_pos; size = h }
        :: parser_aux (cur_id + 1) (cur_pos + h) true t

let parser = parser_aux 0 0 false

let unwrap_id = function
  | FREE -> failwith "Shouldn't get free blocks"
  | USED id -> id

let rec fill_up_empty_aux = function
  | he :: te, hr :: tr ->
      if he.pos > hr.pos then hr :: tr
      else if he.size = hr.size then
        { hr with pos = he.pos } :: fill_up_empty_aux (te, tr)
      else if he.size > hr.size then
        { hr with pos = he.pos }
        :: fill_up_empty_aux
             ( { he with pos = he.pos + hr.size; size = he.size - hr.size } :: te,
               tr )
      else
        { he with id = hr.id }
        :: fill_up_empty_aux (te, { hr with size = hr.size - he.size } :: tr)
  | _ -> failwith "Should end before this"

let rec fill_up_contig_aux el = function
  | [] -> ([], el)
  | he :: te ->
      if el.pos < he.pos then (he :: te, el)
      else if el.size = he.size then (te, { el with pos = he.pos })
      else if el.size < he.size then
        ( { he with pos = he.pos + el.size; size = he.size - el.size } :: te,
          { el with pos = he.pos } )
      else
        let emp, new_alloc = fill_up_contig_aux el te in
        (he :: emp, new_alloc)

let rec fill_up_contig = function
  | [], [] -> failwith "Shouldn't get here"
  | [], hr :: tr -> hr :: tr
  | _ :: _, [] -> []
  | he :: te, hr :: tr ->
      let new_empty, new_alloc = fill_up_contig_aux hr (he :: te) in
      new_alloc :: fill_up_contig (new_empty, tr)

let fill_up_empty ?(part2 = false) lst =
  let empty =
    List.filter
      (fun block -> match block.id with FREE -> true | _ -> false)
      lst
  in
  let rev =
    List.filter
      (fun block -> match block.id with FREE -> false | _ -> true)
      lst
    |> List.rev
  in
  if part2 then fill_up_contig (empty, rev) else fill_up_empty_aux (empty, rev)

let rec checksum = function
  | [] -> 0
  | h :: t ->
      (unwrap_id h.id * ((h.pos * h.size) + (h.size * (h.size - 1) / 2)))
      + checksum t

(** Solver functions *)
let parse_input (s : string) : input_t =
  s |> String.to_seq |> List.of_seq
  |> List.map (fun c -> int_of_char c - 48)
  |> parser

let solve1 (inp : input_t) : output_t = inp |> fill_up_empty |> checksum

let solve2 (inp : input_t) : output_t =
  inp |> fill_up_empty ~part2:true |> checksum

let parse_output (out : output_t) : string = string_of_int out

(** End to end functions *)
let part1 (input_text : string) : string =
  input_text |> parse_input |> solve1 |> parse_output

let part2 (input_text : string) : string =
  input_text |> parse_input |> solve2 |> parse_output
