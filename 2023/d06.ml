let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    contents
    |> String.split_on_char '\n'

(* let input_file = "d06_example.txt" *)
let input_file = "d06_input.txt"

let () =
    let lines = read_lines input_file in
    let times = lines
        |> List.hd
        |> String.split_on_char ' '
        |> List.tl
        |> List.filter_map (fun s -> match s with
            | "" -> None
            | _ -> print_endline s; Some (int_of_string s))
    in
    let distances = List.nth lines 1
        |> String.split_on_char ' '
        |> List.tl
        |> List.filter_map (fun s -> match s with
            | "" -> None
            | _ -> print_endline s; Some (int_of_string s))
    in
    let winning_variations time record_dist =
        (* special functions/operators for floats *)
        let max_dist_hold_ms = int_of_float (floor ((float time) /. 2.0)) in
        let variations_start = if time mod 2 <> 0 then 2 else 1 in
        let rec aux variations dt_ms =
            let hold_time = max_dist_hold_ms - dt_ms in
            let dist = (time - hold_time) * hold_time in
            if dist <= record_dist then 
                variations
            else
                aux (variations + 2) (dt_ms + 1)
        in
        aux variations_start 1
    in
    let rec part1 acc times distances =
        match (times, distances) with
        | (t::trest, d::drest) -> part1 (acc * (winning_variations t d)) trest drest
        | (_::_, []) -> raise (Failure "Too few distances!")
        | ([], _::_) -> raise (Failure "Too few times!")
        | ([], []) -> acc
    in
    Printf.printf "Part1: %d\n" (part1 1 times distances);
    let part2 =
        let collect_digits s = s
            |> String.fold_left (fun acc c -> match c with
                | '0'..'9' -> acc ^ (String.make 1 c)
                | _ -> acc) ""
            |> int_of_string
        in
        let time = lines
            |> List.hd
            |> collect_digits 
        in
        let distance = List.nth lines 1
            |> collect_digits
        in
        winning_variations time distance
    in
    Printf.printf "Part2: %d\n" (part2);

