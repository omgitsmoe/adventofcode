let read_lines file =
    (* open file and read all the contents, since we specify `input_all *)
    (* NOTE: needs at least ocaml 4.14 *)
    let contents = In_channel.with_open_bin file In_channel.input_all in
    (* split the lines *)
    String.split_on_char '\n' contents

(* let input_file = "d02_example.txt" *)
let input_file = "d02_input.txt"

let red_max = 12
let green_max = 13
let blue_max = 14

(* tagged union / rust enum equivalent, without of .. -> regular enum *)
(* NOTE: all names (but variant constructors and modules) have to start
   with a lowercase letter *)
type cube =
    | Red of int
    | Green of int
    | Blue of int

type game_max = {
    red: int;
    green: int;
    blue: int;
}

let () =
    let lines = read_lines input_file in
    let split_line line =
        if String.length line > 0 then
            line
            |> String.split_on_char ':'
            (* can't use map here, since map needs to return the same type
               so we'd have to return string here, but need a string list *)
            |> List.fold_left (fun acc s -> let cube_parts = s
                                    |> String.split_on_char ';'
                                    in
                                    (* append list to/extend acc *)
                                    acc @ cube_parts) []
            (* constructors are not functions in OCaml, *yuck*, so this
               needs to wrapped in a lambda *)
            |> (fun x -> Some x)
        else
            None
    in
    let get_game_id str =
        let one_before_id_start = String.rindex str ' ' in
        let num_length =  (String.length str) - one_before_id_start - 1 in
        int_of_string (String.sub str (one_before_id_start + 1) num_length)
    in
    (* draws: "\d cube_kind, \d cube_kind" *)
    let get_cubes draws =
        let num_cube_pairs =
            draws
            |> String.split_on_char ','
            |> List.fold_left (fun acc s ->
                    let pairs = s |> String.split_on_char ' ' in
                    pairs @ acc) []
        in
        (* helper function called `aux` by convention in OCaml, same as go/..
           in Haskell *)
        let rec aux nc acc =
            match nc with
            | [] -> acc
            (* empty string, happens when ' 3' is split -> [''; '3'] *)
            | ""::rest -> aux rest acc
            (* convert number to string and append the variant to our accumulator
               then recurse *)
            | num::"red"::rest -> aux rest ((Red (int_of_string num))::acc)
            | num::"green"::rest -> aux rest ((Green (int_of_string num))::acc)
            | num::"blue"::rest -> aux rest ((Blue (int_of_string num))::acc)
            | l -> raise (Failure "Expected number and cube kind pair!")
        in
        aux num_cube_pairs []
    in
    let parse_game line =
        let sl = split_line line in
        match sl with
        (* cubes: ["\d cube_kind, \d cube_kind"; ...] *)
        | Some (game::cubes) ->
            (get_game_id game), (cubes 
                                 |> List.fold_left
                                    (fun acc s -> acc @ (get_cubes s)) [])
        | _ -> raise (Failure ("Invalid line format: " ^ line))
    in
    let rec game_valid cubes =
        match cubes with
        | [] -> true
        | c::rest -> (match c with
                      | Red num -> if num > red_max then false else game_valid rest
                      | Green num -> if num > green_max then false else game_valid rest
                      | Blue num -> if num > blue_max then false else game_valid rest)
    in
    let rec part1 lines acc =
        match lines with
        | [] -> acc
        | l::rest ->
            (match parse_game l with
            | game_id, cubes ->
                    if game_valid cubes then
                        part1 rest (acc + game_id)
                    else
                        part1 rest acc
            (* exceptions are extensible variants that can be matched like
               any other value *)
            | exception (Failure _) -> part1 rest acc)
    in
    let max_cubes cubes =
        cubes
        |> List.fold_left (fun acc c ->
            match c with
            (* { x with .. } only update specific fields of x *)
            | Red n -> if n > acc.red then { acc with red = n } else acc
            | Green n -> if n > acc.green then { acc with green = n } else acc
            | Blue n -> if n > acc.blue then { acc with blue = n } else acc
        ) { red = 0; green = 0; blue = 0 }
    in
    let rec part2 lines acc =
        match lines with
        | [] -> acc
        | l::rest ->
            (match parse_game l with
            | game_id, cubes ->
                let cubes_required = max_cubes cubes in
                let prod = (cubes_required.red
                            * cubes_required.green
                            * cubes_required.blue) in
                Printf.printf "Max cubes: game %d -> r%d g%d b%d -> %d\n" game_id cubes_required.red cubes_required.green cubes_required.blue prod;
                part2 rest (acc + prod)
            (* exceptions are extensible variants that can be matched like
               any other value *)
            | exception (Failure _) -> part2 rest acc)
    in
    Printf.printf "Part1: %d\n" (part1 lines 0);
    Printf.printf "Part2: %d\n" (part2 lines 0);
