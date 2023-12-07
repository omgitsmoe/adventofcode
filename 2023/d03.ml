let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    contents
    |> String.split_on_char '\n'
    |> Array.of_list

(* let input_file = "d03_example.txt" *)
let input_file = "d03_input.txt"

type number = {
    x_min: int;
    x_max: int;
    y: int;
    value: int;
}

(* y,x in cw direction including diagonals *)
let directions = [
    (0, -1); (-1, 1); (0, 1);
    (1, 1); (1, 0); (1, -1);
    (0, -1); (-1, -1)]

(* Set is a functor that returns a set module from a module that implements
   the OrderedType interface *)
module Number = struct (* can contain type definitions etc., but can only be bound to module *)
    type t = number
    let compare n0 n1 = 
        match Stdlib.compare n0.y n1.y with
        (* compared equal *)
        | 0 -> Stdlib.compare n0.x_min n1.x_min
        | c -> c
    (* functions added here could be called like: Number.func, but it's
       not like a class and behaves like a namespace *)
end

module NumberSet = Set.Make(Number)

let () =
    let lines = read_lines input_file in
    let get_safe y x lines =
        match Array.get lines y with
        | l ->
            (match String.get l x with
            | c -> Some c
            | exception (Invalid_argument _) -> None)
        | exception (Invalid_argument _) -> None
    in
    (* returns list of numbers adjacent to a symbol *)
    let parse_line (y, numbers) line =
        (* check it there is a symbol around y, x *)
        let find_sym x =
            List.exists (fun (dy, dx) ->
                match get_safe (dy + y) (dx + x) lines with
                | None | Some '0'..'9' | Some '.' -> false
                | Some _ -> true) directions
        in
        (* construct number from y_min, y_max and single digit chars *)
        let make_num start end_idx digits =
            (* helper func so we don't have to carry start/end around etc.
               and user doesn't have to provide init for acc *)
            let rec aux acc digits =
                match digits with
                | [] -> {x_min = start; x_max = end_idx; y = y; value = int_of_string acc}
                (* prefix char before string, since we get them in reverse order *)
                | d::rest -> aux (String.make 1 d ^ acc) rest
            in
            aux "" digits
        in
        (* state machine that parse the numbers and adds them to the set *)
        (* NOTE: end and include are keywords *)
        let aux (x, in_number, start, end_idx, digits, includen, line_numbers) c =
            match c, in_number with
            | '0'..'9', false ->
                (* start number *)
                x+1, true, x, String.length line - 1, c::[],
                find_sym x, line_numbers
            | '0'..'9', true ->
                (* continue number *)
                x+1, true, start, end_idx, c::digits,
                includen || (find_sym x), line_numbers
            | _, false -> x+1, false, start, end_idx, digits, includen, line_numbers
            (* end_idx number *)
            | _, true when includen = false ->
                    x+1, false, 0, 0, [], false, line_numbers
            | _, true when includen = true ->
                    x+1, false, 0, 0, [], false,
                    (* we're one beyond the end (incl.) so use x-1 *)
                    NumberSet.add (make_num start (x-1) digits) line_numbers
        in
        let (_, in_number, start, end_idx, digits, includen, line_numbers) =
            String.fold_left aux (0, false, 0, 0, [], false, numbers) line in
        (* check if we were parsing a number *)
        match in_number, includen with
        | true, true -> NumberSet.add (make_num start end_idx digits) line_numbers
        | _, _ -> line_numbers
    in
    let numbers, part1 =
        let aux (y, numbers) line =
            y + 1, (parse_line (y, numbers) line) in
        let _, numbers = lines
                      |> Array.fold_left aux (0, NumberSet.empty)
        in
        numbers, NumberSet.fold (fun n acc -> acc + n.value) numbers 0
    in
    Printf.printf "Part1: %d\n" part1;
    
    let get_part_nums y x =
        let collides_with n y x =
            if n.y = y && x >= n.x_min && x <= n.x_max
                then (Printf.printf "Num: y%dxmin%dxmax%d val:%d\n" n.y n.x_min n.x_max n.value; true)
                else false
        in
        (* check if we have an adjacent number first *)
        (* -> DON'T, since we would be looking up if we have a __digit__
              so we could get the same number multiple times *)
        (* -> ALSO don't check the directions from the symbol,
              rather check them from the part numbers, otherwise
              we'd still get the same part number multiple times *)
        NumberSet.fold (fun num parts ->
            let hit = directions
                |> List.exists (fun (dy, dx) ->
                        (* NOTE: contrary to the note above we can check from
                                 sym here, but only since we're only visiting
                                 each number once *)
                        if collides_with num (y + dy) (x + dx)
                        then
                            (Printf.printf "Collides with y%d+%d x%d+%d\n" y dy x dx;
                            true)
                        else false) in
            match hit with
            | true -> num::parts
            | false -> parts
        ) numbers []
    in
    let _, part2 = lines
    |> Array.fold_left (fun (y, syms) line ->
        let _, syms_line =
            line
            |> String.fold_left (fun (x, syms) c ->
                match c with
                | '*' ->
                    let parts = get_part_nums y x in
                    Printf.printf "Sym y%dx%d: has %d parts\n" y x (List.length parts);
                    (* needs exactly two part numbers *)
                    (match parts with
                    | [p1; p2] -> x+1, syms + p1.value * p2.value
                    | _ -> x+1, syms)
                | _ -> x + 1, syms) (0, 0)
        in
        Printf.printf "Syms in line: %d\n" syms_line;
        y + 1, syms + syms_line) (0, 0)
    in
    Printf.printf "Part2: %d\n" part2;
