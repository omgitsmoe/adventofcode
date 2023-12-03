(* compile with `ocamplopt -o d01 d01.ml` *)
(* NOTE: as opposed to Haskell (but similar to F#) OCaml has imperative parts
   like for loops etc., e.g. the String type is also imperative.
   Functions who are imperative return ()/unit *)

(* helper function to read a text file into a list of lines *)
(* `in` not needed when defining at the top-level *)
let read_lines file =
    (* open file and read all the contents, since we specify `input_all *)
    (* NOTE: needs at least ocaml 4.14 *)
    let contents = In_channel.with_open_bin file In_channel.input_all in
    (* split the lines *)
    String.split_on_char '\n' contents

(* let input_file = "d01_example.txt" *)
let input_file = "d01_input.txt"

(* no entry point main like C etc., normally the file is just evaluated top
   to bottom, but convention is to use `let () = ..` as "entry point" *)
let () =
    (* bind lines to expression `read_lines ...` `in` the following expression *)
    let lines = read_lines input_file in
    let first_and_last_digit line =
        (* helper for folding, carries first and last digit tuple in acc *)
        let f acc char = match char with
        (* nested match expressions need to be sorrounded with () or begin/end *)
        | '0'..'9' -> (match acc with
                       (* no first digit yet, set both since there might only be one *)
                       | None, None -> Some char, Some char
                       (* has first digit, so replace last digit *)
                       | first, last -> first, Some char)
        | _ -> acc
        in
        String.fold_left f (None, None) line
    in
    let first_and_last_digit_pt2 line =
        let match_spelled_digit str acc =
            (* could use a when guard here also, but less performant and harder
               to read in some sense -> should only be used when the same can't
               be expressed well with if/then/else *)
            (* ~label:value -> pass named argument *)
            if String.starts_with ~prefix:"zero" str then Some '0' else
            if String.starts_with ~prefix:"one" str then Some '1' else
            if String.starts_with ~prefix:"two" str then Some '2' else
            if String.starts_with ~prefix:"three" str then Some '3' else
            if String.starts_with ~prefix:"four" str then Some '4' else
            if String.starts_with ~prefix:"five" str then Some '5' else
            if String.starts_with ~prefix:"six" str then Some '6' else
            if String.starts_with ~prefix:"seven" str then Some '7' else
            if String.starts_with ~prefix:"eight" str then Some '8' else
            if String.starts_with ~prefix:"nine" str then Some '9' else
            None
        in
        let fill_accumulator char acc =
            match acc with
            (* no digit yet *)
            | None, None -> Some char, Some char
            (* have first digit -> replace last *)
            | first, last -> first, Some char
        in
        let rec f str acc =
            if String.length str > 0 then
                (* for the next iteration remove first char *)
                (* NOTE: inefficient, since we copy the str every time;
                         could convert to a list instead,
                         afaik Ocaml has no slices *)
                let next_substr = String.sub str 1 ((String.length str) - 1) in
                let first_char = (String.get str 0) in
                match first_char with
                | '0'..'9' -> f next_substr (fill_accumulator first_char acc)
                (* possible spelled out digit *)
                | _ -> (match match_spelled_digit str acc with
                      | Some char -> f next_substr (fill_accumulator char acc)
                      | None -> f next_substr acc)
            else acc
        in
        f line (None, None)
    in
    (* version using a char list instead of string *)
    let first_and_last_digit_pt2_list line =
        let fill_accumulator char acc =
            match acc with
            (* no digit yet *)
            | None, None -> Some char, Some char
            (* have first digit -> replace last *)
            | first, last -> first, Some char
        in
        let rec f str acc =
            match str with
            | [] -> acc
            | c :: rest when c >= '0' && c <= '9' ->
                f rest (fill_accumulator c acc)
            (* tl -> removes first list element, needed here since spelled out
               digits can __overlap__ *)
            | 'z'::'e'::'r'::'o'::_ -> f (List.tl str) (fill_accumulator '0' acc)
            | 'o'::'n'::'e'::_ -> f (List.tl str) (fill_accumulator '1' acc)
            | 't'::'w'::'o'::_ -> f (List.tl str) (fill_accumulator '2' acc)
            | 't'::'h'::'r'::'e'::'e'::_ -> f (List.tl str) (fill_accumulator '3' acc)
            | 'f'::'o'::'u'::'r'::_ -> f (List.tl str) (fill_accumulator '4' acc)
            | 'f'::'i'::'v'::'e'::_ -> f (List.tl str) (fill_accumulator '5' acc)
            | 's'::'i'::'x'::_ -> f (List.tl str) (fill_accumulator '6' acc)
            | 's'::'e'::'v'::'e'::'n'::_ -> f (List.tl str) (fill_accumulator '7' acc)
            | 'e'::'i'::'g'::'h'::'t'::_ -> f (List.tl str) (fill_accumulator '8' acc)
            | 'n'::'i'::'n'::'e'::_ -> f (List.tl str) (fill_accumulator '9' acc)
            | _ :: rest -> f rest acc
        in
        f (line |> String.to_seq |> List.of_seq) (None, None)
    in
    (* OCaml stdlib is veeeery basic, there's also JaneStreet's Core and
       Base alternatively, which are way more complete *)
    let concatenate_chars c1 c2 =
        (* make a string of 2 chars with c1 for all characters *)
        (* NOTE: use bytes instead, since string is now immutable *)
        let str = Bytes.create 2 in
        (* end with `;` since Bytes.set is imperative *)
        Bytes.set str 0 c1;
        Bytes.set str 1 c2;
        String.of_bytes str
    in
    let num_from_line f_digits line =
        (* line might be empty *)
        if line = "" then 0 else
        match (f_digits line) with
        | Some first, Some last -> int_of_string (concatenate_chars first last)
        | _ -> raise (Failure ("Line must have at least one digit: " ^ line))
    in
    let rec part1 ls acc =
        match ls with
        (* return acc when empty list *)
        | [] -> acc
        (* match on list with at leas one element *)
        (* take the line, add first and last digit to acc and then recurse with
           the rest *)
        (* NOTE: if a func has multiple arguments the pipe |> will supply the
           __last__ one (not the first like in R) *)
        | line :: ls -> part1 ls (
            line |> num_from_line first_and_last_digit |> (+) acc)
    in
    let rec part2 ls acc =
        match ls with
        (* return acc when empty list *)
        | [] -> acc
        (* match on list with at leas one element *)
        (* take the line, add first and last digit to acc and then recurse with
           the rest *)
        | line :: ls -> part2 ls (
            line |> num_from_line first_and_last_digit_pt2_list |> (+) acc)
    in
    Printf.printf "Part1: %d\n" (part1 lines 0);
    Printf.printf "Part2: %d\n" (part2 lines 0)
