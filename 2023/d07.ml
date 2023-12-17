let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    contents
    |> String.trim
    |> String.split_on_char '\n'

(* let input_file = "d07_example.txt" *)
let input_file = "d07_input.txt"
(* let input_file = "d07_test.txt" *)

let compare_card a b =
    if a = b then 0 else
    match a, b with
    | 'A', _ -> 1
    | 'K', '2'..'9' -> 1
    | 'K', 'Q' -> 1
    | 'K', 'J' -> 1
    | 'K', 'T' -> 1
    | 'Q', '2'..'9' -> 1
    | 'Q', 'T' -> 1
    | 'Q', 'J' -> 1
    | 'J', '2'..'9'
    | 'J', 'T' -> 1
    | 'T', '2'..'9' -> 1
    | '2'..'9', '2'..'9' -> Stdlib.compare a b
    | _, _ -> -1

let compare_card_pt2 a b =
    if a = b then 0 else
    match a, b with
    | 'A', _ -> 1
    | 'K', '2'..'9' -> 1
    | 'K', 'Q' -> 1
    | 'K', 'J' -> 1
    | 'K', 'T' -> 1
    | 'Q', '2'..'9' -> 1
    | 'Q', 'T' -> 1
    | 'Q', 'J' -> 1
    | 'T', '2'..'9' -> 1
    | 'T', 'J' -> 1
    | '2'..'9', 'J' -> 1
    | '2'..'9', '2'..'9' -> Stdlib.compare a b
    | _, _ -> -1

type hand =
    | FIVE_OF_A_KIND
    | FOUR_OF_A_KIND
    | FULL_HOUSE
    | THREE_OF_A_KIND
    | TWO_PAIR
    | ONE_PAIR
    | HIGH_CARD

let int_of_hand hand =
    match hand with
    | FIVE_OF_A_KIND -> 6
    | FOUR_OF_A_KIND -> 5
    | FULL_HOUSE -> 4
    | THREE_OF_A_KIND -> 3
    | TWO_PAIR -> 2
    | ONE_PAIR -> 1
    | HIGH_CARD -> 0

let print_hand h =
    match h with
    | FIVE_OF_A_KIND -> print_endline "FIVE_OF_A_KIND"
    | FOUR_OF_A_KIND -> print_endline "FOUR_OF_A_KIND"
    | FULL_HOUSE -> print_endline "FULL_HOUSE"
    | THREE_OF_A_KIND -> print_endline "THREE_OF_A_KIND"
    | TWO_PAIR -> print_endline "TWO_PAIR"
    | ONE_PAIR -> print_endline "ONE_PAIR"
    | HIGH_CARD -> print_endline "HIGH_CARD"

let is_hand_stronger compare_card a b =
    let rec aux a b =
        match a, b with
        | c1::rest1, c2::rest2 ->
            let cmp = compare_card c1 c2 in
            (match cmp with
            | 1 -> true
            | -1 -> false
            | 0 -> aux rest1 rest2)
        | [], [] -> raise (Failure "Hands equally strong!")
    in
    aux a b

let compare_hand (a: char list * hand) (b: char list * hand) compare_card: int =
    let a, a_type = a in
    let b, b_type = b in
    if a_type = b_type then
        if is_hand_stronger compare_card a b then 1 else -1
    else
        Stdlib.compare (int_of_hand a_type) (int_of_hand b_type)

let hand_type cards =
    let rec aux cards streaks =
        let extend_streak streaks = match streaks with
        | [] -> 2::[]
        (* s::rest matches __both__ [s, ...] and [s] *)
        | s::rest -> (s+1)::rest
        in
        match cards with
        | c1::(c2::_ as rest) -> if c1 = c2
            then
                (* start/continue streak of matching cards *)
                aux rest (extend_streak streaks)
            else
                (* end streak *)
                aux rest (1::streaks)
        (* remove empty streak when stopping *)
        | _::[] -> (match streaks with
            | 1::others -> others
            | _ -> streaks)
        | _ -> raise (Failure "Empty list should not happen!")
    in
    let sorted_cards = List.sort Stdlib.compare cards in
    (* descending order *)
    let matching_cards = aux sorted_cards []
        |> List.sort (fun i1 i2 -> (Stdlib.compare i1 i2) * -1) in
    match matching_cards with
    | 5::_ -> FIVE_OF_A_KIND
    | 4::_ -> FOUR_OF_A_KIND
    | 3::2::_ -> FULL_HOUSE
    | 3::_ -> THREE_OF_A_KIND
    | 2::2::_ -> TWO_PAIR
    | 2::_ -> ONE_PAIR
    | 1::_ -> HIGH_CARD
    | f -> failwith (Printf.sprintf "No matching card type %d" (List.hd f))

(* determine strongest possible hand type using present jokers *)
let hand_type_jokers cards =
    let rec aux cards streaks =
        match cards, streaks with
        (* starting "streak" *)
        | c1::rest, [] -> aux rest ((c1, 1)::[])
        | c1::rest, (sc, streak_count)::srest when c1 = sc ->
                (* extend streak *)
                aux rest ((sc, streak_count + 1)::srest)
        | c1::rest, ((sc, streak_count)::_ as srest) when c1 <> sc ->
                (* end/new streak *)
                (* NOTE: still keep ended streak, even if it's a "streak" of 1 *)
                aux rest ((c1, 1)::srest)
        | [], _ -> streaks
    in
    let sorted_cards = List.sort Stdlib.compare cards in
    let matching_cards = aux sorted_cards [] in
    let num_jokers = matching_cards
        |> List.fold_left (fun acc (c, n) -> if c = 'J' then n else acc) 0 in
    (* descending order *)
    let matching_cards_desc = matching_cards
        (* w/o jokers *)
        |> List.filter (fun (c, n) -> if c = 'J' then false else true)
        |> List.map (fun (c, n) -> n)
        |> List.sort (fun i1 i2 -> (Stdlib.compare i1 i2) * -1) in
    match matching_cards_desc, num_jokers with
    | 5::_, _ -> FIVE_OF_A_KIND
    (* only jokers *)
    | [], 5 -> FIVE_OF_A_KIND
    | 4::_, 1 -> FIVE_OF_A_KIND
    | 4::_, 0 -> FOUR_OF_A_KIND
    | 3::2::_, 0 -> FULL_HOUSE
    | 3::_, 2 -> FIVE_OF_A_KIND
    | 3::_, 1 -> FOUR_OF_A_KIND
    | 3::_, 0 -> THREE_OF_A_KIND
    | 2::2::_, 1 -> FULL_HOUSE
    | 2::2::_, 0 -> TWO_PAIR
    | 2::_, 3 -> FIVE_OF_A_KIND
    | 2::_, 2 -> FOUR_OF_A_KIND
    | 2::_, 1 -> THREE_OF_A_KIND
    | 2::_, 0 -> ONE_PAIR
    (* FULL_HOUSE also possible but FIVE_OF_A_KIND worth more *)
    | 1::_, 4 -> FIVE_OF_A_KIND
    | 1::_, 3 -> FOUR_OF_A_KIND
    (* always worth more than TWO_PAIR which would also be possible with this
       constellation *)
    | 1::_, 2 -> THREE_OF_A_KIND
    | 1::_, 1 -> ONE_PAIR
    | _, _ -> HIGH_CARD


let () =
    let lines = read_lines input_file in
    let hands_bids = lines
        |> List.filter (fun l -> (String.length l) > 0)
        |> List.map (fun l ->
            let hand_bid = String.split_on_char ' ' l in
            let hand = hand_bid |> List.hd |> String.to_seq |> List.of_seq in
            let hand_type = hand_type hand in
            let bid = hand_bid |> List.tl |> List.hd in
            hand, hand_type, int_of_string bid
        ) in
    (* sorted in ascending order *)
    let hands_bids_sorted = hands_bids
        |> List.sort (fun (hand1, hand_type1, _) (hand2, hand_type2, _) ->
            compare_hand (hand1, hand_type1) (hand2, hand_type2) compare_card) in
    let part1 hands_bids_sorted =
        let rec aux acc hands_bids rank =
            match hands_bids with
            | [] -> acc
            | (_, _, bid)::rest -> aux (acc + bid * rank) rest (rank + 1)
        in
        aux 0 hands_bids_sorted 1
    in
    Printf.printf "Part1: %d\n" (part1 hands_bids_sorted);
    let hands_bids_pt2 = lines
        |> List.filter (fun l -> (String.length l) > 0)
        |> List.map (fun l ->
            let hand_bid = String.split_on_char ' ' l in
            let hand = hand_bid |> List.hd |> String.to_seq |> List.of_seq in
            let hand_type = hand_type_jokers hand in
            let bid = hand_bid |> List.tl |> List.hd in
            hand, hand_type, int_of_string bid
        ) in
    (* sorted in ascending order *)
    let hands_bids_sorted_pt2 = hands_bids_pt2
        |> List.sort (fun (hand1, hand_type1, _) (hand2, hand_type2, _) ->
            compare_hand (hand1, hand_type1) (hand2, hand_type2) compare_card_pt2) in
    Printf.printf "Part2: %d\n" (part1 hands_bids_sorted_pt2);
