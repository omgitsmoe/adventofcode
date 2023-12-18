import std/sugar, std/strutils, std/sequtils, std/algorithm, std/math, std/tables

type
    HandType = enum
        FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard

let
    input_file = "d07_input.txt"
    # input_file = "d07_example.txt"
    card_to_value = {
        'A': 14,
        'K': 13,
        'Q': 12,
        'J': 11,
        'T': 10,
        '9': 9,
        '8': 8,
        '7': 7,
        '6': 6,
        '5': 5,
        '4': 4,
        '3': 3,
        '2': 2,
    }.newTable
    hand_to_value = {
        FiveOfAKind: 6,
        FourOfAKind: 5,
        FullHouse: 4,
        ThreeOfAKind: 3,
        TwoPair: 2,
        OnePair: 1,
        HighCard: 0,
    }.newTable

proc hand_type(cards: string): HandType =
    # NOTE: not using CountTable so it's "more" comparable to the OCaml solution
    # (OCaml solution does not use a map/table at all, but that wouldn't feel
    #  idiomatic here)
    var card_to_count: Table[char, int]
    for card in cards:
        try:
            card_to_count[card] += 1
        except KeyError:
            card_to_count[card] = 1
    let
        matching_cards_desc = card_to_count.values.toSeq().sorted(SortOrder.Descending)
    if matching_cards_desc[0] == 5:
        result = FiveOfAKind
    elif matching_cards_desc[0] == 4:
        result = FourOfAKind
    elif matching_cards_desc[0] == 3 and matching_cards_desc[1] == 2:
        result = FullHouse
    elif matching_cards_desc[0] == 3:
        result = ThreeOfAKind
    elif matching_cards_desc[0] == 2 and matching_cards_desc[1] == 2:
        result = TwoPair
    elif matching_cards_desc[0] == 2:
        result = OnePair
    else:
        result = HighCard

proc cmp_hand(a: (string, HandType), b: (string, HandType), card_to_value: TableRef[char, int]): int =
    let
        # NOTE: tuple destructuring (a_cards, a_type) = a
        #       while this is declaring multiple values (w/o parens):
        #       a_cards, a_type = a
        (a_cards, a_type) = a
        (b_cards, b_type) = b
    if a_type == b_type:
        for i, a_card in pairs(a_cards):
            let
                b_card = b_cards[i]
                a_val = card_to_value[a_card]
                b_val = card_to_value[b_card]
            if a_val > b_val:
                return 1
            elif a_val < b_val:
                return -1
    else:
        return system.cmp(hand_to_value[a_type], hand_to_value[b_type])

    
let 
    hand_type_bid = (readFile input_file).split('\n')
        .filter(l => len(l) > 0)
        .map(proc (l: string): (string, HandType, int) =
             let
                 words = l.split(' ')
                 cards = words[0]
                 hand = hand_type(cards)
                 bid = words[1].parseInt
             (cards, hand, bid))
    # sorted returns, while .sort is in-place
    hand_type_bid_sorted = hand_type_bid.sorted(proc(
        a: (string, HandType, int),
        b: (string, HandType, int)): int =
            let
                (a_cards, a_type, _) = a
                (b_cards, b_type, _) = b
            return cmp_hand((a_cards, a_type), (b_cards, b_type), card_to_value)
        )

var part1 = 0
for rank_0, (cards, hand_type, bid) in pairs(hand_type_bid_sorted):
    part1 += (rank_0 + 1) * bid

echo "Part1: ", part1

proc hand_type_jokers(cards: string): HandType =
    # NOTE: not using CountTable so it's "more" comparable to the OCaml solution
    # (OCaml solution does not use a map/table at all, but that wouldn't feel
    #  idiomatic here)
    var card_to_count: Table[char, int]
    for card in cards:
        try:
            card_to_count[card] += 1
        except KeyError:
            card_to_count[card] = 1

    let num_jokers = card_to_count.getOrDefault('J', 0)
    if num_jokers == 5:
        return FiveOfAKind
    card_to_count.del('J')

    var matching_cards_desc = card_to_count
        .values
        .toSeq()
        .sorted(SortOrder.Descending)
    var jokers_left = num_jokers
    for count in mitems(matching_cards_desc):
        if jokers_left == 0:
            break
        # try to get the count as close as possible to 5 while at most adding
        # the number of jokers we have to maximize the hand strength
        let dt = min(5 - count, jokers_left)
        count += dt
        jokers_left -= dt

    if matching_cards_desc[0] == 5:
        result = FiveOfAKind
    elif matching_cards_desc[0] == 4:
        result = FourOfAKind
    elif matching_cards_desc[0] == 3 and matching_cards_desc[1] == 2:
        result = FullHouse
    elif matching_cards_desc[0] == 3:
        result = ThreeOfAKind
    elif matching_cards_desc[0] == 2 and matching_cards_desc[1] == 2:
        result = TwoPair
    elif matching_cards_desc[0] == 2:
        result = OnePair
    else:
        result = HighCard


let
    card_to_value_pt2 = {
        'A': 14,
        'K': 13,
        'Q': 12,
        'T': 10,
        '9': 9,
        '8': 8,
        '7': 7,
        '6': 6,
        '5': 5,
        '4': 4,
        '3': 3,
        '2': 2,
        'J': 0,
    }.newTable
    hand_type_bid_pt2 = (readFile input_file).split('\n')
        .filter(l => len(l) > 0)
        .map(proc (l: string): (string, HandType, int) =
             let
                 words = l.split(' ')
                 cards = words[0]
                 hand = hand_type_jokers(cards)
                 bid = words[1].parseInt
             (cards, hand, bid))
    hand_type_bid_sorted_pt2 = hand_type_bid_pt2.sorted(proc(
        a: (string, HandType, int),
        b: (string, HandType, int)): int =
            let
                (a_cards, a_type, _) = a
                (b_cards, b_type, _) = b
            return cmp_hand((a_cards, a_type), (b_cards, b_type), card_to_value_pt2)
        )

var part2 = 0
for rank_0, (cards, hand_type, bid) in pairs(hand_type_bid_sorted_pt2):
    part2 += (rank_0 + 1) * bid

echo "Part2: ", part2
