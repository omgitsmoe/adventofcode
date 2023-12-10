import std/sequtils, std/enumerate, std/sequtils
import std/strutils as str
# provides '=>' macro for defining an anonymous function
import sugar

# let -> immutable binding
let
    input_file = "d04_input.txt"
    # input_file = "d04_example.txt"
    # can define multiple
    input = (readFile input_file)

# object type -> managed by gc
# value type as opposed to `ref object`
type 
    Card = object
        id: int
        points: int
        matching_num: int
    # ref version of Card -> easier construction like CardRef(id: 0, ..)
    # otherwise var t: ref Card = new(Card); t.id = 0; ..
    CardRef = ref Card
    # unsafe ptr type
    CardPtr = ptr Card

# method on Card
# parameter without `var` -> immutable
# don't return seq[Card] since Card is a value type; could return seq[ref Card]
# but then then Cards needs to be `ref object`, since you can't get a `ref`
# from an existing value type... -.-
proc adds_cards(self: Card, card_max: int): seq[int] =
    # result special predefined variable that is returned at the end of the function
    for i in (self.id + 1)..(self.id + self.matching_num):
        if i <= card_max:
            result.add(i)

var
    part1 = 0
    # @[] sequence literal
    cards: seq[Card] = @[]

for i, line in enumerate(input.splitLines()):
    if len(line) == 0:
        continue

    # no way to destructure seqs so we use a mutable var here,
    # since rebinding with let doesn't work as well
    var seq =
        # split returns a sequence
        line.split(": ") 
    let card = seq[0]
    seq = seq[1].split('|')
    echo "'$#' '$#'" % [$seq[0], $seq[1]]
    let
        winning = seq[0].split(' ')
            .filter(s => len(s) > 0)
            .map(d => str.parseInt d)
        actual = seq[1].split(' ')
            .filter(s => len(s) > 0)
            .map(d => str.parseInt d)
        # foldl operation needs to be an expression using 2 params
        # a(acc) and b(value);
        # convert bool to int using `.int`
        num_wins = actual.foldl(a + (b in winning).int, 0)
        # shl -> shift left
        card_points = if num_wins > 0: 1 shl (num_wins - 1) else: 0
        
    cards.add(Card(id: i + 1, points: card_points, matching_num: num_wins))

    # $# does $1, $2, .. automatically
    # use $var to convert to string
    # echo "'$#' '$#'" % [$winning, $actual]
    # echo card_points
    part1 += card_points

echo "Part1: $#" % [$part1]

let card_max = len(cards)
# ranges are inclusive in Nim unless specified with 0..<len(cards)
var
    added_per_card = newSeq[int](len(cards))
    # which cards have their added_per_card computed
    computed_cards: seq[int] = @[]

# figure out the number of cards added per card
# starting with cards with 0 points, which add none
for card in cards:
    if card.matching_num != 0:
        continue
    added_per_card[card.id - 1] = 0
    computed_cards.add(card.id)

while len(computed_cards) < len(cards):
    for card in cards:
        if card.id in computed_cards:
            # already computed for this card
            continue
        if not card.adds_cards(card_max)
                .all(proc(id: int): bool = id in computed_cards):
            # not all cards_added known
            continue

        let
            cards_added =
                card.adds_cards(card_max)
            cards_added_num =
                cards_added
                    .map(proc(id: int): int = added_per_card[id - 1])
                    # sum up the cards added, starting with the cards that
                    # were added by this card
                    .foldl(a + b, len(cards_added))

        added_per_card[card.id - 1] = cards_added_num
        computed_cards.add(card.id)

let part2 =
    toSeq(1..len(cards))
        .map(proc(id: int): int = added_per_card[id - 1])
        .foldl(a + b, len(cards))

echo "Part2: $#" % [$part2]

