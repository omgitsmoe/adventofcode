import Foundation

// NOTE: like how you can have separate labels in Swift to pass things by
//       parameter that isn't coupled to the internal name, so
//       func foo(_ internal: Int, kwparam internalKWParam: Int) {}
//       -> here you __must__ pass `internal` as positional argument
//          and __must__ pass `internalKWParam` with the label
//          `kwparam:` -> foo(1, kwparam: 2)
//       => this way you can have the convenience and clarity of kwparams
//          without the downside of internal param name change breaking code
//          (and you can still explicitly break it by changing the label)
let contents = try! String(contentsOf: URL(fileURLWithPath: "d01.in"))
let lines = contents.components(separatedBy: "\n")
    .filter{ $0.trimmingCharacters(in: .whitespacesAndNewlines) != "" }
    .map{ $0.components(separatedBy: " ").filter{ $0 != "" }.map{ Int($0)! } }
// {} for passing lambda (as last? param)
var (list1, list2): ([Int], [Int]) = lines.reduce(([], [])) {
    let e1 = $1[0]
    let e2 = $1[1]
    return (
        // .0 -> tuple access
        // acc is immutable so we create a new array with + op
        $0.0 + [e1],
        $0.1 + [e2]
    )
}
list1.sort()
list2.sort()

var dists: [Int] = []
var dist = 0
for (idx, e1) in list1.enumerated() {
    let e2 = list2[idx]
    let de = abs(e2 - e1)
    dists.append(de)
    dist += de
}
print("Part1: \(dist)")

// NOTE: { counts, word in ... } -> named parameters for lambda instead of
//       default $0 $1
//       dict[key, default: 0] -> access with default equiv: dict[key] ?? 0
let counts = list2.reduce(into: [:]) { counts, word in counts[word, default: 0] += 1 }
let part2 = list1.reduce(into: 0) { acc, el in acc += el * counts[el, default: 0] }
print("Part2: \(part2)")
