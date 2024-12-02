import Foundation

let inputFile = "d02.in"
let contents = try! String(contentsOf: URL(fileURLWithPath: inputFile))
let lines = contents.components(separatedBy: "\n")
    .filter{ $0.trimmingCharacters(in: .whitespacesAndNewlines) != "" }
let reports = lines.map{ $0.components(separatedBy: " ").map{ Int($0)! } }

enum Order {
    case unknown, increasing, decreasing
}

func isSafe(line: [Int], skip: Int) -> Bool {
    var order = Order.unknown
    var safe = true
    // can't just skip the index, since we need to handle the 2nd last
    // index being skipped, which means the item before it needs to be compared
    // to the last item
    // -> just make a copy and remove it -> still plenty fast
    var skippedLine = line
    if skip >= 0 && skip < line.count {
        skippedLine.remove(at: skip)
    }
    for i in 0..<skippedLine.count - 1 {
        let cur = skippedLine[i]
        let next = skippedLine[i + 1]
        let diff = next - cur
        let currentOrder = if diff > 0 {
            Order.increasing
        } else {
            Order.decreasing
        }

        if order == .unknown {
            order = currentOrder
        } else if order != currentOrder {
            safe = false
            break
        }

        let absDiff = abs(diff)
        if absDiff <= 0 || absDiff > 3 {
            safe = false
            break
        }
    }

    return safe
}

var numSafe = 0
var numSafePt2 = 0
for reportLine in reports {
    if isSafe(line: reportLine, skip: reportLine.count + 1) {
        numSafe += 1
        numSafePt2 += 1
        // print("Safe line: \(reportLine)")
        // line already is safe
        continue
    }

    for skipIndex in 0..<reportLine.count {
        if isSafe(line: reportLine, skip: skipIndex) {
            numSafePt2 += 1
            // print("Safe line pt2: \(reportLine)")
            break
        }
    }
}

print("Part1: \(numSafe)")
print("Part2: \(numSafePt2)")
