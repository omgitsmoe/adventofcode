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

func isSafe2(line: [Int]) -> Int {
    var order = Order.unknown
    var errCount = 0
    for i in 0..<line.count - 1 {
        let cur = line[i]
        let next = line[i + 1]
        let diff = next - cur
        let currentOrder = if diff > 0 {
            Order.increasing
        } else {
            Order.decreasing
        }

        // so we don't count order/diff double
        var indexHasError = false
        if order == .unknown {
            order = currentOrder
        } else if order != currentOrder {
            indexHasError = true
            // print("\tOrder error i\(i)")
            // first order was not the order of the entire sequence
            // so we have to reset here
            if i == 1 {
                order = .unknown
            }
        }

        let absDiff = abs(diff)
        if absDiff <= 0 || absDiff > 3 {
            indexHasError = true
            // print("\tDiff error i\(i)")
            // first item was wrong so we should ignore the order
            if i == 0 {
                order = .unknown
            }
        }
        if indexHasError {
            errCount += 1
        }
    }

    return errCount
}

var numSafe = 0
var numSafePt2 = 0
for reportLine in reports {
    if isSafe2(line: reportLine) == 0 {
        numSafe += 1
        numSafePt2 += 1
        print("Safe line: \(reportLine)")
        // line already is safe
        continue
    }

    let errCount = isSafe2(line: reportLine)
    if  errCount < 2 {
        numSafePt2 += 1
        print("Safe line pt2: \(reportLine)")
    } else {
        print("Unsafe: ec \(errCount) -> \(reportLine)")
    }
}

print("Part1: \(numSafe)")
print("Part2: \(numSafePt2)")
