import Foundation

let inputFile = "d03.in"
let contents = try! String(contentsOf: URL(fileURLWithPath: inputFile))

// NOTE: backslashes need to be escaped twice due to \ being used for string interp
let patt = try! Regex("do\\(\\)|don\\'t\\(\\)|mul\\((\\d{1,3}),(\\d{1,3})\\)")
var sum = 0
var sum2 = 0
var enabled = true
for match in contents.matches(of: patt) {
    switch match.output[0].substring! {
    case "do()":
        enabled = true
    case "don't()":
        enabled = false
    default:
        let a = Int(match.output[1].substring!)!
        let b = Int(match.output[2].substring!)!
        sum += a * b
        if enabled {
            sum2 += a*b
        }
    }
}

print("Part1: \(sum)")
print("Part2: \(sum2)")

