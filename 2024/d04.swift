import Foundation

let inputFile = "d04.in"
let contents = try! String(contentsOf: URL(fileURLWithPath: inputFile))
let lines = contents.components(separatedBy: "\n").filter{$0.count > 0}.map{$0.map{$0}}

var counter = 0
var x = 0
var y = 0

func check(_ row:Int, _ col: Int, _ row_op: Int, _ col_op: Int, _ content: Character) -> Bool{
    let new_row = row + row_op
    let new_col = col + col_op
    if new_row < 0 || new_row >= lines.count {
        return false
    }
    let row_len = lines[new_row].count
    if new_col < 0 || new_col >= row_len {
        return false
    }

    let new_val = lines[new_row][new_col]
    switch (content, new_val) {
    case ("X", "M"):
        return check(new_row, new_col, row_op, col_op, new_val)
    case ("M", "A"):
        return check(new_row, new_col, row_op, col_op, new_val)
    case ("A", "S"):
        return true
    default:
        return false
    }
}    

func check2(_ row:Int, _ col: Int) -> Bool {
    let chars = (
        // top left
        lines[row - 1][col - 1],
        lines[row - 1][col + 1],
        // bottom left
        lines[row + 1][col - 1],
        lines[row + 1][col + 1]
    )

    switch chars {
    case ("M", "M", "S", "S"),
         ("M", "S", "M", "S"),
         ("S", "M", "S", "M"),
         ("S", "S", "M", "M"):
        return true
    default:
        return false
    }
}

for (row, row_content) in lines.enumerated(){
    for (col, content) in row_content.enumerated(){
        if content == "X"{
            for col_op in [-1, 0, 1]{
                for row_op in [-1, 0, 1]{
                    if check(row, col, row_op, col_op, content){
                        counter += 1
                    }
                }
            }
        }
    }
}

var counter2 = 0
for (row, row_content) in lines[1..<lines.count - 1].enumerated(){
    let row = row + 1
    for (col, content) in row_content[1..<row_content.count - 1].enumerated(){
        let col = col + 1
        if content == "A" {
            if check2(row, col) {
                counter2 += 1
            }
        }
    }
}

print("Part1: \(counter)")
print("Part2: \(counter2)")

