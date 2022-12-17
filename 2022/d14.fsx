// using namespace (in the C# sense) System.IO so we can use File.ReadLines instead of
// System.IO.File.ReadLines
open System
open System.IO

// NOTE: to compile to exe: fsc.exe -r System.Xml --target:exe d14.fsx
// ~9s F# compiled vs C++ -O2 ~370ms

let occupied =
    // File.ReadLines will be a Sequence (F# lazy list equivalent) and C#'s IEnumerable pendant
    File.ReadLines "d14.in"
    |> Seq.takeWhile ((<>) "")
    // anonymous function: fun param1 param2 -> function body
    // Split is (by default) only available as a method on string (since F# is not pure functional
    // and uses .NET API)
    // Split expects an array so use array literal e.g. [| elem1; elem2 |]
    |> Seq.map (fun line ->
        // if let need more than 1 lines no part of it can be on the line with the let
        // let path_midpoints = line.Split([| " -> " |])
        //      |> won't work since path_midpoints is just line.Split...
        let path_midpoints =
            // needs the 2nd argument for options to use the split by string instead of char
            line.Split([|" -> "|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun coord ->
                let xy = coord.Split ','
                (int xy.[0], int xy.[1]))
            // can't match on sequences so we need to return a list
            |> Seq.toList
        let pathFromPoints (points:(int * int) * (int * int)) : (int * int) seq =
            match points with
            | ((x1, y1), (x2, y2)) ->
                let low_x = if x1 < x2 then x1 else x2
                let low_y = if y1 < y2 then y1 else y2
                let high_x = if x1 > x2 then x1 else x2
                let high_y = if y1 > y2 then y1 else y2

                seq {for y in low_y..high_y do
                     for x in low_x..high_x do
                     yield (x, y)}
        let rec makePaths points (allPoints:(int * int) seq) =
            match points with
            | []-> failwith "Empty points"
            | [_] -> failwith "Unused point"
            // append lists with @ (infix for List.append)
            // or better use seq for more perf
            | [p1; p2] -> Seq.append (pathFromPoints (p1, p2)) allPoints
            // p2 is needed again for next path
            | p1::p2::tail ->
                // NOTE: forgot to pass on allPoints here; that never would've happend in imperative code
                // the lack of an append or whatever would've been so apparent
                // (just passed on pathFrompoints result)
                // mb it was due to the very noisy ionide type hints, that I now turned off
                // the line wasn't completely visible in the editor, mb that's why i missed it
                // <| backward pipe passes result of rhs to lhs, similar to $ in haskell
                makePaths (p2::tail) <| Seq. append (pathFromPoints (p1, p2)) allPoints

        makePaths path_midpoints Seq.empty)
    // flatten
    |> Seq.concat
    |> Set.ofSeq

let (xmin, xmax, ymax) =
    occupied
    |> Set.fold (fun acc (x, y) ->
        let (minx, mx, my) = acc
        let yy = if y > my then y else my
        let xx = if x > mx then x else mx
        let minxx = if x < minx then x else minx
        (minxx, xx, yy)) (Int32.MaxValue, 0, 0)
// occupied
// |> Set.iter (fun x -> printf "%A" x)
printfn "ymax: %A xmax: %A" ymax xmax
let sim_sand occupied =
    let rec step loc sandUnits occupied =
        // printfn "%A" loc
        match loc with
        | (x, y) when y >= ymax -> sandUnits, occupied
        // down
        | (x, y) when not (Set.contains (x, y + 1) occupied) ->
            step (x, y + 1) sandUnits occupied
        // down+left
        | (x, y) when not (Set.contains (x - 1, y + 1) occupied) ->
            step (x - 1, y + 1) sandUnits occupied
        // down+right
        | (x, y) when not (Set.contains (x + 1, y + 1) occupied) ->
            step (x + 1, y + 1) sandUnits occupied
        | (x, y) ->
            // can't go anywhere else -> settle here
            // and start with next sand
            // printfn "settle %A" loc
            step (500, 0) (sandUnits + 1) (Set.add (x, y) occupied)
    step (500, 0) 0 occupied

let printCave xmin max occupied =
    match max with
    | (mx, my) ->
        seq {0..my}
        |> Seq.iter (fun y ->
            seq {xmin..mx}
            |> Seq.iter (fun x ->
                match Set.contains (x, y) occupied with
                | true -> printf "#"
                | false -> printf ".")
            printf "\n")

// printCave xmin (xmax, ymax) occupied
let part1, filled = sim_sand occupied
// printCave xmin (xmax, ymax) filled
printfn "Part1: Units of sand that came to rest before the abyss: %d" part1
        
let floor = ymax + 2
let sim_sand2 occupied =
    let rec step loc sandUnits occupied =
        // printfn "%A" loc
        match loc with
        | (x, y) when (y + 1) = floor ->
            // come to rest at cave floor
            step (500, 0) (sandUnits + 1) (Set.add (x, y) occupied)
        // down
        | (x, y) when not (Set.contains (x, y + 1) occupied) ->
            step (x, y + 1) sandUnits occupied
        // down+left
        | (x, y) when not (Set.contains (x - 1, y + 1) occupied) ->
            step (x - 1, y + 1) sandUnits occupied
        // down+right
        | (x, y) when not (Set.contains (x + 1, y + 1) occupied) ->
            step (x + 1, y + 1) sandUnits occupied
        | (x, y) ->
            // reached src
            if x = 500 && y = 0 then sandUnits + 1
            else
                // can't go anywhere else -> settle here
                // and start with next sand
                // printfn "settle %A" loc
                step (500, 0) (sandUnits + 1) (Set.add (x, y) occupied)
    step (500, 0) 0 occupied

sim_sand2 occupied
|> printfn "Part2: Units of sand that came to rest before reaching the src: %d"