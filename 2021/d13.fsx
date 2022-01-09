// using namespace (in the C# sense) System.IO so we can use File.ReadLines instead of
// System.IO.File.ReadLines
open System
open System.IO

// you don't need an entry point for F# script files (.fsx)
// Scripts use the file extension .fsx. Instead of compiling source code and
// then later running the compiled assembly, you can just run dotnet fsi and
// specify the filename of the script of F# source code, and F# interactive
// reads the code and executes it in real time
// -> so same as "stack runghc .."
// [<EntryPoint>]
// let main argv =
//     // printfn uses string formatting specifiers like %d/%i for integers %f for float and %s for string
//     printfn "Hello %s" "World"
//     // return 0 as exit code; in F# the last line of a function is what's being returned
//     0

let coords, folds =
    // File.ReadLines will be a Sequence (F# lazy list equivalent) and C#'s IEnumerable pendant
    let input = File.ReadLines "d13.in"
    // this indentation is a new scope so coords/folds is fine to use
    let coords =
        input
        // |> is the F# pipe operator
        // takeWhile consumes from the sequence until the predicate fails
        // we consume from the seq while we don't encounter an empty string
        // <> not equals operator
        |> Seq.takeWhile ((<>) "")
        // anonymous function: fun param1 param2 -> function body
        // Split is (by default) only available as a method on string (since F# is not pure functional
        // and uses .NET API)
        // Split expects an array so use array literal e.g. [| elem1; elem2 |]
        |> Seq.map (fun line ->
            let xy = line.Split([| ',' |])
            // convert string to int with "int" and return (x, y) tuple
            (int xy.[0], int xy.[1]))
        // make an array from a Sequence
        // use an array since list element access would be O(n) (list = singly linked list)
        |> Array.ofSeq

    let folds =
        input
        // input is immutable so a seq does not behave like a generator in python would
        // -> need to skip the elements we consumed for coords
        |> Seq.skip (coords.Length + 1)
        |> Seq.map (fun (line:string) ->
            // remove "fold along "
            let foldDirCoord = line.Substring(11).Split([| '=' |])
            // fold direction, fold coord
            (foldDirCoord.[0], int foldDirCoord.[1]))
        |> List.ofSeq

    (coords, folds)

let dots =
    coords
    |> Set.ofArray

// tuple type in F# is: (int, int) = int * int
let fold (foldInstr:string * int) (dots:Set<int * int>) =
    let direction, foldCoord = foldInstr
    // helper functions to get the coord we need to modify and to assaemble the tuple
    // in the correct x,y order based on direction
    // pattern match
    let (getDir, mkPoint) =
        match direction with
        // get x value from tuple, assemble tuple
        | "x" -> (fst, fun d x -> (x, snd d))
        | "y" -> (snd, fun d y -> (fst d, y))
        | _ -> failwith "Direction must be one of 'y' or 'x'"

    dots
    |> Set.map (fun dot ->
        let v = getDir dot
        // part of the paper the gets folded onto -> keep
        if v < foldCoord then dot
        // new y coord = fold line y - (distance from the fold line)
        // same for x
        else mkPoint dot (foldCoord - (v - foldCoord)))
            

// %A formats an arbitrary type
// printfn "Coords: %A\nFolds: %A" coords folds
fold folds.[0] dots
|> Set.count
|> printfn "Part1: %A"

// we have to mark recursive functions in F#
// apply all the folds in the list iteratively
let rec doFolds folds dots =
    match folds with
    | [] -> dots
    // :: is cons operator (: in haskell)
    // so we split the list in head and tail like this
    // tail-recursive (last instruction in a function is the recursive call) 
    // -> no new stack frame needed
    // (even just adding the result of doFolds to an intermediate would make this
    //  non tail-recursive)
    // you can be more care-free with this in haskell due to lazy eval and thunks
    | head :: tail -> doFolds tail (fold head dots)
// render the marked dots in a grid
let renderDots (dots:Set<int * int>) =
    // minmax function that uses a compare function, starter value and a
    // getter so we can curry these params to make minx maxx etc.
    let minmax cmp starter getter (ds:Set<int * int>) =
        ds
        |> Set.fold (fun (acc:int) d ->
            if cmp (getter d) acc then (getter d) else acc) starter
    let min = minmax (<) 100000
    let max = minmax (>) -1
    let minX = min fst dots
    let maxX = max fst dots
    let minY = min snd dots
    let maxY = max snd dots
    // nested array comprehension
    // let's do the rendering in an imperative way by using a mutable grid
    let mutable grid = [| for _ in minY..maxY -> [| for _ in minX..maxX -> "." |] |]
    // dots is an array so we could use a for loop, but that isn't functional
    // so we can use recursion coupled with slicing:
    dots
    // iter lets us perform an imperative action for every element of a collection
    |> Set.iter (fun (x, y) ->
        // assign element in mutable nested array
        grid.[y].[x] <- "#"
        // imperative 'function' has to return unit ()
        ())
    
    grid
    // concat chars in line
    |> Array.map (String.concat "")
    // concat lines
    |> String.concat "\n"
        
    

let stopWatch = Diagnostics.Stopwatch.StartNew()
doFolds folds dots
|> renderDots
|> printfn "Part2:\n%s"
stopWatch.Stop()
printfn "%fms" stopWatch.Elapsed.TotalMilliseconds
// F# ~15ms for part2
// Haskell: ~3ms (ghci or compiled same speed) for part2