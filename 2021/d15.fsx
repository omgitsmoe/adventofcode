open System
open System.IO

let input =
    File.ReadLines "d15.in"
    |> Array.ofSeq
    // != is <> in F#
    |> Array.filter (fun l -> l <> "")

type Pos = int * int
type Node = 
    { riskSum : int64;
      prev : Pos;
      level : int }

let dimx = input.[0].Length
let dimy = input.Length
// F# also has a Char type that the int conversion just turns into an ascii code.......
let digitToInt (c:Char) =
    // 48 is ascii code for 0
    (int c) - 48
// F# does not have immutable arrays, but apparently will soon as "block"
// https://stackoverflow.com/a/68972980
// As to why: Like OCaml, F# is a pragmatic instead of a pure language. This is what some
// people, like me, like the most on these languages.
// In OCaml, arrays are mutable, and F# bridges the OCaml and the .NET CLR
// libaries. It makes perfect sense, to have mutable arrays then.
// TODO: mb test with map? to be a fairer comparison with Haskell's array?
let grid =
    // [| for (y, l) in Seq.zip (seq { 0..dimy }) input do
    //     yield [| for (x, e) in Seq.zip (seq { 0..dimx }) l do
    [| for l in input do
       [| for e in l do
               yield { riskSum = Int64.MaxValue; prev = (-1, -1); level = digitToInt e }
       |]
    |]
let findPath (g:Node array array) (startPos:Pos) (endPos:Pos) =
    // init starting node with a cost of 0
    g.[0].[0] <- { g.[0].[0] with riskSum = 0L }

    let dimx = g.[0].Length
    let dimy = g.Length
    let inRange (x, y) = x >= 0 && x < dimx && y >= 0 && y < dimy  
    // arrays are not resizable so we have to use a list for the queue
    let rec go (q:(Pos * Int64) list) (visited:Set<Pos>) =
        match q with
        | [] -> failwith "Could not find path"
        // if we keep nodes in the queue they might get outdated when a shorter path is found
        | (((x, y), pathCost)::ps) ->
            // F# equality operator is just '='
            if (x, y) = endPos
            // end node reached return total risk level
            then pathCost
            else
                // neighbours we could visit next
                // , COMMA is for TUPLES ; SEMICOLON for separating elments in lists/arrays
                let neighbours = [for p in [(x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y)] do
                                  if inRange p then yield p]
                let newVisited = Set.add (x, y) visited
                let rec insertSorted i l =
                    match l with
                    | [] -> [i]
                    | (y::ys) ->
                        let irs = snd i
                        let crs = snd y
                        if irs < crs
                        then i::y::ys
                        else y :: (insertSorted i ys)
                // either update visited nodes that now would have a lower riskSum
                // or queue them to visit next setting their current riskSum from the current node
                let rec queueOrUpdate q (neighbours:Pos list) (curPos:Pos) =
                    match neighbours with
                    | [] -> q
                    | (pos::ps) ->
                        if Set.contains pos newVisited
                        // visited notes should already have the optimal cost so just skip them
                        then queueOrUpdate q ps curPos
                        // not visited -> update cost and queue
                        else
                            // .[idx] only works for lists/arrays
                            let nx, ny = pos
                            let nn = g.[ny].[nx]
                            let oldCost = nn.riskSum
                            let newCost = pathCost + (int64 nn.level)

                            if oldCost > newCost
                            // new path is cheaper -> update and queue
                            then
                                g.[ny].[nx] <- { nn with prev = pos; riskSum = newCost }
                                queueOrUpdate (insertSorted (pos, newCost) q) ps curPos
                            else queueOrUpdate q ps curPos
                let newQ = queueOrUpdate ps neighbours (x, y)

                go newQ newVisited

    go [(startPos, 0L)] (Set(seq { startPos }))

findPath grid (0, 0) (dimx - 1, dimy - 1)
|> printfn "Part1: %d"


let stopWatch = Diagnostics.Stopwatch.StartNew()
// repeat grid 4 times to the rigth and below, where the risklvl increases every time
// we go to the right or down
let fiveXGrid =
    [| for yy in 0..4 do // for needs to be on sep lines
       for l in input do
        [| for xx in 0..4 do
           for e in l do
           let risk = digitToInt e
           yield {
               riskSum = Int64.MaxValue; prev = (-1, -1);
               // NOTE: level increases every step to the right or down with wrap around >9 to 1
               // -1 to get it into the 0-8 range then mod 9 so we get wrapping
               // add 1 to get it back to 1-9
               level = ((risk - 1 + xx + yy) % 9) + 1
            }
        |]
    |]

#if INTERACTIVE
// Some code that executes only in FSI
printfn "We're interactive OMG"
#endif
// NOTE: dotnet fsi -> F# interactive reads the code and executes it in real time
// DOES NOT COMPILE IT!! -> prob makes no difference since C# only gets "compiled" to the IL
// and is JIT-compiled on run
// tested creating a console project and compiled it: ~12s
// dotnet fsi: ~12s
// so it actually doesn't make a difference!
// (also does not get faster on consecutive runs!)
// NOTE: as long as we time it ourselves here with stopwatch, if we use time or Measure-Command
// provided by the shell then the compilation time is included and then it will make a big
// difference

// python: 1.5s (with binsearch; without 6.7s)
// julia 0.4s with same binsearch as python (own impl); julia 5s (with stdlib bin search 0.5s)
// mikefarquhar's rust solution: 30ms
// toakushi's go solution: 184ms Dijkstra (A* 214ms)
// tpatetl's nodejs solution: 224s
// heyitsmattwade's nodejs solution: 1.5s (using a heap/prioqueue library)
// IT WAS ACTUALLY IMPOSSIBLE TO FIND A JS IMPLEMENTATION THAT DID NOT use
// A LIBRARY!!_)!!_)_!_!__!_!_!! HOW? what's the point of AoC then
// 3/5 used a lib for dijkstra which IS THE WHOLE CODING TASK!?!??!
// can't use binsearch with Haskell since the q is a list and iterating over the list
// is what's slow
// Haskell 728.1910445s
// F#: ~13s (with dotnet fsi d15.fsx command which apparently gets compiled)
// (^ also no binsearch, and using the same algo as Haskell, the ONLY difference is that
//  F# uses the mutable array whereas Haskell uses the immutable Data.Array implementation)
findPath fiveXGrid (0, 0) (5 * dimx - 1, 5 * dimy - 1)
|> printfn "Part2: %d"

stopWatch.Stop()
printfn "%fs %fms" stopWatch.Elapsed.TotalSeconds stopWatch.Elapsed.TotalMilliseconds
