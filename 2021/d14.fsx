open System
open System.IO

let input = File.ReadLines "d14.in"
let polymer =
    input
    |> Seq.head
    // convert to char list, so we can insert more freely later
    |> Seq.toList
let rules =
    input
    |> Seq.skip 2
    |> Seq.map (fun l ->
        match l.Split([|" -> "|], StringSplitOptions.RemoveEmptyEntries) with
            | [|pair; result|] -> (pair |> Seq.toList, Seq.head result)
            | _ -> failwith "Unexpected rule format")
    |> Map.ofSeq
let applyRules (p:char list) =
    // helper function so a user doesn't have to pass the accumulator
    let rec go p acc =
        // match and | have to match up (column wise)
        match p with
        // match on two chars
        | a::b::tail ->
            match Map.tryFind (a::b::[]) rules with
            // build resulting string with inserts in reverse
            // don't add b yet since it might be used in the next pair
            | Some insert -> go (b::tail) (insert::a::acc)
            | None -> go (b::tail) (a::acc)
        // add last remaining char to modified polymer
        | a::tail -> go tail (a::acc)
        | _ -> acc |> List.rev
    
    go p []
// F# doesn't have an iterate/apply function like haskell
let rec iterate f times acc =
    match times with
    | 0 -> acc
    | _ -> iterate f (times - 1) (f acc)
let countChars chars =
    let rec go chars (acc:Map<char, int>) =
        match chars with
        // Map.change: change function where key gets replace whatever the function returns
        | c::tail -> go tail (Map.change c (fun x ->
            match x with
            | Some x -> Some (x + 1)
            | None -> Some 1) acc)
        | [] -> acc
    go chars Map.empty

// generic type: 'b -> runtime polymorphism using .NET
// ^b -> compile time generics using F# compiler only
// there's no generic number type apparently??
// so we have to use a fixed int64 type.... what's the type of having generics then!??!
let getLeasAndMostFrequent (map:Map<char,int64>) =
    map
    |> Map.fold (fun acc _ v ->
        let minv, maxv = acc
        let newMin = if v < minv then v else minv
        let newMax = if v > maxv then v else maxv
        (newMin, newMax)
        ) (Int64.MaxValue, Int64.MinValue)


// let least, most = // line below would not work on this line
//     iterate applyRules 10 polymer
//     |> countChars
//     |> Map.map (fun _ v -> int64 v)
//     |> getLeasAndMostFrequent

// printfn "Part1: %d" (most - least)

// instead of building up the charlist step by step
// just keep a queue of char pairs that can be checked against the rules
// and track the amount of inserted chars
let applyRules2 (p:char list) times =
    // queue pairs in p
    let rec queue p times acc =
        match p with
        | a::b::tail -> queue (b::tail) times (((a, b), times)::acc)
        | _ -> acc
    // helper function so a user doesn't have to pass the accumulator
    let rec go q acc =
        // match and | have to match up (column wise)
        match q with
        // match on two chars
        | ((a, b), times)::tail ->
            // for stopping at a specific step
            if times > 0 then
                match Map.tryFind (a::b::[]) rules with
                | Some insert ->
                    // queue new resulting pairs
                    let newQ = ((a, insert), times - 1)::((insert, b), times - 1)::tail
                    // add 1 to inserted char in map
                    let newAcc = Map.change insert (fun x ->
                        match x with
                        | Some x -> Some (x + 1)
                        | None -> Some 1) acc
                    go newQ newAcc
                | None -> go tail acc
            else go tail acc
        | _ -> acc

    go (queue p times []) (countChars p)

// merge two maps adding the values
let mergeMap (map1:Map<char,int>) (map2:Map<char,int>) =
    Map.fold (fun acc k v -> // this IS AN ERROR??!?! Map.change k (fun x ->
        // but this is fine!??!
        Map.change k (fun x ->
            match x with
            // Some (v + x): int, but this is int option: Some v + x
            // OMGOMGOMG and the compiler doesn't point me to that error
            // but to the map1 param saying the type is wrong
            | Some x -> Some (v + x)
            | None -> Some v)
            acc) map1 map2

// DOESNT WORK still too many iterations even though memory stays roughly the same
// run the pairs in parallel
// [for p in (polymer |> applyRules |> applyRules |> pList) do yield async { return applyRules2 p 40 }]
// |> Async.Parallel
// |> Async.RunSynchronously
// |> Array.fold (fun acc x -> mergeMap acc x) Map.empty
// |> getLeasAndMostFrequent
// |> (fun (min, max) -> printfn "Part2: %d" (max - min))

// transform rulesmap so we get resulting 2 pairs from the src pair
let rulesMap =
    rules
    |> Map.fold (fun acc k insert ->
        let src = (List.head k, List.last k)
        Map.add src ((List.head k, insert), (insert, List.last k)) acc
        ) Map.empty
// keep a paircount map and add new pairs from the rules based on the count in the map
let applyRules3 (p:char list) times =
    // initial map from pair to count
    let pairsMap =
        Seq.zip p (p |> Seq.skip 1)
        // fold over pair list, incrementing by one in the acc each time we encounter a pair
        // use int64 (1L = int64 literal for 1) so we don't overflow, since example is over 4billion
        |> Seq.fold (fun acc pair -> Map.change pair (fun x ->
            Some ((Option.defaultValue 0L x) + 1L)) acc) Map.empty
    // helper function so a user doesn't have to pass the accumulator
    let rec go pairs times =
        if times > 0
        then
            let newPairs =
                // fold over the rules so we only need to check if the respective pair
                // is present in the pairs/acc Map
                // (could also fold over pairs and check if there's a matching rule, but
                //  that would result in unnecessary lookups)
                rulesMap
                |> Map.fold (fun acc k (p1, p2) ->
                    // check if the src rule is present in pairs
                    // add the two resulting pairs with the count found for the src pair
                    match Map.tryFind k pairs with
                    | Some count ->
                        acc
                        // the src pair got transformed into the two resulting pairs above
                        // so we decrement the src pair count
                        |> Map.change k  (fun x -> Some ((Option.get x) - count))
                        // use defaultValue so we don't have to match and just use 0 as default
                        // increment resulting pairs by src pair count
                        // change needs an Option so we need to wrap the result in Some
                        |> Map.change p1 (fun x -> Some ((Option.defaultValue 0L x) + count)) 
                        |> Map.change p2 (fun x -> Some ((Option.defaultValue 0L x) + count)) 
                    // use pairs as acc so we can add/change values in the rulesMap fold
                    | None -> acc) pairs 
            // recurse
            go newPairs (times - 1)
        else pairs

    go pairsMap times

// m
// // to print every element of a list/seq/..
// |> Seq.iter (printf "%A; ")
// printfn "\n"

let stopWatch = Diagnostics.Stopwatch.StartNew()
applyRules3 polymer 40
// ^ returns map of Pair->PairCount which we need to transform to Char->CharCount
// NOTE: !! we have to account for pairs REUSING one of the chars of a pair
|> Map.fold (fun acc k count ->
    match k with
    | (a, b) ->
        // add count for the two chars in pair to charCount in acc
        acc
        |> Map.change a (fun x -> Some ((Option.defaultValue 0L x) + count))
        |> Map.change b (fun x -> Some ((Option.defaultValue 0L x) + count))
    ) Map.empty
// one char gets reused between pairs so use count/2+0.5, see below for example
// +0.5 so we round up when converting back to int
|> Map.map (fun _ count -> ((float count) / 2.0 + 0.5) |> int64)
|> getLeasAndMostFrequent
|> (fun (min, max) -> printfn "Part2: %d" (max - min))

// python and julia are fastest between python, julia and F#
// python: 2ms julia: 2ms (applyrules functional reduce 50ms) F#: 27ms
stopWatch.Stop()
printfn "%fms" stopWatch.Elapsed.TotalMilliseconds

// step2:
// NBCCNBBBCBHCB
//
// NB 2
// BC 2
// CN 1
// BB 2
// CB 2
// BH 1
// HC 1
//
// N 3
// B 11
// C 6
// H 2
//
// actual:
// N 2
// B 6
// C 4
// H 1
