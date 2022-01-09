import System.IO
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.List(foldl')
import Data.Char(digitToInt)

import Data.Time

import Util

type Pos = (Int, Int)
data Node = Node
    { riskSum :: Int
    , prev :: Pos
    , level :: Int
    }

main = do
    contents <- readFile "d15.in"
    let input = lines $ trim contents
        dimx = length (head input)
        dimy = length input
        grid =
            Array.array ((0, 0), (dimx - 1, dimy - 1))
                [((x, y),
                  Node { riskSum = maxBound :: Int, prev = (-1, -1), level = digitToInt e }) |
                  (y, l) <- zip [0..] input, (x, e) <- zip [0..] l]
        findPath g pos end =
            go g' ((pos, 0):[]) (Set.fromList [pos]) where
                -- init starting node with a cost of 0
                g' = g Array.// [((0, 0), (g Array.! (0, 0)) { riskSum = 0 })]
                inRange = Array.inRange (Array.bounds g')
                go :: Array.Array Pos Node -> [(Pos, Int)] -> Set.Set Pos -> Int
                go g [] visited = error "Could not find path"
                -- if we keep nodes in the queue they might get outdated when a shorter path is found
                go g (((x, y), pathCost):ps) visited =
                    if (x, y) == end
                    -- end node reached return total risk level
                    then pathCost --riskSum (g Array.! end)
                    else go newG newQ newVisited
                    where
                        -- neighbours we could visit next
                        neighbours = [p | p <- [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)], inRange p]
                        (newG, newQ) = queueOrUpdate g visited ps neighbours
                        newVisited = Set.insert (x, y) visited
                        -- either update visited nodes that now would have a lower riskSum
                        -- or queue them to visit next setting their current riskSum from the current node
                        queueOrUpdate :: Array.Array Pos Node -> Set.Set Pos -> [(Pos, Int)]
                            -> [Pos] -> (Array.Array Pos Node, [(Pos, Int)])
                        queueOrUpdate g v q [] = (g, q)
                        -- use as-pattern syntax (name@(pattern) pattern matches on the pattern as well
                        -- as binding the whole matched structure to name) to match on the record fields
                        -- AND bind the whole record to a name "nn"
                        -- queueOrUpdate g v q ((pos, nn@(Node { riskSum = rs, level = nlvl })):ns) =
                        queueOrUpdate g v q (pos:ps) =
                            if Set.member pos visited
                            -- visited notes should already have the optimal cost so just skip them
                            then queueOrUpdate g v q ps
                            -- not visited -> update cost and queue
                            else
                                if oldCost > newCost
                                -- new path is cheaper -> update and queue
                                then queueOrUpdate gridUpdated v (insertSorted (pos, newCost) q) ps
                                -- NOTE: part1 worked on 2nd run (after fixing compile errors only 1 run where
                                -- I forgot to update the node in the grid if it wasn't visited below)
                                -- UPDATE1: it only worked for the example since we carried around outdated node
                                -- instances (due to immutability they won't be updated)
                                -- UPDATE2: only was updating costs on visited nodes which by the design of the
                                -- algo should already have the optimal costs, now update non-visited nodes
                                -- and queue them if cost is smaller (since initial cost is maxInt this works
                                -- for non-queued as well); this works since we only want to get to our target
                                -- (it would not work if we wanted the shortest paths to all nodes in the grid)
                                -- NOTE: technically should also check if queued already and update it in the q
                                -- queue item where position is based on riskSum
                                else queueOrUpdate g v q ps
                            where nn = g Array.! pos
                                  oldCost = riskSum nn
                                  nlvl = level nn
                                  newCost = pathCost + nlvl
                                  nodeUpdated = nn { prev = (x, y), riskSum = newCost }
                                  gridUpdated = (g Array.// [(pos, nodeUpdated)])
                        insertSorted i [] = [i]
                        insertSorted i (y:ys) =
                            let irs = snd i
                                crs = snd y in
                            if irs < crs
                            then i:y:ys
                            else y : (insertSorted i ys)

    -- repeat grid 4 times to the rigth and below, where the risklvl increases every time
    -- we go to the right or down
    start <- getCurrentTime
    let fiveXGrid =
            Array.array ((0, 0), (5 * dimx - 1, 5 * dimy - 1))
                [((x + dimx * xx, y + dimy * yy), --((r - 1 + xx + yy) `mod` 9) + 1) |
                  Node { riskSum = maxBound :: Int, prev = (-1, -1)
                       -- NOTE: level increases every step to the right or down with wrap around >9 to 1
                       -- -1 to get it into the 0-8 range then mod 9 so we get wrapping
                       -- add 1 to get it back to 1-9
                       , level = ((r - 1 + xx + yy) `mod` 9) + 1 }) |
                  (y, l) <- zip [0..] input, (x, e) <- zip [0..] l, let r = digitToInt e,
                  xx <- [0..4], yy <- [0..4]]
        printArray arr =
            unlines [unwords [show (arr Array.! (x, y)) | x <- [10..19]] | y <- [0..9]]



    -- putStrLn $ show $ findPath grid (0, 0) (dimx - 1, dimy - 1)
    -- putStrLn $ printArray fiveXGrid
    -- COMPILED version takes 728.1910445s WOW
    putStrLn $ show $ findPath fiveXGrid (0, 0) (5 * dimx - 1, 5 * dimy - 1)
    stop <- getCurrentTime
    print $ diffUTCTime stop start
    -- python: 1.5s (with binsearch; without 6.7s)
    -- julia 0.4s with same binsearch as python (own impl); julia 5s (with stdlib bin search 0.5s)
    -- mikefarquhar's rust solution: 30ms
    -- toakushi's go solution: 184ms Dijkstra (A* 214ms)
    -- tpatetl's nodejs solution: 224s
    -- heyitsmattwade's nodejs solution: 1.5s (using a heap/prioqueue library)
    -- IT WAS ACTUALLY IMPOSSIBLE TO FIND A JS IMPLEMENTATION THAT DID NOT use
    -- A LIBRARY!!_)!!_)_!_!__!_!_!! HOW? what's the point of AoC then
    -- 3/5 used a lib for dijkstra which IS THE WHOLE CODING TASK!?!??!
    -- can't use binsearch with Haskell since the q is a list and iterating over the list
    -- is what's slow
    -- Haskell 728.1910445s
    -- F#: ~13s (with dotnet fsi d15.fsx command which apparently gets compiled)
    -- (^ also no binsearch, and using the same algo as Haskell, the ONLY difference is that
    --  F# uses the mutable array whereas Haskell uses the immutable Data.Array implementation)