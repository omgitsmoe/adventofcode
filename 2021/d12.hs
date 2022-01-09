import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.Char(isUpper)
import Data.Tuple(swap)
import Data.List(intercalate)

import Debug.Trace

import Util

-- record type with two fields label of type String and adjacents of type List of Node
data Node = Node { label :: String
                 , adjacents :: [Node]
                 } deriving (Show)
data Graph = Graph [Node] deriving (Show)

main = do
    contents <- readFile "d12.in"
    let splitToBinTuple [x, y] = (x, [y])
        splitToBinTuple _ = error "More than two values in split!"
        labelToLabel = map (splitToBinTuple . splitOn "-") . lines . trim $ contents
        buildGraph edges = Graph $ map snd nodeLookupList
            -- https://stackoverflow.com/questions/9732084/how-do-you-represent-a-graph-in-haskell
            -- ^ "tying the knot" so we can create cyclic graphs (see bottom of file)
            -- build map from label to adjacent labels
            where
                -- edges go both ways!!!! -> extend list with swapped edges:
                swapEdge (x, [y]) = (y, [x])
                labelToAdjacents = Map.fromListWith (++) (edges ++ (map swapEdge edges))
                -- make node by looking up all the labels of adjacents
                mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
                -- use assoc list to get [(label, adjacent labels)]
                -- this uses mkNode which in turn uses lookupNode which itself uses nodeLookupList
                -- again -> mutually recursive (works due to layz evaluation)
                nodeLookupList = map mkNode (Map.assocs labelToAdjacents)
                -- looks up label in nodeLookupList using:
                -- lookup key assocs = looks up a key in an association list.
                lookupNode lbl = case lookup lbl nodeLookupList of
                    Just n -> n
                    -- this should only be hit on the "end" node
                    Nothing -> Node {label = lbl, adjacents = []}
        graph = buildGraph labelToLabel
        findAllPaths :: Node -> [[String]]
        findAllPaths node = go [(node, [])] []
            where
                go [] paths = paths
                -- go over queue of nodes
                go (((Node lbl adjs), path):ns) paths = case lbl of
                    -- reached end -> found a new path
                    "end" -> go ns ((lbl:path):paths)
                    -- all upper case -> big cave which we can visit more than once
                    -- otherwise small cave which can only be visited once
                    _ -> if (all isUpper lbl) || not (lbl `elem` path)
                        then go newToVisit paths
                        else go ns paths
                    where
                        -- old: access adjacents on node by using adjacents function that was auto created
                        -- by the compiler
                        -- queue adjeacent nodes adding the current node to their path
                        newToVisit = (map (\n -> (n, lbl:path)) adjs) ++ ns
        -- acutal timing: ~1s
        findAllPaths2 :: Node -> [[String]]
        findAllPaths2 node = go [(node, False, [])] []
            where
                go :: [(Node, Bool, [String])] -> [[String]] -> [[String]]
                go [] paths = paths
                -- go over queue of nodes
                go (((Node lbl adjs), visitedSmallTwice, path):ns) paths = case lbl of
                    -- reached end -> found a new path
                    "end" -> go ns ((lbl:path):paths)
                    "start"
                        -- starting state so we are allowed to visit "start" node
                        | null path -> go (newToVisit visitedSmallTwice) paths
                        -- otherwise don't visit start again
                        | otherwise -> go ns paths
                    -- we can visit big caves more than once
                    -- otherwise small cave which can only be visited once
                    _ -> if bigCave then go (newToVisit visitedSmallTwice) paths else
                            case smallCaveVisits of
                                0 -> go (newToVisit visitedSmallTwice) paths
                                1
                                    -- already visited a small cave twice so can't visit this one again
                                    | visitedSmallTwice -> go ns paths
                                    --  first small cave we'll visit twice
                                    | otherwise -> go (newToVisit True) paths
                                -- already visited twice
                                2 -> go ns paths
                    where
                        -- all upper case -> big cave
                        bigCave = all isUpper lbl
                        -- NOTE: bad performance since we need to traverse the whole path list
                        smallCaveVisits = length . filter (== lbl) $ path
                        -- old: access adjacents on node by using adjacents function that was auto created
                        -- by the compiler
                        -- queue adjeacent nodes adding the current node to their path
                        -- mark new paths sucht that we know we've vistied a small cave twice already
                        newToVisit visitedTwice = (map (\n -> (n, visitedTwice, lbl:path)) adjs) ++ ns
        -- try using a map to speed up looking up smallCaveVisits
        -- not too much of a difference (11.5s vs vs 14.5s)
        -- where is the performance bottleneck using the Map??
        -- acutal timing: 0.75s
        findAllPaths3 :: Node -> [[String]]
        findAllPaths3 node = go [(node, Map.empty, [])] []
            where
                go :: [(Node, Map.Map String Int, [String])] -> [[String]] -> [[String]]
                go [] paths = paths
                -- go over queue of nodes
                go (((Node lbl adjs), visitedCounts, path):ns) paths = case lbl of
                    -- reached end -> found a new path
                    "end" -> go ns ((lbl:path):paths)
                    "start"
                        -- starting state so we are allowed to visit "start" node
                        | null path -> go newToVisitBig paths
                        -- otherwise don't visit start again
                        | otherwise -> go ns paths
                    -- we can visit big caves more than once
                    -- otherwise small cave which can only be visited once
                    _ -> if bigCave then go newToVisitBig paths else
                            case smallCaveVisits of
                                0 -> go newToVisitSmall paths
                                1
                                    -- already visited a small cave twice so can't visit this one again
                                    | visitedSmallTwice -> go ns paths
                                    --  first small cave we'll visit twice
                                    | otherwise -> go newToVisitSmall paths
                                -- already visited twice
                                2 -> go ns paths
                    where
                        -- all upper case -> big cave
                        bigCave = all isUpper lbl
                        -- use default of 0 for lookup
                        smallCaveVisits = Map.findWithDefault 0 lbl visitedCounts
                        -- check if we already visited a cave twice
                        visitedSmallTwice = Map.foldr (\x acc -> acc || x == 2) False visitedCounts
                        -- queue adjeacent nodes adding the current node to their path
                        newToVisitBig = (map (\n -> (n, visitedCounts, lbl:path)) adjs) ++ ns
                        newToVisitSmall = (map (\n -> (n, newVisitedCounts, lbl:path)) adjs) ++ ns
                        -- keep track of small cave visits
                        newVisitedCounts = Map.insert lbl (smallCaveVisits + 1) visitedCounts
        -- we only need the number of distinct paths not the paths themselves
        -- -> stop keeping track of them (these allocations were prob a big bottleneck)
        -- use set of seen nodes and check if they've been visited instead of tracking the count
        -- just use a bool for small cave double visit
        -- ~8s
        -- acutal timing: 0.53s
        findAllPaths4 :: Graph -> Int
        -- find startNode with label "start", but don't add it to the set since we only
        -- consider them visited after we called go with that node, so add them after
        findAllPaths4 g = go [(startNode, Set.empty, False)] 0
            where
                startNode = head . filter (\(Node l _) -> l == "start") $ getNodes g
                go :: [(Node, Set.Set String, Bool)] -> Int -> Int
                go [] paths = paths
                -- go over queue of nodes
                go (((Node lbl adjs), visited, visitedSmallTwice):ns) paths = case lbl of
                    -- reached end -> found a new path
                    "end" -> go ns (paths + 1)
                    -- we can visit big caves more than once
                    -- otherwise small cave which can only be visited once
                    _ -> if bigCave then go (newToVisit visited visitedSmallTwice) paths else
                            case seenCave of
                                False -> go (newToVisit (Set.insert lbl visited) visitedSmallTwice) paths
                                True
                                    -- first small cave we'll visit twice
                                    -- we won't visit "start" node twice!
                                    | not visitedSmallTwice && lbl /= "start"
                                        -> go (newToVisit visited True) paths
                                    -- already visited a small cave twice so can't visit this one again
                                    | otherwise -> go ns paths
                    where
                        -- all upper case -> big cave
                        bigCave = all isUpper lbl
                        seenCave = Set.member lbl visited
                        -- queue adjeacent nodes adding the current node to their path
                        newToVisit newVisited dblVisit = (map (\n -> (n, newVisited, dblVisit)) adjs) ++ ns
        -- decide if to visit nodes when we q them
        -- ~7s: woopsie, expected stack/ghc to just cache the compile and re-use that, but it doesn't
        -- compile at all and goes over the script in interpreter mode which is BAAD
        -- actual timing: ~0.48s
        findAllPaths5 :: Graph -> Int
        findAllPaths5 g = go [(startNode, Set.singleton "start", False)] 0 where
            startNode = head . filter (\(Node l _) -> l == "start") $ getNodes g
            go :: [(Node, Set.Set String, Bool)] -> Int -> Int
            go [] paths = paths
            -- go over queue of nodes
            go (((Node lbl adjs), visited, visitedSmallTwice):ns) paths = case lbl of
                -- reached end -> found a new path
                "end" -> go ns (paths + 1)
                _ -> go newToVisit paths
                where
                    -- queue adjeacent nodes adding the current node to their path
                    newToVisit = qCaves adjs ns
                    qCaves [] q = q
                    qCaves (n:ns) q = 
                        -- we can visit big caves more than once
                        -- otherwise small cave which can only be visited once
                        if bigCave then qCaves ns ((n, visited, visitedSmallTwice):q) else
                            case seenCave of
                                False -> qCaves ns ((n, Set.insert lbl visited, visitedSmallTwice):q)
                                True
                                    -- first small cave we'll visit twice
                                    -- we won't visit "start" node twice!
                                    | not visitedSmallTwice && lbl /= "start"
                                        -> qCaves ns ((n, visited, True):q)
                                    -- already visited a small cave twice so can't visit this one again
                                    | otherwise -> qCaves ns q
                        where
                            (Node lbl _) = n
                            -- upper case -> big cave
                            bigCave = isUpper . head $ lbl
                            seenCave = Set.member lbl visited
        getNodes (Graph nodes) = nodes
        -- calling show on a string with newlines prints the newlines in escaped form
        -- intercalate inserts the passed element between each list element and then concats the list
        -- == join
        renderPaths paths = intercalate "\n" . (map (intercalate "," . reverse)) $ paths

    -- select start node and get number of distinct paths
    -- putStrLn $ "Part1: "
    --     ++ (show $ length . findAllPaths . head . filter (\(Node l _) -> l == "start") $ getNodes graph)
    -- putStrLn $ "Part2: "
    --     ++ (show . length . findAllPaths2 . head . filter (\(Node l _) -> l == "start") $ getNodes graph)
    -- putStrLn $ "Part2b: "
    --     ++ (show . length . findAllPaths3 . head . filter (\(Node l _) -> l == "start") $ getNodes graph)
    -- putStrLn $ "Part2c: "
    --     ++ (show . findAllPaths4 $ graph)
    -- TIL compiling a 100loc script can take ~6s and stack runghc is actually running in interpreter mode
    -- instead of compiling and executing the result
    putStrLn $ "Part2d: "
        ++ (show . findAllPaths5 $ graph)


-- https://stackoverflow.com/questions/9732084/how-do-you-represent-a-graph-in-haskell
-- cyclic data in Haskell is constructed by a mechanism called "tying the knot". In practice, it means that we write mutually recursive declarations using let or where clauses, which works because the mutually recursive parts are lazily evaluated.

-- Here's an example graph type:

-- import Data.Maybe (fromJust)

-- data Node a = Node
--     { label    :: a
--     , adjacent :: [Node a]
--     }

-- data Graph a = Graph [Node a]

-- As you can see, we use actual Node references instead of indirection. Here's how to implement a function that constructs the graph from a list of label associations.

-- mkGraph :: Eq a => [(a, [a])] -> Graph a
-- mkGraph links = Graph $ map snd nodeLookupList where

--     mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)

--     nodeLookupList = map mkNode links

--     lookupNode lbl = fromJust $ lookup lbl nodeLookupList

-- We take in a list of (nodeLabel, [adjacentLabel]) pairs and construct the actual Node values via an intermediate lookup-list (which does the actual knot-tying). The trick is that nodeLookupList (which has the type [(a, Node a)]) is constructed using mkNode, which in turn refers back to the nodeLookupList to find the adjacent nodes. 
