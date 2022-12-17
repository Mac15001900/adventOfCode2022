import MyUtils
import Data.List
import Data.Maybe
import qualified Data.HashSet as Set
import Debug.Trace

type Pos = (Int, Int)
type Shape = ([Pos]) --List of coordinates relative to the bottom-left corner (which might not be included itself, like with the + shape)
type Board = Set.HashSet Pos --List of coordinates that are aleardy taken
type Stream = (String, String) --The first string stores the next jets, the latter stores all of them
type State = (Board, Stream, [Shape], Int) --The board, state of jets, shape queue, highest point

shapes :: [Shape]
shapes = [[(0,0),(1,0),(2,0),(3,0)], [(0,1),(1,0),(1,1),(1,2),(2,1)], [(0,0),(1,0),(2,0),(2,1),(2,2)], [(0,0),(0,1),(0,2),(0,3)], [(0,0),(0,1),(1,0),(1,1)]]

fits :: Board -> Shape -> Pos -> Bool --Tests if a shape can exist at a specific position
fits board shape pos = map (addPos pos) shape |> any (\p-> (Set.member p board) || (outOfBounds p)) |> not

outOfBounds :: Pos -> Bool
outOfBounds (x,_) = x<0 || x>=7

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2,y1+y2)

fallDown :: Board -> Stream -> Shape -> Pos -> (Board, Stream, Int)
--fallDown b _ shape pos | trace (showBoard (Set.union (Set.fromList (map (addPos pos) shape)) b) 10 0) False = error "xD"
fallDown b ([],sb) shape pos = fallDown b (sb,sb) shape pos
fallDown b (s:ss,sb) shape pos = if fits b shape shiftedV then fallDown b (ss,sb) shape shiftedV else (Set.union (Set.fromList inserted) b, (ss,sb), map snd inserted |> maximum) where
    tryHShift = addPos pos (streamShift s)
    shiftedH = if fits b shape tryHShift then tryHShift else pos
    shiftedV = addPos (0,-1) shiftedH
    inserted = map (addPos shiftedH) shape

streamShift :: Char -> Pos
streamShift '<' = (-1,0)
streamShift '>' = ( 1,0)

runAll :: State -> Int -> (Board, Int)
runAll (b, _,  _,        m) 0 = (b,m)
runAll (b, st, [],       m) n = runAll (b, st, shapes, m) n
runAll (b, st, (sh:shs), m) n = runAll (b', st', shs, max m m') (n-1) where
    (b',st',m') = fallDown b st sh (2, m+4)

runFor :: [String] -> Int -> Int
runFor input n = runAll (Set.fromList (map (\x-> (x,0)) [0..6]), (head input, head input), shapes, 0) n |> snd

part1 :: [String] -> Int
part1 input = runFor input 2022

findCycle :: State -> [(Int, Int, Int, Board)] -> Int -> (Int, Int, Int) --Start state, list of base cycle endings, shapes placed (cycle start, cycle length, cycle height)
--findCycle s h n | (n `mod` 10000==0) && (traceShow n False) = error ""
findCycle (b, st,       [], m) h n = findCycle (b, st, shapes, m) h n
findCycle (b, (st,stb), sh, m) h n | length st < 10 = case getCycle newEntry h of
        Nothing -> findCycle (b, (st++stb,stb), sh, m) (newEntry:h) n
        Just c -> c
    where
    newEntry = (n, m, length sh, combinations [0..6] [m-40..m] |> filter (flip Set.member b) |> map (mapSnd (subtract m)) |> Set.fromList)
    --newEntry = traceShowId (n, m, length sh, combinations [0..6] [m-40..m] |> filter (flip Set.member b) |> map (mapSnd (subtract m)) |> Set.fromList)
    --newEntry = trace (showBoard b (m+1) (m-20)) (n, m, length sh, combinations [0..6] [m-40..m] |> filter (flip Set.member b) |> map (mapSnd (subtract m)) |> Set.fromList)
findCycle (b, st, (sh:shs), m) h n = findCycle (b', st', shs, max m m') h (n+1) where (b',st',m') = fallDown b st sh (2, m+4)

getCycle :: (Int, Int, Int, Board) -> [(Int, Int, Int, Board)] -> Maybe (Int, Int, Int)
getCycle (n,m,sh,b) h = case find (\(n',m',sh',b')-> b==b' && sh==sh') h of
    Nothing -> Nothing
    Just (n',m',_,_)-> Just (n', n-n', m-m')

part2 :: [String] -> Int
part2 input = ((target-cStart) `div` cLength) * cHeight + extraHeight where
    target = 1000000000000 :: Int
    --target = 2022 :: Int
    start = (Set.fromList (map (\x-> (x,0)) [0..6]), (head input, head input), shapes, 0)
    (cStart, cLength, cHeight) = findCycle start [] 0
    extraHeight = runFor input (cStart + ((target-cStart) `mod` cLength))


test = [">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"]

showBoard :: Board -> Int -> Int -> String
showBoard b n f | n<f       = ""
                | otherwise = (show n)++": |"++(map (\x-> Set.member (x,n) b) [0..6] |> map (\r-> if r then '#' else ' '))++"|\n"++(showBoard b (n-1) f)

runTest :: Int -> IO()
runTest n = putStrLn (showBoard b m (max (m-2500) 0)) where
    (b,m) = runAll (Set.fromList (map (\x-> (x,0)) [0..6]), (head test, head test), shapes, 0) n

{-
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
-}


{-
part2 :: [String] -> Int
part2 input = traceShow [m1, m2, m2', m3] $ m1 + (((target `div` baseCycle)-1) * (m2'-m1)) + (m3-m1) where
    target = 1000000000000 :: Int
    start@(b0, st, sh, m0) = (Set.fromList (map (\x-> (x,0)) [0..6]), (head input, head input), shapes, 0)
    baseCycle = (length $ snd st) * 5
    (b1,m1)   = runAll start            baseCycle
    (b2,m2)   = runAll (b1, st, sh, m1) baseCycle
    (b2',m2') = runAll start (baseCycle*2)
    (b3,m3)   = runAll start ((target `mod` baseCycle) + baseCycle)

--A dumb attempt that does not work
--1595560400379 is too low
--1596472103819 is too high
brokenPart2 :: [String] -> Int
brokenPart2 input = (target `div` cycle) * cycleHeight + (traceShowId (runFor input (target `mod` cycle))) where
    target      = 1000000000000 :: Int
    cycle       = traceShowId $ head input |> length |> (*5)
    cycleHeight = traceShowId $ runFor input cycle

part2 :: [String] -> Int
part2 input = (target `div` cycle) * cycleHeight + (runFor input (target `mod` cycle)) where
    target = 1000000000000 :: Int
    start = (Set.fromList (map (\x-> (x,0)) [0..6]), (head input, head input), shapes, 0)
    (cycle, cycleHeight) = findCycle start

findCycle :: State -> (Int, Int) --Starting state, (cycle length, cycle height)
--findCycle (_,_,_,m) | traceShow m False = error "xD"
findCycle (b,_,_,m) | trace ((showBoard b m (m-10))++"-------") False = error "xD"
findCycle s@(b, (st,sb), sh, m) = if valid b' m' then (baseCycle, m') else mapFst (+baseCycle) (findCycle (b', (st,sb), sh, m')) where
    baseCycle = 5 * (length sb)
    (b', m') = runAll s baseCycle

valid :: Board -> Int -> Bool
--valid b m = [0..2] |> any (\x-> Set.member (x,m) b) |> not
valid b m = (Set.member (1,m) b) && (any (\x-> Set.member (x,m) b) [3..6])


-}