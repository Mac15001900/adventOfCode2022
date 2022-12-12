import MyUtils
import Data.Char

--               Neighbours         Heuristic  Start  isTarget    Cost of shortest path
aStar :: Eq a => (a->[(a, Int)]) -> (a->Int) -> a -> (a->Bool) -> Int
aStar neighbours heuristic start target = aStar' neighbours heuristic [(start, 0, heuristic start)] [] target

--                neighbours        heuristic    frontier          visited  isTarget
aStar' :: Eq a => (a->[(a, Int)]) -> (a->Int) -> [(a, Int, Int)] ->  [a] -> (a->Bool) -> Int
aStar' _  _ []              _  _ = error "Explored everything, no target found"
aStar' ns h (next:frontier) vs t | t (fst3 next)         = snd3 next
                                 | (fst3 next) `elem` vs = aStar' ns h frontier vs t
                                 | otherwise             = aStar' ns h (expand frontier newNodes) ((fst3 next):vs) t where
                                      newNodes = ns (fst3 next) |> filter (\(a,_)-> not (a `elem` vs)) |> map (\(a,c) -> (a, c + snd3 next, h a)) 

expand :: [(a, Int, Int)] -> [(a, Int, Int)] -> [(a, Int, Int)]
expand frontier newNodes = foldl insertNode frontier newNodes

insertNode :: [(a, Int, Int)] -> (a, Int, Int) -> [(a, Int, Int)]
insertNode [] node = [node]
insertNode ((a1,c1,h1):xs) (a2,c2,h2) = if c2+h2 < c1+h1 then (a2,c2,h2):(a1,c1,h1):xs else (a1,c1,h1):(insertNode xs (a2,c2,h2))


type Pos = (Int, Int)
type Node = (Pos, [Pos])
type Map = [[Char]]

addPos :: Pos -> Pos -> Pos
addPos (x1,y1) (x2,y2) = (x1+x2, y1+y2)

inBounds :: Map -> Pos -> Bool
inBounds m (x,y) = x>=0 && y>=0 && x<length (m!!0) && y<length m

getHeight :: Map -> Pos -> Int
getHeight m (x,y) = if c == 'S' then 0 else if c == 'E' then 25 else ord c - ord 'a'   where c = m!!y!!x

createNode :: Map -> Pos -> Node
createNode m p = (p, [(1,0), (0,1), (-1,0), (0,-1)] |> map (addPos p) |> filter (inBounds m) |> filter (\p2-> getHeight m p2 <= getHeight m p + 1))


part1 :: [String] -> Int
part1 lines = aStar ((map (\n-> (createNode lines n, 1))) . snd) (\((x,y),_)-> (abs (tx-x)) + (abs (ty-y))) (createNode lines (findIndex2 (=='S') lines)) ((==(tx,ty)) . fst) where
    (tx,ty) = findIndex2 (=='E') lines

createNode2 :: Map -> Pos -> Node
createNode2 m p = (p, [(1,0), (0,1), (-1,0), (0,-1)] |> map (addPos p) |> filter (inBounds m) |> filter (\p2-> getHeight m p <= getHeight m p2 + 1))

part2 :: [String] -> Int
part2 lines = aStar ((map (\n-> (createNode2 lines n, 1))) . snd) (\_->0) (createNode2 lines (findIndex2 (=='E') lines)) ((==0) . (getHeight lines) . fst)



test = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]