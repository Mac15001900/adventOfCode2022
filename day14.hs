import MyUtils
import Data.List
import Data.Char
import Data.Maybe

type Pos = (Int, Int)
type Cave = [[Bool]] --True is taken (either by wall or sand), False is empty

parseLine :: String -> [Pos] --Creates a list of points from a given line; the list will have duplicates
parseLine line = splitOn ' ' line  |> filter ((/='-') . head) |> map (splitOn ',') |> map2 readInt |> map (\[a,b]->(a,b)) |> pointsBetween

pointsBetween :: [Pos] -> [Pos]
pointsBetween [p]                             = []
pointsBetween ((x1,y1):(x2,y2):xs) | x1 == x2 = (map (\y-> (x1,y)) [min y1 y2 .. max y1 y2]) ++ (pointsBetween ((x2,y2):xs))
                                   | y1 == y2 = (map (\x-> (x,y1)) [min x1 x2 .. max x1 x2]) ++ (pointsBetween ((x2,y2):xs))
buildCave :: [Pos] -> Cave
buildCave points =  foldl (flip (\(x,y)-> setElement2 x y True)) emptyCave points where
    maxY = map snd points |> maximum
    emptyCave = repeat False |> take 1000 |> repeat |> take (maxY+1)

addSand :: Pos -> Cave -> Maybe Cave --Returns Nothing if the sand falls into the abyss, Just new cave otherwise
addSand (x,y) cave | y>=((length cave)-1) = Nothing
                   | otherwise = case newPos of 
                        Nothing  -> Just (setElement2 x y True cave)
                        Just pos -> addSand pos cave 
                    where
                        freeGround = cave!!(y+1) |> drop (x-1) |> take 3 |> zipWithIndexes |> (\[a,b,c]->[b,a,c]) |> dropWhile fst
                        newPos = if freeGround==[] then Nothing else Just ((head freeGround |> snd) + x - 1, y+1)

runAll :: Cave -> Int
runAll cave = case addSand (500,0) cave of
    Nothing -> 0
    Just (newCave) -> 1 + (runAll newCave)

part1 :: [String] -> Int
part1 lines = map parseLine lines |> concat |> buildCave |> runAll

buildCave2 :: [Pos] -> Cave
buildCave2 points =  foldl (flip (\(x,y)-> setElement2 x y True)) emptyCave (points++caveFloor) where
    maxY = map snd points |> maximum
    maxX = map fst points |> maximum
    emptyCave = repeat False |> take 1000 |> repeat |> take (maxY+5)
    caveFloor = pointsBetween [(0, maxY+2), (999, maxY+2)]

runAll2 :: Cave -> Int
runAll2 cave = if cave!!0!!500 then 0 else 1 + (runAll2 (fromJust (addSand (500,0) cave)))

part2 :: [String] -> Int
part2 lines = map parseLine lines |> concat |> buildCave2 |> runAll2


test = ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

showCave :: Cave -> IO()
showCave cave = map2(\b-> if b then '#' else ' ') cave |> joinWith "\n" |> putStrLn