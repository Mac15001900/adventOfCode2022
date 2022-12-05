import MyUtils
import Data.List

parseLine :: String -> ((Int,Int), (Int,Int))
parseLine line = ((a,b), (x,y)) where [[a,b],[x,y]] = splitOn ',' line |> map (splitOn '-') |> map2 read

isFullyContained :: ((Int,Int), (Int,Int)) -> Bool
isFullyContained ((a,b), (x,y)) = (a>=x && b <=y) || (x>=a && y<=b)

isOverlap :: ((Int,Int), (Int,Int)) -> Bool
isOverlap ((a,b), (x,y)) = not $ y<a || x>b

part1 :: [String] -> Int
part1 = count (isFullyContained . parseLine)

part2 :: [String] -> Int
part2 = count (isOverlap . parseLine)