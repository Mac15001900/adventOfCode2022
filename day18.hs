import MyUtils
import Data.List
import Data.Maybe
import qualified Data.HashSet as Set

type Pos = (Int, Int, Int)
type Lava = Set.HashSet Pos

parse :: [String] -> Lava
parse input = map ('[':) input |> map (++"]") |> map read |> map (\[a,b,c]-> (a,b,c)) |> Set.fromList

neighbours :: Pos -> [Pos]
neighbours (x,y,z) = map (\(a,b,c)-> (a+x,b+y,c+z)) [(1,0,0), (0,1,0), (0,0,1), (-1,0,0), (0,-1,0), (0,0,-1)]

part1 :: [String] -> Int
part1 input = Set.toList lava |> map neighbours |> concat |> count (\x-> not (Set.member x lava))  where lava = parse input

part2 :: [String] -> Int
part2 input = Set.toList lava |> map neighbours |> concat |> filter (\x-> not (Set.member x lava)) |> count (isOutside lava)
    where lava = parse input

isOutside :: Lava -> Pos -> Bool
isOutside lava start = isJust $ tryAStar (\pos-> neighbours pos |> filter (\x-> not (Set.member x lava)) |> map (\x->(x,1))) (\(x,y,z)-> (abs x)+(abs y)+(abs z)) start (==(0,0,0))

test = ["2,2,2","1,2,2","3,2,2","2,1,2","2,3,2","2,2,1","2,2,3","2,2,4","2,2,6","1,2,5","3,2,5","2,1,5","2,3,5"]