import MyUtils
import Data.List

type Trees = [[Int]]

parse :: [String] -> Trees
parse = map2 (readInt . (:[]))

findVisible :: Trees -> [[Bool]]
findVisible trees = map (\f->f trees) transforms |> map2 (visibleInLine (-1)) |> zip revTransforms |> map (\(t,b)-> t b) |> foldr1 (zipWith (zipWith (||))) where
    transforms = [id, map reverse, transpose, (map reverse) . transpose]
    revTransforms = [id, map reverse, transpose, transpose . (map reverse)]

visibleInLine :: Int -> [Int] -> [Bool]
visibleInLine n [] = []
visibleInLine n (x:xs) = if x > n then True:(visibleInLine x xs) else False:(visibleInLine n xs)

part1 :: [String] -> Int
part1 lines = parse lines |> findVisible |> map (count id) |> sum

countVisible :: Int -> [Int] -> Int
countVisible maxH [] = 0
countVisible maxH (x:xs) = if x >= maxH then 1 else 1+(countVisible maxH xs)

getScore :: Trees -> Int -> Int -> Int
getScore trees x y = [left, right, up, down] |> map (countVisible (trees!!y!!x)) |> product where
    left = trees!!y |> take x |> reverse
    right = trees!!y |> drop (x+1)
    up = transpose trees |> (!!x) |> drop (y+1)
    down = transpose trees |> (!!x) |> take y |> reverse

part2 :: [String] -> Int
part2 lines = map (\y-> map(\x-> getScore (parse lines) x y) [0..(length (lines!!0))-1]) [0..(length lines)-1] |> map maximum |> maximum

test = ["30373", "25512", "65332", "33549", "35390"]
test2 = [[3,3],[4,9],[3],[3,5,3]] :: [[Int]]