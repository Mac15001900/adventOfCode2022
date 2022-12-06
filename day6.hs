import MyUtils
import Data.List

findUnique :: Int -> String -> Int
findUnique n xs = if length (unique (take n xs)) == n then n else 1 + (findUnique n (tail xs))

part1 :: [String] -> Int
part1 = (findUnique 4) . head

part2 :: [String] -> Int
part2 = (findUnique 14) . head