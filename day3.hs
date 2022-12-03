import MyUtils
import Data.List
import Data.Char

priority :: Char -> Int
priority a = (ord a) - (if isUpper a then 38 else 96)

splitHalfs :: String -> (String, String)
splitHalfs as = (take x as, drop x as) where x = (length as) `div` 2

findCommon :: Eq a => [a] -> [a] -> a
findCommon xs ys = filter ((flip elem) ys) xs |> head

part1 :: [String] -> Int
part1 lines = map splitHalfs lines |> tupleMap findCommon |> map priority |> sum

part2 :: [String] -> Int
part2 []                = 0
part2 ((a:as):bs:cs:xs) = if (a `elem` bs) && (a `elem` cs) then (priority a) + (part2 xs) else part2 (as:bs:cs:xs)