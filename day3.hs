import MyUtils
import Data.List
import Data.Char

priority :: Char -> Int
priority a 
    | isUpper a = (ord a) - 38
    | otherwise = (ord a) - 96

splitHalfs :: String -> (String, String)
splitHalfs as = (take x as, drop x as) where x = (length as) `div` 2

findCommon :: Eq a => [a] -> [a] -> a
findCommon [] _ = error "No common part"
findCommon (x:xs) ys = if exists (==x) ys then x else findCommon xs ys

part1 :: [String] -> Int
part1 lines = map splitHalfs lines |> tupleMap findCommon |> map priority |> sum

part2 lines = part2' lines 0

part2' :: [String] -> Int -> Int
part2' [] n         = n
part2' ((a:as):bs:cs:xs) n = if (a `elem` bs) && (a `elem` cs) then part2' xs (n+(priority a)) else part2' (as:bs:cs:xs) n