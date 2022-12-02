import MyUtils
import Data.List

data Shape = Rock | Paper | Scissors deriving (Show, Eq, Read)

parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'X' = Rock 
parseShape 'B' = Paper
parseShape 'Y' = Paper
parseShape 'C' = Scissors
parseShape 'Z' = Scissors

value :: Shape -> Int
value Rock = 1
value Paper = 2
value Scissors = 3

score :: Shape -> Shape -> Int
score a b = (value a) + if a==b then 3 else if wins a b then 6 else 0

wins :: Shape -> Shape -> Bool
wins Rock Scissors  = True
wins Scissors Paper = True
wins Paper Rock     = True
wins _ _            = False

outcome :: String -> Int
outcome line = score (parseShape (last line)) (parseShape (head line))

part1 :: [String] -> Int
part1 lines = map outcome lines |> sum

--What shape should we choose?
reaction :: Shape -> Char -> Shape
reaction a 'Y'        = a
reaction Rock 'X'     = Scissors
reaction Scissors 'X' = Paper
reaction Paper 'X'    = Rock
reaction Rock 'Z'     = Paper
reaction Scissors 'Z' = Rock
reaction Paper 'Z'    = Scissors

outcome2 :: String -> Int
outcome2 line = score (reaction opponent (last line)) opponent where opponent = head line |> parseShape

part2 :: [String] -> Int
part2 lines = map outcome2 lines |> sum

--14888: too high

test = "A Y\n\
\B X\n\
\C Z"