import MyUtils
import Data.Char

data Instruction = ADDX Int | NOOP deriving (Show, Read, Eq)
type State = (Int, Int, Int, [Instruction]) --Cycle number, time until current operation finishes, register value, instructions left

execute :: State -> Int
execute s@(c, 0, r, [])            =  strength s
execute s@(c, 0, r, ((ADDX x):is)) = (strength s) + execute (c+1, time is, r+x, is)
execute s@(c, 0, r, ((NOOP):is))   = (strength s) + execute (c+1, time is, r,   is)
execute s@(c, n, r, is)            = (strength s) + execute (c+1, n-1,     r,   is)

time :: [Instruction] -> Int
time []           = 0
time (NOOP:_)     = 0
time ((ADDX _):_) = 1

strength :: State -> Int
strength (c, _, r, _) = if c `mod` 40 == 20 then c*r else 0

part1 :: [String] -> Int
part1 lines = map2 toUpper lines |> map read |> (\is-> (1, time is, 1, is)) |> execute

execute2 :: State -> String
execute2 s@(c, 0, r, [])            = (displayChar s):("")
execute2 s@(c, 0, r, ((ADDX x):is)) = (displayChar s):(execute2 (c+1, time is, r+x, is))
execute2 s@(c, 0, r, ((NOOP):is))   = (displayChar s):(execute2 (c+1, time is, r,   is))
execute2 s@(c, n, r, is)            = (displayChar s):(execute2 (c+1, n-1,     r,   is))

displayChar :: State -> Char
displayChar (c, _, r, _) = if abs (((c-1) `mod` 40)-r) <= 1 then '#' else '.'

splitToLines :: String -> String
splitToLines [] = []
splitToLines s  = (take 40 s)++"\n"++(splitToLines (drop 40 s))

part2 :: [String] -> String
part2 lines = map2 toUpper lines |> map read |> (\is-> (1, time is, 1, is)) |> execute2 |> splitToLines

test1 = ["noop", "addx 3", "addx -5"]
test2 = ["addx 15", "addx -11", "addx 6", "addx -3", "addx 5", "addx -1", "addx -8", "addx 13", "addx 4", "noop", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx -35", "addx 1", "addx 24", "addx -19", "addx 1", "addx 16", "addx -11", "noop", "noop", "addx 21", "addx -15", "noop", "noop", "addx -3", "addx 9", "addx 1", "addx -3", "addx 8", "addx 1", "addx 5", "noop", "noop", "noop", "noop", "noop", "addx -36", "noop", "addx 1", "addx 7", "noop", "noop", "noop", "addx 2", "addx 6", "noop", "noop", "noop", "noop", "noop", "addx 1", "noop", "noop", "addx 7", "addx 1", "noop", "addx -13", "addx 13", "addx 7", "noop", "addx 1", "addx -33", "noop", "noop", "noop", "addx 2", "noop", "noop", "noop", "addx 8", "noop", "addx -1", "addx 2", "addx 1", "noop", "addx 17", "addx -9", "addx 1", "addx 1", "addx -3", "addx 11", "noop", "noop", "addx 1", "noop", "addx 1", "noop", "noop", "addx -13", "addx -19", "addx 1", "addx 3", "addx 26", "addx -30", "addx 12", "addx -1", "addx 3", "addx 1", "noop", "noop", "noop", "addx -9", "addx 18", "addx 1", "addx 2", "noop", "noop", "addx 9", "noop", "noop", "noop", "addx -1", "addx 2", "addx -37", "addx 1", "addx 3", "noop", "addx 15", "addx -21", "addx 22", "addx -6", "addx 1", "noop", "addx 2", "addx 1", "noop", "addx -10", "noop", "noop", "addx 20", "addx 1", "addx 2", "addx 2", "addx -6", "addx -11", "noop", "noop", "noop"]