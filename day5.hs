import MyUtils
import Data.List

type Crates = [[Char]]
type Instruction = (Int, Int, Int) --Amount of repetitions, source, target

parseCrates :: [String] -> Crates
parseCrates lines = init lines |> map removeSpacing |> transpose |> map (dropWhile (==' '))

removeSpacing :: String -> String
removeSpacing [_, x, _]    = [x]
removeSpacing (_:x:_:_:xs) = x:(removeSpacing xs)

parseInstruction :: String -> Instruction
parseInstruction line = (read (words!!1), read (words!!3), read (words!!5)) where words = splitOn ' ' line

executeInstruction :: Instruction -> Crates -> Crates
executeInstruction (n,s,t) = repeatF n (move (s-1) (t-1))

move :: Int -> Int -> Crates -> Crates
move source target crates = changeElement source tail crates |> changeElement target (crate:) where crate = head $ crates!!source

executeAll :: (Instruction -> Crates -> Crates) -> [String] -> String
executeAll f lines = foldl (flip f) crates instructions |> map head  where 
    [cratePart, instructionPart] = splitOn "" lines
    crates = parseCrates cratePart
    instructions = map parseInstruction instructionPart

part1 :: [String] -> String
part1 = executeAll executeInstruction

executeInstruction' :: Instruction -> Crates -> Crates
executeInstruction' (n,s,t) crates = changeElement (s-1) (drop n) crates |> changeElement (t-1) (moved++) where moved = (take n) $ crates!!(s-1)

part2 :: [String] -> String
part2 = executeAll executeInstruction'

test = ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]", " 1   2   3 ", "", "move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"]