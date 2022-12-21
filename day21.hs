import MyUtils
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-------- Part 1 --------

data Monkey = Number Integer | Op String String (Integer->Integer->Integer) -- deriving (Show, Eq, Read) 
type Monkeys = Map.Map String Monkey

parseMonkey :: String -> (String, Monkey)
parseMonkey line = (name, parseOp (splitOn ' ' (tail op))) where [name, op] = splitOn ':' line

parseOp :: [String] -> Monkey
parseOp [x] = Number (read x)
parseOp [n1, op, n2] = Op n1 n2 (opDict Map.! op) where opDict = Map.fromList [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

evaluate :: Monkeys -> Monkey -> Integer
evaluate ms (Number x) = x
evaluate ms (Op m1 m2 op) = op (evaluate ms (ms Map.! m1)) (evaluate ms (ms Map.! m2))

part1:: [String] -> Integer
part1 lines = evaluate monkeys (monkeys Map.! "root") where monkeys = Map.fromList (map parseMonkey lines)

-------- Part 2 --------

data Monkey2 = Human | Root Monkey2 Monkey2 | Value Integer | Op2 MonkeyOp Monkey2 Monkey2 deriving (Show, Eq, Read) 
data MonkeyOp = Add | Sub | Mul | Div deriving (Show, Eq, Read, Ord) 

parseMonkey2 :: Map.Map String String -> String -> Monkey2
parseMonkey2 ms "humn" = Human
parseMonkey2 ms "root" = Root (parseMonkey2 ms m1) (parseMonkey2 ms m2) where [m1,_,m2] = splitOn ' ' (ms Map.!"root")
parseMonkey2 ms name   = case splitOn ' ' (ms Map.!name) of
    [x] -> Value (read x)
    [x,op,y] -> Op2 op' (parseMonkey2 ms x) (parseMonkey2 ms y) where op' = (Map.fromList [(("+"), Add), (("-"), Sub), (("*"), Mul), (("/", Div))]) Map.! op

parseMonkeys2 :: [String] -> String -> Monkey2
parseMonkeys2 lines root = parseMonkey2 (lines |> map (splitOn ':') |> map (\[a,b]-> (a, tail b)) |> Map.fromList) root

hasHuman :: Monkey2 -> Bool --Does this branch contain a human?
hasHuman Human = True
hasHuman (Value _) = False
hasHuman (Op2 _ m1 m2) = (hasHuman m1) || (hasHuman m2)

evaluate2 :: Monkey2 -> Integer --Evaluates a monkey's value. Assumes no human is present.
evaluate2 (Value x) = x
evaluate2 (Op2 op m1 m2) = (evaluate2 m1) `op'` (evaluate2 m2) where op' = (Map.fromList [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, (div))]) Map.! op

solve :: Monkey2 -> Integer -> Integer --Takes the value this monkey should evaluate to, returns human's value that makes it happen. Assumes a human is present somewhere.
solve (Root m1 m2)    _ = solve mh (evaluate2 mm) where [mh,mm] = if hasHuman m1 then [m1,m2] else [m2,m1]
solve  Human          x = x
solve (Op2 Add m1 m2) x = solve mh (x   -   (evaluate2 mm))   where [mh,mm] = if hasHuman m1 then [m1,m2] else [m2,m1]
solve (Op2 Mul m1 m2) x = solve mh (x `div` (evaluate2 mm))   where [mh,mm] = if hasHuman m1 then [m1,m2] else [m2,m1]
solve (Op2 Sub m1 m2) x = if hasHuman m1 then solve m1 ((evaluate2 m2) + x) else solve m2 ((evaluate2 m1)   -   x)
solve (Op2 Div m1 m2) x = if hasHuman m1 then solve m1 ((evaluate2 m2) * x) else solve m2 ((evaluate2 m1) `div` x)

part2 :: [String] -> Integer
part2 lines = parseMonkeys2 lines "root" |> flip solve 0

test = ["root: pppw + sjmn", "dbpl: 5", "cczh: sllz + lgvd", "zczc: 2", "ptdq: humn - dvpt", "dvpt: 3", "lfqf: 4", "humn: 5", "ljgn: 2", "sjmn: drzm * dbpl", "sllz: 4", "pppw: cczh / lfqf", "lgvd: ljgn * ptdq", "drzm: hmdt - zczc", "hmdt: 32"]
verify = runOnFile "input21.txt" (\lines-> part2 lines ==3093175982595 && part1 lines == 299983725663456)

{- --I was fairly sure this wouldn't work, but I had to try xD
part2Brute :: [String] -> Integer
part2Brute lines = [0..] |> find (\n-> (evaluate (Map.insert "humn" (Number n) monkeys) m1') == (evaluate (Map.insert "humn" (Number n) monkeys) m2')) |> fromJust    where
    monkeys = Map.fromList (map parseMonkey lines)
    (Op m1 m2 _) = monkeys Map.! "root"
    m1' = monkeys Map.! m1
    m2' = monkeys Map.! m2
-}


