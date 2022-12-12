import MyUtils
import Debug.Trace
import Data.List

type Item = Integer
type MonkeyID = Int
 --Monkey contains: Held items, worry modifier, worry test, which monkeys to throw to (if true, if false), total items inspected
type Monkey = ([Item], (Item->Item), (Item->Bool), (MonkeyID, MonkeyID), Int)
type Adjuster = Item -> Item

parseMonkey :: [String] -> Monkey
parseMonkey lines = (items, op, test, (t1,t2), 0) where
    items = lines!!1 |> splitOn ' ' |> drop 4 |> concat |> ('[':) |> (++"]") |> read
    op1 = lines!!2 |> splitOn ' ' |> drop 5
    op = case op1 !! 1 of
        "+" -> (\old->old + (op1!!2 |> read))
        "*" -> if op1!!2 == "old" then (\old->old*old) else (\old->old * (op1!!2 |> read))
    test = (\worry-> worry `mod` (lines!!3 |> splitOn ' ' |> (!!5) |> read) == 0)
    t1 = lines!!4 |> splitOn ' ' |> (!!9) |> read
    t2 = lines!!5 |> splitOn ' ' |> (!!9) |> read

runRound :: Adjuster -> [Monkey] -> [Monkey]
runRound a ms = foldl (processMonkey a) ms (indexes ms)

processMonkey :: Adjuster -> [Monkey] -> MonkeyID -> [Monkey]
processMonkey a ms n = zipWithIndexes ms |> map (\(m, mid)-> if mid==n then newMonkey else addItems thrown mid m) where 
    thrown = monkeyBusiness a (ms!!n)
    newMonkey = (ms!!n) |> (\(is, fm, ft, (t1,t2), n) -> ([], fm, ft, (t1,t2), n + (length thrown)))

monkeyBusiness :: Adjuster -> Monkey -> [(MonkeyID, Item)] --Returns a list of items a monkey is throwing
monkeyBusiness a ([],     _,  _,  _,       _) = []
monkeyBusiness a ((x:xs), fm, ft, (t1,t2), _) = (if ft x' then (t1, x') else (t2, x')):(monkeyBusiness a (xs,fm,ft,(t1,t2),0))  where x' = a (fm x)

addItems :: [(MonkeyID, Item)] -> MonkeyID -> Monkey -> Monkey
addItems xs mid (is, fm, ft, (t1,t2), n) = filter ((==mid) . fst) xs |> map snd |> (\newItems-> (is++newItems, fm, ft, (t1,t2), n))

runAll :: Adjuster -> Int -> [String] -> Int
runAll a n lines = splitOn "" lines |> map parseMonkey |> repeatF n (runRound a)|> map (\(_,_,_,_,n)->n) |> sort |> reverse |> take 2 |> product

part1 = runAll (`div` 3) 20

maxWorry :: Item
maxWorry = 2*3*5*7*11*13*17*19*23

part2 = runAll (`mod` maxWorry) 10000