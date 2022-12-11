import MyUtils
import Debug.Trace
import Data.List

type Item = Integer
type MonkeyID = Int
 --Held items, worry modifier, worry test, which monkeys to throw to (if true, if false), total items inspected
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

monkeyBusiness :: Adjuster -> Monkey -> [(MonkeyID, Item)] --Returns a list of items a monkey is throwing
monkeyBusiness a ([],     _,  _,  _,       _) = []
monkeyBusiness a ((x:xs), fm, ft, (t1,t2), _) = (if ft x' then (t1, x') else (t2, x')):(monkeyBusiness a (xs,fm,ft,(t1,t2),0))  where x' = a (fm x)

runRound :: Adjuster -> [Monkey] -> [Monkey]
runRound a ms = foldl (processMonkey a) ms (indexes ms)

processMonkey :: Adjuster -> [Monkey] -> MonkeyID -> [Monkey]
processMonkey a ms n = zipWithIndexes ms |> map (\(m, mid)-> if mid==n then newMonkey else addItems thrown mid m) where 
    thrown = monkeyBusiness a (ms!!n)
    newMonkey = (ms!!n) |> (\(is, fm, ft, (t1,t2), n) -> ([], fm, ft, (t1,t2), n + (length thrown)))

addItems :: [(MonkeyID, Item)] -> MonkeyID -> Monkey -> Monkey
addItems xs mid (is, fm, ft, (t1,t2), n) = filter ((==mid) . fst) xs |> map snd |> (\newItems-> (is++newItems, fm, ft, (t1,t2), n))

runAll :: Adjuster -> Int -> [String] -> Int
runAll a n lines = splitOn "" lines |> map parseMonkey |> repeatF n (runRound a)|> map (\(_,_,_,_,n)->n) |> sort |> reverse |> take 2 |> product

part1 = runAll (`div` 3) 20

maxWorry :: Item
maxWorry = 2*3*5*7*11*13*17*19*23

part2 = runAll (`mod` maxWorry) 10000


--14283108168 is too low
--Debug
--showMonkey :: Monkey -> String
--showMonkey (items, _, _, (t1,t2), n) = (show items)++" | ("++(show t1)++", "++(show t2)++") | "++(show n)

showMonkey :: Monkey -> (Int, [Item])
showMonkey (items, _, _, (t1,t2), n) = (n, items)

modify :: Monkey -> Item -> Item
modify (_, f, _, _, _) x = f x

testItem :: Monkey -> Item -> Bool
testItem (_, _, f, _, _) x = f x

--runFor n = splitOn "" test |> map parseMonkey |> repeatF n (runRound (\x->)) |> map (\(_,_,_,_,n)->n)
runFor a n = splitOn "" test |> map parseMonkey |> repeatF n (runRound a) |>  map showMonkey--map (\(xs,_,_,_,n)->xs)
--runFor2 n = splitOn "" test |> map parseMonkey |> repeatF n (runRound id) |>  map showMonkey--map (\(xs,_,_,_,n)->xs)

roundPart a n input = map parseMonkey (splitOn "" input) |> (\ms-> foldl (processMonkey a) ms (indexes ms |> take n) )|> map showMonkey  |> map snd

dw x = x `mod` maxWorry

m0 = test |> splitOn "" |> head
test = ["Monkey 0:", "  Starting items: 79, 98", "  Operation: new = old * 19", "  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3", "", "Monkey 1:", "  Starting items: 54, 65, 75, 74", "  Operation: new = old + 6", "  Test: divisible by 19", "    If true: throw to monkey 2", "    If false: throw to monkey 0", "", "Monkey 2:", "  Starting items: 79, 60, 97", "  Operation: new = old * old", "  Test: divisible by 13", "    If true: throw to monkey 1", "    If false: throw to monkey 3", "", "Monkey 3:", "  Starting items: 74", "  Operation: new = old + 3", "  Test: divisible by 17", "    If true: throw to monkey 0", "    If false: throw to monkey 1"]
test2 = ["Monkey 0:", "  Starting items: 1182113,2856070,9497140,4808529,5267398,6777043,5330003,4308633", "  Operation: new = old * 19", "  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3", "", "Monkey 1:", "  Starting items: 2446652,7808262", "  Operation: new = old + 6", "  Test: divisible by 19", "    If true: throw to monkey 2", "    If false: throw to monkey 0", "", "Monkey 2:", "  Starting items:  ", "  Operation: new = old * old", "  Test: divisible by 13", "    If true: throw to monkey 1", "    If false: throw to monkey 3", "", "Monkey 3:", "  Starting items:   ", "  Operation: new = old + 3", "  Test: divisible by 17", "    If true: throw to monkey 0", "    If false: throw to monkey 1"]
test3 = ["Monkey 0:", "  Starting items: 10881803,196849870,814571410,470393649,1227428338,164774140531273,199323803,149803983", "  Operation: new = old * 19", "  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3", "", "Monkey 1:", "  Starting items: 177041072,201802062", "  Operation: new = old + 6", "  Test: divisible by 19", "    If true: throw to monkey 2", "    If false: throw to monkey 0", "", "Monkey 2:", "  Starting items:  ", "  Operation: new = old * old", "  Test: divisible by 13", "    If true: throw to monkey 1", "    If false: throw to monkey 3", "", "Monkey 3:", "  Starting items:   ", "  Operation: new = old + 3", "  Test: divisible by 17", "    If true: throw to monkey 0", "    If false: throw to monkey 1"]