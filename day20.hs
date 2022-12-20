import MyUtils
import Data.List

type Item = (Int, Int) --The numer and its initial position
type File = [Item]
type Index = Int

move :: File -> Item -> File
move xs n = insertAt n (i+(fst n)) xs' where (xs', i) = splitFile n xs

splitFile :: Item -> File -> (File, Index) -- Provides: (list without the target, target's index)
splitFile n (x:xs) | x==n      = (xs, 0)
                   | otherwise = splitFile n xs |> \(a,i)-> (x:a, i+1)

insertAt :: Item -> Index -> File -> File
insertAt n i f = (take i' f)++[n]++(drop i' f) where i' = i `mod` (length f)

runAll :: Int -> [Int] -> Int
runAll n input = sum [coord 1, coord 2, coord 3] where 
    file = zipWithIndexes input
    mixed = repeatF n (\f-> foldl move f file) file
    zeroIndex = indexesWhere ((==0) . fst) mixed |> head
    coord n = mixed!!((zeroIndex + (n*1000)) `mod` (length file)) |> fst

part1 :: [String] -> Int
part1 lines = map readInt lines |> runAll 1

part2 :: [String] -> Int
part2 lines = map readInt lines |> map (*811589153) |> runAll 10 

test = ["1", "2", "-3", "3", "-2", "0", "4"]