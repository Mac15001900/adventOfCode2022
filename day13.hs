import MyUtils
import Data.List

data Packet = Number Int | List [Packet] deriving (Show, Eq, Read) 

parsePacket :: String -> Packet
parsePacket "[]"     = List []
parsePacket ('[':xs) = List $ init xs |> splitList 0 "" |> map parsePacket
parsePacket xs       = Number (read xs)

splitList :: Int -> String -> String -> [String]
splitList _     acc []       = [reverse acc]
splitList level acc ('[':xs) = splitList (level+1) ('[':acc) xs
splitList level acc (']':xs) = splitList (level-1) (']':acc) xs
splitList 0     acc (',':xs) = (reverse acc):(splitList 0 [] xs)
splitList level acc (x:xs)   = splitList level (x:acc) xs

comparePackets :: Packet -> Packet -> Ordering
--If both values are integers
comparePackets (Number x)    (Number y)    = if x<y then LT else if x>y then GT else EQ
--If both values are lists
comparePackets (List (x:xs)) (List (y:ys)) = if res /= EQ then res else comparePackets (List xs) (List ys) where res = comparePackets x y
comparePackets (List [])     (List (y:ys)) = LT
comparePackets (List (x:xs)) (List [])     = GT
comparePackets (List [])     (List [])     = EQ
--If exactly one value is an integer
comparePackets (Number x)    (List y)      = comparePackets (List [Number x]) (List y)
comparePackets (List x)      (Number y)    = comparePackets (List x) (List [Number y])

part1 :: [String] -> Int
part1 lines = splitOn "" lines |> map2 parsePacket |> map (\[a,b]->(a,b)) |> map (uncurry comparePackets) |> zipWithIndexes |> filter ((==LT) . fst) |> map snd |> map (+1) |> sum

dividers :: [Packet]
dividers = map parsePacket ["[[2]]", "[[6]]"]

part2 :: [String] -> Int
part2 lines = filter (/="") lines |> map parsePacket |> (dividers++) |> sortBy comparePackets |> zipWithIndexes |> filter (((flip elem) dividers) . fst) |> map snd |> map (+1) |> product


test = ["[1,1,3,1,1]", "[1,1,5,1,1]", "", "[[1],[2,3,4]]", "[[1],4]", "", "[9]", "[[8,7,6]]", "", "[[4,4],4,4]", "[[4,4],4,4,4]", "", "[7,7,7,7]", "[7,7,7]", "", "[]", "[3]", "", "[[[]]]", "[[]]", "", "[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]"]