import MyUtils
import Data.List

part1 :: [String] -> Int
part1 lines = splitOn "" lines |> map2 read |> map sum |> foldr max 0


part2 :: [String] -> Int
part2 lines = splitOn "" lines |> map2 read |> map sum |> sort |> reverse |> take 3 |> sum

test ="\
\1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000\n\
\"