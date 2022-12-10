import MyUtils
import Data.List

type Pos = (Int, Int)
type State = (Pos, Pos, [Pos]) -- Position of the head and the tail, and history of the tail
data Direction = U Int | D Int | L Int | R Int deriving (Show, Read, Eq)

catchUp :: Pos -> Pos -> Pos --Tail -> head -> new tail
catchUp (xt,yt) (xh,yh) | nearby (xh,yh) (xt,yt) = (xt,yt)
                        | otherwise = (changeTowards xt xh, changeTowards yt yh)

nearby :: Pos -> Pos -> Bool
nearby (xh,yh) (xt,yt) = abs (xh-xt) <= 1 && abs (yh-yt) <= 1

 --Moves the first number towards the second by 1
changeTowards :: Int -> Int -> Int
changeTowards x y | x==y = x
                  | x>y  = x-1
                  | x<y  = x+1

splitDir :: Direction -> [Direction]
splitDir (U n) = take n $ repeat (U 1)
splitDir (D n) = take n $ repeat (D 1)
splitDir (L n) = take n $ repeat (L 1)
splitDir (R n) = take n $ repeat (R 1)

moveHead :: Pos -> Direction -> Pos
moveHead (x,y) (U n) = (x,  y+n)
moveHead (x,y) (D n) = (x,  y-n)
moveHead (x,y) (L n) = (x-n,y  )
moveHead (x,y) (R n) = (x+n,y  )

move :: State -> Direction -> State
move (h, t, his) dir = (h', catchUp t h', t:his) where h' = moveHead h dir

part1 :: [String] -> Int
part1 lines = map read lines |> map splitDir |> concat |> foldl move ((0,0),(0,0),[]) |> (\(_,t,his)-> t:his) |> unique |> length

type State2 = (Pos, [Pos], [Pos]) -- Position of the head, rope, and tail history

catchUp2 :: [Pos] -> Pos -> [Pos] -- Rope -> head -> new rope
catchUp2 [] _     = []
catchUp2 (x:xs) h = h':(catchUp2 xs h')  where h' = catchUp x h

move2 :: State2 -> Direction -> State2
move2 (h, t, his) dir = (h', catchUp2 t h', (last t):his) where h' = moveHead h dir

part2 :: [String] -> Int
part2 lines = map read lines |> map splitDir |> concat |> foldl move2 ((0,0),take 9 $ repeat (0,0),[]) |> (\(_,t,his)-> (last t):his) |> unique |> length

test = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]
test2 = ["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"]