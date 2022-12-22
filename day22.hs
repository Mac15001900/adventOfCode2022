import Prelude hiding (Left, Right) 
import MyUtils
import Data.List
import Data.Maybe
import Data.Char hiding (Space)
import qualified Data.HashSet as Set
import qualified Data.Map as Map
import Debug.Trace

type Pos = (Int, Int)
data Facing = Right | Down | Left | Up  deriving (Show, Eq, Read, Enum)
data Tile = Space | Wall deriving (Show, Eq, Read)
type Board = (Map.Map Pos Tile, Int, Int) --The tiles, total width and height
type State = (Pos, Facing)
data Instruction = TurnRight | TurnLeft | Move Int deriving (Show, Eq, Read)

parseBoard :: [String] -> Board
parseBoard lines = (board, length (lines!!0), length lines) where
    board = zipWithIndexes2 lines |> filter2 ((/=' ') . fst) |> map2 (\(c, pos)-> if c=='.' then (pos, Space) else (pos, Wall)) |> concat |> Map.fromList

moveBy :: Board -> Facing -> Int -> Pos -> Pos
moveBy _ _ 0 pos = pos
moveBy bb@(b,w,h) dir n pos = if b Map.! next == Wall then pos else moveBy bb dir (n-1) next  where next=nextTile bb dir pos

nextTile :: Board -> Facing -> Pos -> Pos --Finds the next tile in a given direction, wrapping around if needed
nextTile (b,w,h) Up    (x,y) = if Map.member (x,  y-1) b then (x,  y-1) else firstInDir (b,w,h) x Up
nextTile (b,w,h) Down  (x,y) = if Map.member (x,  y+1) b then (x,  y+1) else firstInDir (b,w,h) x Down
nextTile (b,w,h) Left  (x,y) = if Map.member (x-1,y  ) b then (x-1,y  ) else firstInDir (b,w,h) y Left
nextTile (b,w,h) Right (x,y) = if Map.member (x+1,y  ) b then (x+1,y  ) else firstInDir (b,w,h) y Right

firstInDir :: Board -> Int -> Facing -> Pos --Finds the first tile when wrapping in a specific direction. n specifies the coordinate that doesn't change
firstInDir (b,w,h) n Up    = find (\dy-> Map.member (n, h-dy) b) [0..h] |> fromJust |> \dy-> (n, h-dy)
firstInDir (b,w,h) n Down  = find (\dy-> Map.member (n,   dy) b) [0..h] |> fromJust |> \dy-> (n,   dy)
firstInDir (b,w,h) n Left  = find (\dx-> Map.member (w-dx, n) b) [0..w] |> fromJust |> \dx-> (w-dx, n)
firstInDir (b,w,h) n Right = find (\dx-> Map.member (dx,   n) b) [0..w] |> fromJust |> \dx-> (dx,   n)

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions ('L':xs) = TurnLeft :(parseInstructions xs)
parseInstructions ('R':xs) = TurnRight:(parseInstructions xs)
parseInstructions xs = (Move (takeWhile isDigit xs |> readInt)):(dropWhile isDigit xs |> parseInstructions)

execute :: Board -> State -> Instruction -> State
execute _ (pos,dir) TurnLeft  = (pos, (((fromEnum dir)-1) `mod` 4) |> toEnum)
execute _ (pos,dir) TurnRight = (pos, (((fromEnum dir)+1) `mod` 4) |> toEnum)
execute b (pos,dir) (Move n)  = (moveBy b dir n pos, dir)

part1 :: [String] -> Int
part1 input = (1000*(y+1) + 4*(x+1) + (fromEnum dir)) where
     board = init input |> parseBoard
     ((x,y),dir) = foldl (execute board) ((find (\x-> Map.member (x,0) (fst3 board)) [0..]) |> fromJust |> \x->((x,0),Right))  (parseInstructions (last input))

---- Part 2 does not work for the example ----

nextTile2 :: Board -> Facing -> Pos -> State --Finds the next tile in a given direction, wrapping around if needed
nextTile2 b Up    (x,y) = if Map.member (x,  y-1) (fst3 b) then ((x,  y-1), Up)    else wrap b 50 Up    (x,y)
nextTile2 b Down  (x,y) = if Map.member (x,  y+1) (fst3 b) then ((x,  y+1), Down)  else wrap b 50 Down  (x,y)
nextTile2 b Left  (x,y) = if Map.member (x-1,y  ) (fst3 b) then ((x-1,y  ), Left)  else wrap b 50 Left  (x,y)
nextTile2 b Right (x,y) = if Map.member (x+1,y  ) (fst3 b) then ((x+1,y  ), Right) else wrap b 50 Right (x,y)

wrap :: Board -> Int -> Facing -> Pos -> State
wrap b s Up (x,y)    | x < s   = (firstInDir b (x+s)     Right, Right)
                     | x < 2*s = (firstInDir b (x+2*s)   Right, Right)
                     | x < 3*s = (firstInDir b (x-2*s)   Up, Up)
wrap b s Down (x,y)  | x < s   = (firstInDir b (x+2*s)   Down, Down)
                     | x < 2*s = (firstInDir b (x+2*s)   Left, Left)
                     | x < 3*s = (firstInDir b (x-s)     Left, Left)
wrap b s Right (x,y) | y < s   = (firstInDir b (3*s-y-1) Left, Left) 
                     | y < 2*s = (firstInDir b (y+s)     Up, Up)
                     | y < 3*s = (firstInDir b (3*s-y-1) Left, Left)
                     | y < 4*s = (firstInDir b (y-2*s)   Up, Up)
wrap b s Left (x,y)  | y < s   = (firstInDir b (3*s-y-1) Right, Right)
                     | y < 2*s = (firstInDir b (y-s)     Down, Down)
                     | y < 3*s = (firstInDir b (3*s-y-1) Right, Right)
                     | y < 4*s = (firstInDir b (y-2*s)   Down, Down)

execute2 :: Board -> State -> Instruction -> State
--execute2 _ _ i | traceShow i False = error ""
execute2 _ (pos,dir) TurnLeft  = (pos, (((fromEnum dir)-1) `mod` 4) |> toEnum)
execute2 _ (pos,dir) TurnRight = (pos, (((fromEnum dir)+1) `mod` 4) |> toEnum)
execute2 b (pos,dir) (Move n)  = traceShow (Move n) $ moveBy2 b dir n pos

moveBy2 :: Board -> Facing -> Int -> Pos -> State
--moveBy2 _ dir n pos | traceShow (dir,n,pos) False = error ""
--moveBy2 _ dir n pos | trace (showBoard test2 (pos,dir)) False = error ""
moveBy2 _ dir 0 pos = traceShow "Finished move" (pos, dir)
moveBy2 bb@(b,w,h) dir n pos = if b Map.! nextPos == Wall then traceShow ("Hit a wall at"++(show nextPos)) (pos,dir) else moveBy2 bb nextDir (n-1) nextPos  where (nextPos, nextDir)=nextTile2 bb dir pos


part2 :: [String] -> Int
part2 input = (1000*(y+1) + 4*(x+1) + (fromEnum dir)) where
     board = init input |> parseBoard
     ((x,y),dir) = foldl (execute2 board) ((find (\x-> Map.member (x,0) (fst3 board)) [0..]) |> fromJust |> \x->((x,0),Right))  (parseInstructions (last input))


test = ["        ...#         ", "        .#..", "        #...", "        ....", "...#.......#", "........#...", "..#....#....", "..........#.", "        ...#....", "        .....#..", "        .#......", "        ......#.", "", "10R5L5R10L4R5L5"]
test2 = ["    ...#...#", "    .#......", "    #....#..", "    ........", "    ...#", "    #...", "    ....", "    ..#.", "...#....", "........", "..#....#", "........", "....     ", ".#..     ", "....     ", "..#.     ", "", "10R5L5R10L4R5L5"]

showBoard :: [String] -> State -> String
showBoard b ((x,y),dir) = setElement2 x y (">v<^"!!(fromEnum dir)) b |> joinWith "\n"