import MyUtils
import Data.List

data File = Folder [File] String | Plain Int String deriving (Show, Eq, Read)
type State = ([String], File) --The current path (represented as a stack) and the known file structure
data Command = Up | Down String | Outermost | Ls [String] deriving (Show, Eq, Read)

parseCommands :: [String] -> [Command]
parseCommands []                             = []
parseCommands ("$ cd ..":xs)                 = (Up):(parseCommands xs)
parseCommands ("$ cd /" :xs)                 = (Outermost):(parseCommands xs)
parseCommands (('$':' ':'c':'d':' ':dir):xs) = (Down dir):(parseCommands xs)
parseCommands ("$ ls"   :xs)                 = (Ls (takeWhile ((/='$') . head) xs)):(parseCommands (dropWhile ((/='$') . head) xs))

executeCommand :: Command -> State -> State
executeCommand Up         (x:xs, files) = (xs, files)
executeCommand Outermost  (_,    files) = (["/"], files)
executeCommand (Down dir) (xs,   files) = (dir:xs, files)
executeCommand (Ls [])    s             = s
executeCommand (Ls (y:ys)) (xs, files)  = executeCommand (Ls ys) (xs, addFile (reverse xs|>tail) (parseFile y) files)

parseFile :: String -> File
parseFile ('d':'i':'r':' ':name) = Folder [] name
parseFile s = Plain (head parts |> readInt) (last parts) where parts = splitOn ' ' s

addFile :: [String] -> File -> File -> File -- Adds a file with a given path to the filesystem
addFile _ _ (Plain _ _) = error "Trying to add a file to a non-directory"
addFile []  newFile (Folder fs n) = Folder (newFile:fs) n
addFile (x:xs) newFile (Folder fs n) = Folder (addFile' (x:xs) newFile fs) n

addFile' :: [String] -> File -> [File] -> [File]
addFile' path newFile ((Plain a b):fs)    = (Plain a b):(addFile' path newFile fs)
addFile' (x:xs) newFile ((Folder f n):fs) = if n==x then (addFile xs newFile (Folder f n)):fs else (Folder f n):(addFile' (x:xs) newFile fs)
addFile' (x:xs) newFile [] = error ("Directory "++x++" not found")
addFile' a b c = error ("WTF "++(show a)++", "++(show b)++", "++(show c))

parseAll :: [String] -> File
parseAll lines = parseCommands lines |> foldl (flip executeCommand) ([], Folder [] "/") |> snd

size :: File -> Int
size (Plain n _) = n
size (Folder [] _) = 0
size (Folder (x:xs) n) = (size x) + size(Folder xs n)

countBelow :: Int -> File -> Int
countBelow maxSize (Plain _ _) = 0
countBelow maxSize (Folder xs n) = (map (countBelow maxSize) xs |> sum) + if size (Folder xs n) <= maxSize then size (Folder xs n) else 0

part1 :: [String] -> Int
part1 lines = parseAll lines |> countBelow 100000

findBest :: Int -> File -> Int
findBest _ (Plain _ _) = maxBound
findBest minSize (Folder xs n) = if current >= minSize then min current bestInside else bestInside where 
    bestInside = if length xs>0 then (map (findBest minSize) xs |> minimum) else maxBound
    current = size (Folder xs n)

part2 :: [String] -> Int
part2 lines = findBest (30000000-70000000+(size files)) files where files = parseAll lines
test = ["$ cd /", "$ ls", "dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a", "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst", "$ cd e", "$ ls", "584 i", "$ cd ..", "$ cd ..", "$ cd d", "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k"]