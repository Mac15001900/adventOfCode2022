

module MyUtils (runOnFile,runOnFile2,runOnFileGroup,(|>),split,count,freq,exists,(!!?),unique,unique',rotateMatrix,splitOn,joinWith,valueBetween, differences, tupleMap, repeatF, examine, examineRepeat, removeNothing, indexes, zipWithIndexes, map2, map3, setElement, setElement2, setElement3, empty2, empty3, directions2D, directions3D, flattenMaybe) where
import Control.Monad
import Data.List
import Data.Maybe
import System.IO

(|>) :: a -> (a->b) -> b
a |> f = f a

--Takes a file path and a function, runs that function on the file's contents, and prints the function's output. Trims the last line of the file if it's an empty line
runOnFile :: Show a => String -> ([String]->a) -> IO ()
runOnFile input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   print $ start linesTrimmed
   hClose handle

--Same as run on file, but splits the resulting array of strings by empty lines
runOnFileGroup :: Show a => String -> ([[String]]->a) -> IO ()
runOnFileGroup input start = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = splitOn '\n' contents
   let linesTrimmed = if last lines == "" then init lines else lines
   let res = splitOn "" linesTrimmed
   print $ start res
   hClose handle   
   
runOnFile2 :: ([String]->String) -> String -> IO ()
runOnFile2 start input = do
   handle <- openFile input ReadMode
   contents <- hGetContents handle
   let lines = split (=='\n') contents
   putStrLn $ start lines
   hClose handle
   
split     :: (a -> Bool) -> [a] -> [[a]]
split p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

count :: (a->Bool) -> [a] -> Int
count p = length . (filter p)

freq :: Eq a => [a] -> a -> Int
freq [] _     = 0
freq (x:xs) a = (if x==a then 1 else 0) + (freq xs a)

exists :: (a->Bool) -> [a] -> Bool
exists p xs = isJust (find p xs) 

--Equivalent of !!, but returns Nothing if there is no such element in an array and Just a otherwise
(!!?) :: [a] -> Int -> Maybe a
list !!? index = if index<0 || index>=length list then Nothing else Just (list!!index)

--Removes duplicates from a list
unique :: Eq a  => [a] -> [a]
unique xs = xs |> reverse |> unique' |> reverse

--Removes duplicates from a list faster, but messes up the order
unique' :: Eq a => [a] -> [a]
unique' []     = []
unique' (x:xs) = if x `elem` xs then unique' xs else x:unique' xs

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix (x:xs) = foldr largerZip (map (\a->[a]) x) (reverse xs) |> map reverse

largerZip :: [a] -> [[a]] -> [[a]]
largerZip []     []       = []
largerZip (x:xs) (ys:yss) = (x:ys):(largerZip xs yss)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a xs = splitOn' a xs []

splitOn' :: Eq a => a -> [a] -> [a]-> [[a]]
splitOn' a [] op     = [reverse op]
splitOn' a (x:xs) op = if a==x then (reverse op):(splitOn' a xs []) else splitOn' a xs (x:op)

joinWith :: [a] -> [[a]] -> [a]
joinWith a [] = []
joinWith a [x] = x
joinWith a (x:xs) = (x++a)++(joinWith a xs)

valueBetween :: Ord a => (a,a) -> a -> Bool
valueBetween (low,high) x = x >= low && x <= high

differences :: Num a => [a] -> [a]
differences [] = []
differences a = zip (tail a) (init a) |>  tupleMap (-)

tupleMap :: (a->b->c) -> [(a,b)] -> [c]
tupleMap f = map (\(a,b) -> f a b)

repeatF :: Int -> (a->a) -> a -> a
repeatF 0 _ x = x
repeatF n f x = repeatF (n-1) f (f x)

--For testing: shows the function's output for a list of inputs
examine :: Show a => Show b => (a->b) -> [a] -> IO()
examine f xs = map (\x-> (x, f x)) xs |> map (\(a,b)-> (show a)++(": ")++(show b)) |> joinWith "\n" |> putStrLn

--For testing: shows the results of a function's repeated applications
examineRepeat :: Show a => (a->a) -> a -> Int -> IO()
examineRepeat f a n = examine (\x-> (repeatF x f a)) [0..n]

removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing ((Just a):xs) = a:(removeNothing xs)

indexes :: [a] -> [Int]
indexes [] = []
indexes a = [0..(length a)-1]

zipWithIndexes :: [a] -> [(a,Int)]
zipWithIndexes a = zip a (indexes a)

map2 :: (a->b) -> [[a]] -> [[b]]
map2 f = map (map f)

map3 :: (a->b) -> [[[a]]] -> [[[b]]]
map3 f = map (map (map f))

empty2 :: Eq a => [[a]] -> Bool
empty2 xs = not $ exists (/=[]) xs

empty3 :: Eq a => [[[a]]] -> Bool
empty3 xss = (map (\xs->not $ exists (/=[]) xs) xss |> and)

setElement :: Int -> a -> [a] -> [a]
setElement i x xs = (take i xs)++[x]++(drop (i+1) xs)

setElement2 :: Int -> Int -> a -> [[a]] -> [[a]]
setElement2 i j x xs = (take j xs)++[setElement i x (xs!!j)]++(drop (j+1) xs)

setElement3 :: Int -> Int -> Int -> a -> [[[a]]] -> [[[a]]]
setElement3 i j k x xs = (take k xs)++[setElement2 i j x (xs!!k)]++(drop (k+1) xs)

directions2D :: [(Int,Int)]
directions2D = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]

directions3D :: [(Int,Int,Int)]
directions3D = [(-1,-1,-1),(-1,-1,0),(-1,-1,1),(-1,0,-1),(-1,0,0),(-1,0,1),(-1,1,-1),(-1,1,0),(-1,1,1),(0,-1,-1),(0,-1,0),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,0),(0,1,1),(1,-1,-1),(1,-1,0),(1,-1,1),(1,0,-1),(1,0,0),(1,0,1),(1,1,-1),(1,1,0),(1,1,1)]

flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe Nothing = Nothing
flattenMaybe (Just Nothing) = Nothing
flattenMaybe (Just (Just a)) = Just a

tempCombine ::  [Int] -> [[Int]] -> [[Int]]
tempCombine ys xss = map (\y-> map (\xs-> y:xs) xss) ys |> concat






