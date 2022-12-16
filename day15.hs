import MyUtils
import Data.Maybe
import Data.List
import Data.Ord

type Pos = (Int, Int)
type Sensor = (Pos, Int) --Position of the sensor and its range
type Segment = (Int, Int) --Start and end positions of a covered segment

parseLine :: String -> Sensor
parseLine line = split (\x-> x `elem` "=,:") line |> \[_,a,_,b,_,c,_,d]->(buildSensor (read a, read b) (read c, read d))

buildSensor :: Pos -> Pos -> Sensor
buildSensor (xs,ys) (xb,yb) = ((xs,ys), (abs (xs-xb))+(abs (ys-yb)))

combineSegments :: Segment -> Segment -> Maybe Segment
combineSegments (s1,e1) (s2,e2) | s1 > e2 || s2 > e1 = Nothing
                                | otherwise          = Just (min s1 s2, max e1 e2)

getSegment :: Int -> Sensor -> Maybe Segment
getSegment row ((x,y), r) | abs (y-row) > r = Nothing
                          | otherwise       = Just (x - l, x + l) where l = r - (abs (y-row))

getAllSegments :: [Sensor] -> Int -> [Segment]
getAllSegments ss row = map (getSegment row) ss |> removeNothing |> sortBy (comparing fst) |> groupSegments

groupSegments :: [Segment] -> [Segment]
groupSegments []       = [] 
groupSegments [s]      = [s]
groupSegments (a:b:ss) = case combineSegments a b of
    Nothing -> a:(groupSegments (b:ss))
    (Just s)-> groupSegments (s:ss)

part1 :: [String] -> Int
part1 lines = map parseLine lines |> ((flip getAllSegments) 2000000) |> map (\(s,e)-> e-s+1) |> sum |> (subtract beacons) where 
    beacons = filter (\line-> splitOn '=' line |> last |> readInt |> (==2000000)) lines |> map (splitOn '=') |> map reverse |> map (take 2) |> unique |> length

part2 :: [String] -> Int
part2 lines = map (getAllSegments sensors) [0..4000000] |> zipWithIndexes |> find (\(ss,i)->length ss>1) |> fromJust |> \([(s,e),_], row)-> (e+1)*4000000+row where sensors = map parseLine lines

