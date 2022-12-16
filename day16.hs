import MyUtils
import Data.List
import Data.Maybe
import qualified Data.Map as Map

type RawRoom = (String, Int, [String]) --Name, flow, list of neighbours
--type Room = (String, Int, [(String, Int)]) --Name, flow, list of all other valve rooms and the costs of getting to them
type Room = (String, Int) --Name, flow
type State = (String, [Room], Int, Int) --Current room, unopened valves, pressure released, time left
type Distances = Map.Map (String, String) Int


parseRoom :: String -> RawRoom
parseRoom line = (name, flow, neighbours) where 
    name = (words line)!!1
    flow = split ((flip elem) "=;") line |> (!!1) |> read
    neighbours = words line |> drop 9 |> filter2 (/=',')

rawDistance :: [RawRoom] -> String -> String -> Int
rawDistance rooms start end = aStar (\r-> find ((==r) . fst3) rooms |> fromJust |> thd3 |> map (\n->(n,1))) (\_->0) start (==end)

buildDistanceMap :: [RawRoom] -> Distances
buildDistanceMap rooms = combinations valveRooms valveRooms |> foldl (\m (r1,r2)-> Map.insert (r1,r2) (rawDistance rooms r1 r2) m) Map.empty where
    valveRooms = filter ((>0) . snd3) rooms |> map fst3 |> ("AA":)

buildRooms :: [RawRoom] -> [Room]
buildRooms rooms = filter ((>0) . snd3) rooms |> map (\(n,f,_)->(n,f))

move :: Distances -> State -> Room -> Maybe State
move d (r, rs, pr, t) (n,f) = if newTime<=0 then Nothing else Just (n, filter (/=(n,f)) rs, pr + f*newTime, newTime)  where newTime = t - 1 - (d Map.! (r,n))

runAll :: Distances -> State -> Int
runAll d s@(r, rs, pr, t) = if next==[] then pr else map (runAll d) next |> maximum  where next = map (move d s) rs |> removeNothing 

--part1 :: [String] -> Int
--part1 lines = runAll (buildDistanceMap parsed) ("AA", buildRooms parsed, 0, 30)  where parsed = map parseRoom lines

type State2 = ([Actor], [Room], Int) -- Actor's states, unopened valves, pressure released,
type Actor = (String, Int) --Current room and time left

move2 :: Distances -> State2 -> Room -> Maybe State2
move2 d ((r,t):as, rs, pr) (n,f) = if newTime<=0 then Nothing else Just (insertActor as (n, newTime), filter (/=(n,f)) rs, pr + f*newTime) where 
    newTime = t - 1 - (d Map.! (r,n))

runAll2 :: Distances -> State2 -> Int
runAll2 d s@(as, rs, pr) = if next==[] then pr else map (runAll2 d) next |> maximum  where next = map (move2 d s) rs |> removeNothing 

insertActor :: [Actor] -> Actor -> [Actor]
insertActor [] a = [a]
insertActor ((r1,t1):as) (r2,t2) = if t2>t1 then (r2,t2):(r1,t1):as else (r1,t1):(insertActor as (r2,t2))


part2 :: [String] -> Int
part2 lines = runAll2 (buildDistanceMap parsed) ([("AA",26),("AA",26)], buildRooms parsed, 0)  where parsed = map parseRoom lines



main = runOnFile "input16.txt" part2

test = ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB", "Valve BB has flow rate=13; tunnels lead to valves CC, AA", "Valve CC has flow rate=2; tunnels lead to valves DD, BB", "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE", "Valve EE has flow rate=3; tunnels lead to valves FF, DD", "Valve FF has flow rate=0; tunnels lead to valves EE, GG", "Valve GG has flow rate=0; tunnels lead to valves FF, HH", "Valve HH has flow rate=22; tunnel leads to valve GG", "Valve II has flow rate=0; tunnels lead to valves AA, JJ", "Valve JJ has flow rate=21; tunnel leads to valve II"]