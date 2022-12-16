import MyUtils
import Data.List
import Data.Maybe
import qualified Data.Map as Map

type RawRoom = (String, Int, [String]) --Name, flow, list of neighbours
type Room = (String, Int) --Name, flow
type Distances = Map.Map (String, String) Int
type State = ([Actor], [Room], Int) -- Actor's states, unopened valves, pressure released,
type Actor = (String, Int) --Current room and time left

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

move2 :: Distances -> State -> Room -> Maybe State
move2 d ((r,t):as, rs, pr) (n,f) = if newTime<=0 then Nothing else Just (insertActor as (n, newTime), filter (/=(n,f)) rs, pr + f*newTime) where 
    newTime = t - 1 - (d Map.! (r,n))

explore :: Distances -> State -> Int
explore d s@(as, rs, pr) = if next==[] then pr else map (explore d) next |> maximum  where next = map (move2 d s) rs |> removeNothing 

insertActor :: [Actor] -> Actor -> [Actor]
insertActor [] a = [a]
insertActor ((r1,t1):as) (r2,t2) = if t2>t1 then (r2,t2):(r1,t1):as else (r1,t1):(insertActor as (r2,t2))

runAll :: Int -> [String] -> Int
runAll elephants input = explore (buildDistanceMap parsed) (actors, buildRooms parsed, 0)  where 
    parsed = map parseRoom input
    actors = [1..elephants] |> map (\n->("AA", 30 - 4*n)) |> (++[("AA", 30-4*elephants)]) --Assuming we train and let loose one elephant every 4 minutes

part1 = runAll 0
part2 = runAll 1

main = runOnFile "input16.txt" runAll 2 --Tried this for fun. 5 hours later it's still running. UPDATE: finished after 10 hours, with 2428 (328 more than with 1 elephant)

test = ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB", "Valve BB has flow rate=13; tunnels lead to valves CC, AA", "Valve CC has flow rate=2; tunnels lead to valves DD, BB", "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE", "Valve EE has flow rate=3; tunnels lead to valves FF, DD", "Valve FF has flow rate=0; tunnels lead to valves EE, GG", "Valve GG has flow rate=0; tunnels lead to valves FF, HH", "Valve HH has flow rate=22; tunnel leads to valve GG", "Valve II has flow rate=0; tunnels lead to valves AA, JJ", "Valve JJ has flow rate=21; tunnel leads to valve II"]