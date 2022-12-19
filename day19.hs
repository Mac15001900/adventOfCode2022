import MyUtils
import Data.List
import Text.Read

type Items = [Int] -- [Ore, Clay, Obsidian, Geode]
type Robots = Items --Amount of robots for each type
type Blueprint = [Items] --The costs for each type of robot
type Buildable = [Bool] --List of robots that it makes sense to build this turn (i.e. we can afford them, but couldn't previously)
type State = (Blueprint, Items, Robots, Buildable, Int) --The last int is the time remaining

parseBlueprint :: String -> Blueprint
parseBlueprint line = [[w!!0,0,0,0], [w!!1,0,0,0], [w!!2,w!!3,0,0], [w!!4,0,w!!5,0]] where w = words line |> map readMaybe |> removeNothing

turn :: State -> [State] --Current state -> list of possible moves
turn (_, _,  _,  _,  0) = []
turn (b, is, rs, bs, t) = (b, is', rs, newlyAffordable, t-1):moves where
    is' = zipWith (+) is rs
    newlyAffordable = zipWith (\old new-> (not old) && new) (affordable b is) (affordable b is')
    bs' =  zipWith (||) bs newlyAffordable
    moves = indexesWhere id bs |> map (buildRobot (b, is', rs, bs',t) is)

affordable :: Blueprint -> Items -> Buildable --Gets the list of affordable robots
affordable b is =  b |> map (zipWith (>=) is) |> map and

buildRobot :: State -> Items -> Int -> State
buildRobot (b, is, rs, bs, t) oldItems n =  (b, is', changeElement n (+1) rs, bs'', t-1) where
    is' = zipWith (-) is (b!!n)
    bs' = zipWith (&&) (affordable b is') bs
    --Now we need to restore buildability if: 1. Resources at the start of turn minus the cost of the robot would not be enough 2. Resources at the end of this turn are
    bs'' = zipWith (||) bs' (zipWith (&&) (affordable b (zipWith (-) oldItems (b!!n)) |> map not) (affordable b is'))

simulate :: State -> Int --Provides a best-case estimate of how many geodes can be cracked from a given state
simulate s@(_, is, _,  _,  0) = last is
simulate s@(b, is, rs, bs, t) = simulate (b, is4, rs4, bs, t-1) where
    is2 = zipWith (+) is rs
    rs2 = [0, (rs!!1)+1, rs!!2, rs!!3]
    (rs3,is3) = if (is!!1 >= b!!2!!1) then (changeElement 2 (+1) rs2, changeElement 1 (subtract (b!!2!!1)) is2) else (rs2,is2)
    (rs4,is4) = if (is!!2 >= b!!3!!2) then (changeElement 3 (+1) rs3, changeElement 2 (subtract (b!!3!!2)) is3) else (rs3,is3)

runAll :: State -> Int
runAll s@(_, is, _, _, _) = case turn s of
    [] -> last is
    ss -> foldr (\s m-> if simulate s > m then max (runAll s) m else m) (-1) ss

part1 :: [String] -> Int
part1 lines = map parseBlueprint lines |> map (\b-> runAll (b, [0,0,0,0], [1,0,0,0], [False,False,False,False], 24)) |> zipWithIndexes |> map (\(g,i)-> g*(i+1)) |> sum

part2 :: [String] -> Int
part2 lines = take 3 lines |> map parseBlueprint |> map (\b-> runAll (b, [0,0,0,0], [1,0,0,0], [False,False,False,False], 32)) |> product

