import AdventHelper

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Char
import Debug.Trace

-- killGraph2 :: State -> Integer -> Integer
-- killGraph2 state offset
--    --all the workers are free and there are no jobs left
--    | and freeWorkers && (length vertices == 0) = 0
--    --no free workers so decrement and carry on
--    | not . or $ freeWorkers                 = 1 + killGraph2 (graph, decr workerTimes) offset
--    --we have a free worker and at least one open node
--    | otherwise                              = killGraph2 (reduceGraph, setWorker workerTimes topNodeTime) offset
--    where graph       = fst state
--          vertices    = traceShowId (fst graph)
--          debug       = traceShowId (length vertices)
--          workerTimes = snd state -- traceShowId (snd state)
--          freeWorkers = map (== 0) $ workerTimes
--          availVerts  = sort . orphans $ graph
--          topNode     = if length availVerts > 0 then head availVerts else '_'--traceShowId (head availVerts)
--          topNodeTime = node2Time offset topNode
--          reduceGraph = removeVertex graph topNode

node2Time :: Char -> Integer
node2Time c = toInteger (ord c) - 64 + 0
--------------------------------------------------------------------------------
type Edge = (Char, Char)
type Graph = Map.Map Char String -- (Id, Parents)

parseLine :: String -> Edge
parseLine s = (s!!5, s!!36)

addEdge :: Graph -> Edge -> Graph
addEdge g (par, i) = Map.insertWith (++) i [par] g

removeNode :: Graph -> Char -> Graph
removeNode g c = Map.filter (/= "") $ Map.map (f c) g
  where f c' s'  = filter (/= c') s'

children :: Graph -> String
children g = nub $ concat $ Map.elems g

parents :: Graph -> String
parents g = Map.keys g

allNodes :: Graph -> String
allNodes g = nub $ (parents g) ++ (children g)

orphans :: Graph -> String
orphans g = sort $ (allNodes g) \\ (parents g)

reduceGraph :: (String, Graph) -> (String, Graph)
reduceGraph (s, g)
  | Map.size g == 1 = (s ++ (sort $ concat $ Map.elems g) ++ (Map.keys g), Map.empty)
  | length os == 0  = (s, g)
  | otherwise       = reduceGraph (s ++ [hd], removeNode g hd)
  where os = orphans g
        hd = head os
--------------------------------------------------------------------------------
type Graph' = [((Char, Integer), String)] -- ((Id, Cooldown), Parents)

addEdge' :: Graph' -> Edge -> Graph'
addEdge' g (par, i) = t' ++ [(n, s ++ [par])]
  where l = filter (\x -> (fst $ fst x) == i) g
        (n,s) = if' (length l > 0) (head l) ((i, node2Time i), "")
        t' = filter (\x -> (fst $ fst x) /= i) g

removeNode' :: Graph' -> Char -> Graph'
removeNode' g c = filter (\((c',i),s) -> c /= c') $ map (f c) g
  where f c' (x,s)  = (x, filter (/= c') s)

children' :: Graph' -> String
children' g = nub $ concat $ map (snd) g

parents' :: Graph' -> String
parents' g = map (fst . fst) g

orphans' :: Graph' -> String
orphans' g = map (fst . fst) k
  where k = filter (\(_,s) -> s == "") g
------------------- Broken ----------------------
timerDown :: Graph' -> Graph'
timerDown g = foldl f g q
  where f g' c = map (\((i,t),s) -> if' (i == c) ((i,t-1),s) ((i,t),s)) g'
        q = take 2 $ sort $ map (fst.fst) $ filter (\(_,s) -> s == "") g
------------------- Broken ----------------------
removable :: Graph' -> [Char]
removable g = sort $ intersect g0 $ orphans' g
  where g0 = map (fst.fst) $ filter (\((_,t),_) -> t <= 0) g

reduceGraph' :: (Integer, String, Graph') -> (Integer, String, Graph')
reduceGraph' (n, s, g)
  | length g  == 0 = (n+1, s, g)
  | length r  == 0 = reduceGraph' (n+1,s,g')
  | otherwise      = reduceGraph' (n+1,s ++ [hd], removeNode' g' hd)
  where g' = traceShowId $ timerDown g
        r  = removable g'
        hd = head r
--------------------------------------------------------------------------------
main = do 
--   -->A--->B--
--  /    \      \
-- C      -->D----->E
--  \           /
--   ---->F-----
  putStrLn "Day 7"
  f <- readFile "../input/input_07.txt"
  -- let es = map(parseLine) $ lines f
  let es = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
  let g = foldl (addEdge) Map.empty es
  printSoln 1 (fst $ reduceGraph ("", g))
  printSoln 2 ("---------------- TODO ----------------")

  let newG = foldl (addEdge') [] es
  let extras = (children' newG) \\ (parents' newG)
  let gg = foldl (\ g i -> g ++ [((i, node2Time i), "")]) newG extras
  putStrLn $ show gg
  let (n,p,_) = reduceGraph' (0, "", gg)
  putStrLn $ show $ n
  putStrLn $ show $ p

  -- let os = orphans' newG
  -- let gg = foldl (\ g i -> g ++ [((i, node2Time i), "")]) newG os
  -- putStrLn $ show newG
  -- putStrLn $ show gg
  -- putStrLn $ show $ allNodes' newG
  -- putStrLn $ show $ allNodes' gg
  -- putStrLn $ show $ orphans' newG
  -- putStrLn $ show $ orphans' gg
  -- putStrLn $ show $ parents' newG
  -- putStrLn $ show $ parents' gg
  -- putStrLn $ show $ children' newG
  -- putStrLn $ show $ children' gg


  -- printSoln 1 (fst $ reduceGraph' ("", newG))

