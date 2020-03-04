import qualified Data.Set as Set
import Data.List
import Data.Char
import Debug.Trace

type Edge = (Char, Char)

unique :: Ord a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

parseInput :: String -> Edge
parseInput s = (s!!5, s!!36)

type Graph = ([Char], [Edge]) --(vertices, edges)
type State = (Graph, [Integer]) --(graph, workers)

orphans :: Graph -> [Char]
orphans g
  | length v == 0 = []
  | length v == 1 = v
  | otherwise     = unique . filter (\x -> not $ elem x c) $ p
  where v = fst g
        e = snd g
        c = map (snd) $ e
        p = map (fst) $ e

killGraph :: Graph -> String
killGraph g
  | length v == 1 = v
  | otherwise     = (t:killGraph r)
  where v = fst g
        t = head . sort . orphans $ g
        r = removeVertex g t

removeVertex :: Graph -> Char -> Graph
removeVertex g c = (v, e)
  where v = [ x | x <- (fst g), x /= c]
        e = filter(\x -> fst x /= c) (snd g)

decr :: [Integer] -> [Integer]
decr x = map (subtract 1) x

setWorker :: [Integer] -> Integer -> [Integer]
setWorker x t =  (delete 0 x) ++ [t] -- delete the first 0 then stick t on the end

killGraph2 :: State -> Integer -> Integer
killGraph2 state offset
   --all the workers are free and there are no jobs left
   | and freeWorkers && (length vertices == 0) = 0
   --no free workers so decrement and carry on
   | not . or $ freeWorkers                 = 1 + killGraph2 (graph, decr workerTimes) offset
   --we have a free worker and at least one open node
   | otherwise                              = killGraph2 (reduceGraph, setWorker workerTimes topNodeTime) offset
   where graph       = fst state
         vertices    = traceShowId (fst graph)
         debug       = traceShowId (length vertices)
         workerTimes = snd state -- traceShowId (snd state)
         freeWorkers = map (== 0) $ workerTimes
         availVerts  = sort . orphans $ graph
         topNode     = if length availVerts > 0 then head availVerts else '_'--traceShowId (head availVerts)
         topNodeTime = node2Time offset topNode
         reduceGraph = removeVertex graph topNode



node2Time :: Integer -> Char -> Integer
node2Time x c = toInteger (ord c) - 64 + x

main = do 
  f <- readFile "input"
  let es = map (parseInput) $ lines f
  --let es = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
  let vs = unique $ (map (fst) $ es) ++ (map (snd) $ es)
  let g = (vs, es)
  let m = killGraph g
  putStr "Part 1: "
  putStr . show $ m
  putStr "\nPart2: "
  --let n = killGraph2 (g, [0, 0]) 0
  --putStr . show $ n
  putStr "\n"
