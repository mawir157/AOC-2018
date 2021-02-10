import Data.List.Split
import qualified Data.Vector as Vec

data Tile = OPEN | TREE | YARD  deriving (Eq)

type Grid = Vec.Vector Tile

getNeighbours :: Int -> Int -> [Int]
getNeighbours n i = x
  where (ix, iy) = indexToCoord n i
        lx = max 0 (ix - 1)
        hx = min (n-1) (ix + 1)
        ly = max 0 (iy - 1)
        hy = min (n-1) (iy + 1)
        nCoord = [(x, y) | x <- [lx..hx], y <- [ly..hy], (x,y) /= (ix, iy)]
        x = map (coordToIndex n) nCoord
        indexToCoord n i = (i `mod` n, i `div` n)
        coordToIndex n (x, y) = (n * y) + x

parseInput :: [String] -> (Grid, Int)
parseInput s = (k, n)
  where n = length $ head s
        k = Vec.fromList $ map (charToTile) $ collapse s
        collapse [] = []
        collapse (s:ss) = s ++ collapse ss
        charToTile c
          | c == '.' = OPEN
          | c == '#' = YARD
          | c == '|' = TREE

updateTile :: Tile -> [Tile] -> Tile
updateTile t nbrs
  | t == OPEN = if (ts >= 3) then TREE else OPEN
  | t == TREE = if (ys >= 3) then YARD else TREE
  | t == YARD = if ((ys >= 1) && (ts >= 1)) then YARD else OPEN
  where os = length $ filter (\x -> x == OPEN) nbrs
        ts = length $ filter (\x -> x == TREE) nbrs
        ys = length $ filter (\x -> x == YARD) nbrs

tick :: (Grid, Int) -> (Grid, Int)
tick (g, n) = (g', n)
  where i = [0..((length g)-1)]
        nbrs = map (getNeighbours n) i
        nbrTiles = map (gridAtArr g) nbrs
        pairs = Vec.zip g (Vec.fromList nbrTiles)
        g' = Vec.map (\x -> updateTile (fst x) (snd x)) pairs
        gridAtArr g is = map (g Vec.! ) is

score :: Grid -> Int
score g = trees * yards
  where trees = length $ Vec.filter (\x -> x == TREE) g
        yards = length $ Vec.filter (\x -> x == YARD) g

run :: Int -> (Grid, Int) -> (Grid, Int)
run 0 g = g
run n g = run (n-1) (tick g)

runUntil :: Int -> ((Grid, Int), Int) -> ((Grid, Int), Int)
runUntil n (g, x)
  | n == score (fst g') = (g', x')
  | otherwise           = runUntil n (g', x')
  where g' = tick g
        x' = x + 1

main :: IO()
main = do
  f <- readFile "../input/input_18.txt"
  let l = lines $ f
  let i = parseInput l
  let p10 = run 10 i
  putStr "Part 1: "
  putStrLn $ show $ score (fst p10)

  let burnIn = 501
  let p500 = run burnIn i
  let s = score (fst p500)
  let j = runUntil s (p500, 0)
  let reduced = (1000000000 - burnIn) `mod` (snd j)
  let pFinal = run reduced p500
  putStr "Part 2: "
  putStrLn $ show $ score (fst pFinal)
