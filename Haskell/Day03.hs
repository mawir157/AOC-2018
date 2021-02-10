import AdventHelper

import Data.List
import Data.List.Split

data Box = Box Integer Integer Integer Integer Integer deriving (Show)
--Access functions
boxid :: Box -> Integer
boxid (Box x _ _ _ _) = x
x0 :: Box -> Integer
x0 (Box _ x _ _ _) = x
y0 :: Box -> Integer
y0 (Box _ _ x _ _) = x
dx :: Box -> Integer
dx (Box _ _ _ x _) = x
dy :: Box -> Integer
dy (Box _ _ _ _ x) = x
tl :: Box -> (Integer, Integer)
tl (Box _ x y _ _) = (x, y)
br :: Box -> (Integer, Integer)
br (Box _ x y a b) = (x + a - 1, y + b - 1)

isInt :: Char -> Bool
isInt c = elem c "0123456789"

parseHelper :: String -> Char -> String
parseHelper x c = takeWhile(isInt).dropWhile(not.isInt).dropWhile(/= c)$x

parseInput :: String -> Box
parseInput x = Box a1 a2 a3 a4 a5
  where a1 = read (parseHelper x '#')::Integer
        a2 = read (parseHelper x '@')::Integer
        a3 = read (parseHelper x ',')::Integer
        a4 = read (parseHelper x ':')::Integer
        a5 = read (parseHelper x 'x')::Integer

pairToIndex :: Integer -> (Integer, Integer) -> Integer
pairToIndex gDim (x, y) = y + gDim * x

boxToIndices :: Integer -> Box -> [Integer]
boxToIndices gDim b = map (pairToIndex gDim) bPairs
  where bPairs = [(x, y) | x <- [(x0 b)..(x0 b + dx b - 1)],
                           y <- [(y0 b)..(y0 b + dy b - 1)]]

getAllIndices :: Integer -> [Box] -> [Integer]
getAllIndices gDim bs = concat $ map (boxToIndices gDim) bs

overlap :: Box -> Box -> Bool
overlap b c
  | boxid b == boxid c                                                 = False
  | (fst . tl $ b) > (fst . br $ c) || (fst . tl $ c) > (fst . br $ b) = False
  | (snd . tl $ c) > (snd . br $ b) || (snd . tl $ b) > (snd . br $ c) = False
  | otherwise                                                          = True

goodBox :: [Box] -> Box -> Bool
goodBox bs b = and $ map (not . overlap b) bs

main = do
  putStrLn "Day 3"
  f <- readFile "../input/input_03.txt"
  let bs = map (parseInput) $ lines f
  let gL = 1000

  let f = freqCount $ getAllIndices gL bs
  printSoln 1 (length $ filter(\(_,x) -> x > 1) f)
  printSoln 2 (boxid $ head $ filter (goodBox bs) bs)
