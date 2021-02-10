import Data.List
type Point = (Integer, Integer)

isInt :: Char -> Bool
isInt c = elem c ['0','1','2','3','4','5','6','7','8','9']

parseHelper :: String -> Char -> String
parseHelper x c = takeWhile(isInt).dropWhile(not.isInt).dropWhile(/= c)$x

parseInput :: String -> Point
parseInput s = (x, y)
  where x = read (takeWhile(isInt) s)::Integer
        y = read (parseHelper s ',')::Integer

dist :: Point -> Point -> Integer
dist p q = abs (fst p - fst q) + abs (snd p - snd q) 

distArray :: [Point] -> Point -> [Integer]
distArray (p:ps) a = (dist p a:distArray ps a)
distArray [] a = []

uniqueMinimumDist :: [Integer] -> Integer
uniqueMinimumDist x
  | (length . filter (== m) $ x) == 1 = m
  | otherwise                         = -1
  where m = minimum x

nearestUniquePoint :: [Point] -> Point -> Point
nearestUniquePoint [p] a = p
nearestUniquePoint p a
  | t >= 0 = snd . head . dropWhile(\x -> (fst x) /= t) $ k
  | t < 0  = (-1, -1)
  where t = uniqueMinimumDist $ distArray p a
        k = zip (distArray p a) p

compareGrids :: Ord a => [([a], Int)] -> Int
compareGrids [] = -1
compareGrids [p] = snd p
compareGrids (p:q:ps)
  | snd p > snd q = compareGrids(p:ps)
  | otherwise     = compareGrids(q:ps)

filterGrids :: Ord a => [(([a], Int), ([a], Int))] -> [([a], Int)]
filterGrids x = map(fst) . filter(\x -> (snd . fst $ x) == (snd . snd $ x)) $ x

totalDistance :: [Point] -> Point -> Integer
totalDistance [] a = 0
totalDistance (p:ps) a = dist p a + totalDistance ps a

summarise :: Ord a => [a] -> [([a], Int)]
summarise g = map (\x -> ([head x], length x)) . group . sort $ g

main = do 
  f <- readFile "../input/input_06.txt"
  let ps = map (parseInput) $ lines f
  let xLim = maximum . map (fst) $ ps
  let yLim = maximum . map (snd) $ ps
  let g = [(x,y) | x <- [0..(xLim + 1)], y <- [0..(yLim + 1)]]
  let t = map (nearestUniquePoint ps) g
  let v = summarise t
  let g2 = [(x,y) | x <- [-1..(xLim + 2)], y <- [-1..(yLim + 2)]]
  let t2 = map (nearestUniquePoint ps) g2
  let v2 = summarise t2
  let t = compareGrids . filterGrids $ zip v v2
  putStr "Part 1: "
  putStr . show $ t
  let r = map (totalDistance ps) g
  putStr "\nPart 2: "
  putStr . show . length $ filter (< 10000) r
  putStr "\n"