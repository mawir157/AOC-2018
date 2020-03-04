import Data.List

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
isInt c = elem c ['0','1','2','3','4','5','6','7','8','9']

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
getAllIndices _ [] = []
getAllIndices gDim (b:bs) = (boxToIndices gDim b) ++ (getAllIndices gDim bs)

freqCount :: Ord a => [a] -> [(a, Int)]
freqCount t = map (\xs@(x:_) -> (x, length xs)) . group $ sort t

overlap :: Box -> Box -> Bool
overlap b c
  | boxid b == boxid c                                                 = False
  | (fst . tl $ b) > (fst . br $ c) || (fst . tl $ c) > (fst . br $ b) = False
  | (snd . tl $ c) > (snd . br $ b) || (snd . tl $ b) > (snd . br $ c) = False
  | otherwise                                                          = True


bestBox :: [Box] -> [Box] -> Box
bestBox [] cs = error "Empty list"
bestBox [x] cs = x
bestBox (x:y:xs) cs = if cx < cy then bestBox (x:xs) cs else bestBox (y:xs) cs
  where cx = length $ filter (== True) (map (overlap $ x) (cs))
        cy = length $ filter (== True) (map (overlap $ y) (cs))

main = do
  f <- readFile "input"
  let bs = map (parseInput) $ lines f
  --let bs = [Box 1 1 3 4 4, Box 2 3 1 4 4, Box 3 5 5 2 2]
  let gL = 1000
  putStrLn "Part 1: "
  putStrLn . show . length $ filter(\x -> snd x > 1) $ freqCount $ getAllIndices gL bs
  putStrLn "Part 2: "
  let r = bestBox bs bs
  putStrLn . show $ boxid r
