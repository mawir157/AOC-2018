type Pos = (Integer, Integer)
type Vel = (Integer, Integer)
type Particle = (Pos, Vel)

parseNext :: String -> String
parseNext [] = error "parseNext"
parseNext [s] = []
parseNext (s:ss) = ss

parseInput :: String -> Particle
parseInput s = ((read x::Integer, read y::Integer),
                (read u::Integer, read v::Integer))
  where x = takeWhile(/= ',') . parseNext . dropWhile(/= '<') $ s
        y = takeWhile(/= '>') . dropWhile(== ' ') . parseNext . dropWhile(/= ',') $ s
        u = takeWhile(/= ',') . parseNext . dropWhile(/= '<') . dropWhile(/= 'v') $ s
        v = takeWhile(/= '>') . dropWhile(== ' ') . parseNext . dropWhile(/= ',') . dropWhile(/= 'v') $ s

tick :: Particle -> Particle
tick (p, v) = (((fst p + fst v), (snd p + snd v)), v)

getX :: Particle -> Integer
getX (p, v) = fst p

getY :: Particle -> Integer
getY (p, v) = snd p

dimns :: [Particle] -> (Integer, Integer)
dimns [] = error "dimension of an empty list"
dimns [p] = (0,0)
dimns x = (dx, dy)
  where poss = map (fst) x
        xVals = map (fst) poss
        yVals = map (snd) poss
        dx = (maximum $ xVals) - (minimum $ xVals)
        dy = (maximum $ yVals) - (minimum $ yVals)

posMatch :: Pos -> Particle -> Bool
posMatch pos prt 
  | (getX prt) /= (fst pos) = False
  | (getY prt) /= (snd pos) = False
  | otherwise               = True

pos2Char :: [Particle] -> Pos -> Char
pos2Char prts p 
  | or . map (posMatch p) $ prts = '#'
  | otherwise = ' '

splitString :: [Char] -> Int -> [String]
splitString [] n = []
splitString x n = [take n x] ++ splitString (drop n x) n

toPrintable :: [Particle] -> [String]
toPrintable p = splitString (map (pos2Char p) grid) width
  where poss  = map (fst) p
        xVals = map (fst) poss
        yVals = map (snd) poss
        width = fromIntegral (maximum xVals - minimum xVals + 1)
        grid  = [(x,y) | y <- [minimum yVals..maximum yVals],
                         x <- [minimum xVals..maximum xVals]]

allignStars :: ([Particle], Int) -> ([Particle], Int)
allignStars (p,t)
  | fst d' <= fst d && snd d' <= snd d = allignStars (p', t+1)
  | otherwise                          = (p,t)
  where p' = map (tick) p
        d  = dimns p
        d' = dimns p'

main = do
  f <- readFile "input_10.txt"
  let p = map (parseInput) $ lines f
  let q = allignStars $ (p, 0)
  mapM_ putStrLn (toPrintable . fst $ q)
  putStrLn . show . snd $ q