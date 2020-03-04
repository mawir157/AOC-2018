import Data.List
import qualified Data.Sequence as Seq

getGrid :: Int -> Int -> [(Int, Int)]
getGrid m width = [ (x,y) | x <- [1..(m - width)], y <- [1..(m - width)] ]

powerGrid :: Int -> [(Int, Int)] -> [Int]
powerGrid _ [] = []
powerGrid s (x:xs) = (p:powerGrid s xs)
  where p = powerLevel (fst x) (snd x) s
        powerLevel x y s = (((((rackId * y) + s) * rackId) `div` 100) `mod` 10) - 5
          where rackId = 10 + x

pairToIndex :: (Int, Int) -> Int
pairToIndex (x, y) = (y - 1) + gridDimn * (x - 1)

indexToPair :: Int -> (Int, Int)
indexToPair i = (i `div` gridDimn + 1, i `mod` gridDimn + 1)

sumNbyN :: Seq.Seq Int -> Int -> Int -> Int
sumNbyN powLev p n = sum r
  where indices = map (\x -> p + x * gridDimn) [0..(n-1)]
        r = map (sumRow powLev n) indices
        sumRow x n i = sum (Seq.take n (Seq.drop i x))

maxNbyN :: Seq.Seq Int -> Int -> [Int] -> (Int, Int)
maxNbyN powLev n [f] = (f, sumNbyN powLev f n)
maxNbyN powLev n (f:fs)
  | snd lhs > snd rhs = lhs
  | otherwise         = rhs
  where lhs = (f, sumNbyN powLev f n)
        rhs = maxNbyN powLev n fs

maximumSnd :: Ord a => [(t, a)] -> (t, a)
maximumSnd [] = error "maximum of empty list"
maximumSnd (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

--SLOW!
maxAtPos :: Seq.Seq Int -> (Int, Int) -> (((Int, Int), Int), Int)
maxAtPos powLev p = maximumSnd $ zip l k
  where lim = min (gridDimn - (fst p) + 1) (gridDimn - (snd p) + 1)
        scale = [1..lim]
        k = map (sumNbyN powLev $ pairToIndex p) scale
        l = map (\x -> (p, x)) scale

gridDimn :: Int
gridDimn = 300

main :: IO()     
main = do 
  let pLevels = Seq.fromList $ powerGrid 7672 $ getGrid gridDimn 0 
  let redGrid = getGrid gridDimn 2
  putStrLn "Part 1: "
  let t = map (pairToIndex) redGrid
  let k = maxNbyN pLevels 3 t
  putStrLn $ show (indexToPair $ fst k, snd k)
  let o = map (maxAtPos pLevels) $ getGrid gridDimn 0
  putStrLn "Part 2: "
  putStrLn . show $ maximumSnd o
