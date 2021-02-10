module AdventHelper where
import Data.List
import Data.List.Split

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

if' True  x _ = x
if' False _ y = y

zipWithFn :: (a -> b) -> [a] -> [(a,b)]
zipWithFn fn as  = zip as (map (fn) as)

printSoln :: (Show a) => Integer -> a -> IO()
printSoln n s = do
  putStrLn ("  Part " ++ (show n) ++ ": " ++ (show s))

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

concat' :: [[a]] -> a -> [a]
concat' [] _ = []
concat' [s] _ = s
concat' (s:ss) c  = s ++ [c] ++ concat' ss c

freqCount :: Ord a => [a] -> [(a, Int)]
freqCount t = map (\xs@(x:_) -> (x, length xs)) . group $ sort t
