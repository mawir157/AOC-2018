import AdventHelper

import qualified Data.Set as Set
import Data.Maybe

stringToInt :: String -> Int
stringToInt x
   | head x == '+' = read (tail x)::Int
   | head x == '-' = -1 * read (tail x)::Int

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)

main :: IO()     
main = do 
    putStrLn "Day 1"
    f <- readFile "../input/input_01.txt"  
    let l =  map (stringToInt) . lines $ f
    printSoln 1 (last $ scanl1 (+) l)
    printSoln 2 (fromJust $ dup $ scanl1 (+) $ cycle $ l)
