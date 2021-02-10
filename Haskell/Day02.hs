import AdventHelper

import Data.List
import Data.Ord

countChars :: [Char] -> String -> [Int]
countChars [] _ = []
countChars (c:cc) s  = [h] ++ (countChars cc s)
  where h = length $ filter (== c) s

countInt :: Int -> [Int] -> Bool
countInt y xs = (length $ filter (== y) xs) > 0

overlap :: (String, String) -> String
overlap ([],[]) = []
overlap ((x:xs),(y:ys)) = (if' (x == y) [x] []) ++ overlap (xs,ys)

main :: IO()
main = do
    putStrLn "Day 2"
    f <- readFile "../input/input_02.txt"
    let l = lines $ f
    let letters = ['a'..'z']
    let m = map (countChars $ letters) l
    let two = length $ filter (countInt 2) m
    let three = length $ filter (countInt 3) m
    printSoln 1 (two * three)

    let q = map (overlap) [(a, b) | a <- l, b <- l , a /= b]
    printSoln 2 $ maximumBy (comparing length) q
