import AdventHelper

import Data.Char

polar :: Char -> Char -> Bool
polar x y = (x /= y) && (toLower x == toLower y)

red :: Char -> String -> String
red c [] = [c]
red c (s:ss) = if' (polar c s) ss (c:s:ss)

del :: String -> Char -> String
del s c = filter (\x -> x /= toLower c && x /= toUpper c) s
 
main = do
  putStrLn "Day 05"
  f <- readFile "../input/input_05.txt"
  let s = head $ lines f
  printSoln 1 $ length $ foldr red "" s
  printSoln 2 $ minimum $ map (length . foldr red "" . del s) ['a'..'z']
