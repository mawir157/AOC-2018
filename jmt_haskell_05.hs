import Data.Char

kill :: String -> Char -> String
kill s c = filter (\x -> x /= toLower c && x /= toUpper c) s

killThenReduce :: String -> Char -> String
killThenReduce s c = reduce' $ kill s c

reduce :: Char -> String -> String
reduce c [] = [c]
reduce c (s:ss)
  | polar c s = ss
  | otherwise = (c:s:ss)
  
-- foldr  (a -> b -> b) -> b -> [a] -> b
-- foldr  (Char -> String -> String) -> String -> [Char] -> String
reduce' :: String -> String
reduce' s = foldr reduce "" s

polar :: Char -> Char -> Bool
polar x y
  | (x /= y) && (toLower x == toLower y) = True
  | otherwise                            = False
 
main = do
  f <- readFile "input_05.txt"
  let s = head $ lines f
  let r = reduce' s
  putStrLn "Part 1: "
  putStrLn . show . length $ r
  let t = map (killThenReduce s) $ ['a'..'z']
  putStrLn "Part 2: "
  putStrLn . show . minimum . map (length) $ t
