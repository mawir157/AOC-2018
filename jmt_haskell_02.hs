countChars :: [Char] -> String -> [Int]
countChars (c:cc) s  = (h:(countChars cc s))
  where h = length $ filter (== c) s
countChars [] _ = []

countInt :: Int -> [Int] -> Bool
countInt y xs = (length $ filter (== y) xs) > 0

overlap :: String -> String -> Int
overlap (s:ss) (t:tt)
  | s == t    = 1 + overlap ss tt
  | otherwise = overlap ss tt
overlap [] [] = 0

printOverlap :: String -> String -> String
printOverlap (s:ss) (t:tt)
  | s == t    = [s] ++ (printOverlap ss tt)
  | otherwise = "" ++ (printOverlap ss tt)
printOverlap [] [] = ""

compareOverlap :: (String, String) -> (String, String) -> (String, String)
compareOverlap pair1 pair2
  | overlap (fst $ pair1) (snd $ pair1) > overlap (fst $ pair2) (snd $ pair2) = pair1
  | otherwise                                                                 = pair2

getMaxOverlap :: [(String, String)] -> (String, String)
getMaxOverlap [] = error "maximum of empty list"
getMaxOverlap [s] = s
getMaxOverlap (s:ss) = compareOverlap s (getMaxOverlap ss)

main :: IO()
main = do
    let letters = ['a','b','c','d','e','f','g','h','i','j','k','l','m',
                   'n','o','p','q','r','s','t','u','v','w','x','y','z']
    f <- readFile "input_02.txt"
    let l = lines $ f
    let m = map (countChars $ letters) l
    let two = length $ filter (countInt 2) m
    let three = length $ filter (countInt 3) m
    putStr "Part 1: "
    putStr . show $ (two * three)
    putStr "\nPart 2: "
    let j = getMaxOverlap [(a, b) | a <- l, b <- l , a /= b]
    putStr . show $ printOverlap (fst $ j) (snd $ j)
    putStr "\n"
