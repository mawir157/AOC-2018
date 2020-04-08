type Rule = (Int, Int)

-- parsing code
parseState :: String -> [Int]
parseState x = stringToState $ drop 15 x

parseRule :: String -> Rule
parseRule s = (lhs, rhs)
  where lhs = blockScore $ stringToState (take 5 s)
        rhs = (stringToState $ [last s])!!0

stringToState :: [Char] -> [Int]
stringToState [] = []
stringToState (x:xs)
  | x == '#'  = (1:stringToState xs)
  | otherwise = (0:stringToState xs)

-- doing code
rule :: [Rule] -> Int -> Int
rule rs t
  | elem t (map (fst) rs) = snd . head $ dropWhile(\x -> fst x /= t) rs
  | otherwise             = 0

padState :: [Int] -> Int -> [Int]
padState x n = (take n (repeat 0)) ++ x ++ (take n (repeat 0))

blockScore :: [Int] -> Int
blockScore [] = 0
blockScore (x:xs) = x + 2 * blockScore xs

tick :: [Rule] -> [Int] -> [Int]
tick rs x = [0,0] ++ (map (rule rs) $ map (blockScore) (map (block x) [2..steps])) ++ [0,0]
  where steps = (length x) - 3
        block x n = take 5 (drop (n-2) x)


tickTock :: [Rule] -> Int -> [Int] -> [Int]
tickTock rs n x = (iterate (tick rs) x) !! n

main :: IO()     
main = do
  f <- readFile "input_12.txt"
  let l = lines f
  let st = parseState $ head l -- first line is state
  let rs = map (parseRule) (drop 2 l) -- lines 3 onwards are rules

  putStr "Part 1: "
  let p = 20
  let i = [(-p)..(p + length st - 1)]
  let ps = padState st p
  let es = tickTock rs 20 ps
  putStrLn . show . sum $ zipWith (*) es i
  
  putStr "Part 2: "
  -- we go linear after about 150 iterations
  let upto = 150
  let p2 = upto
  let i2 = [(-p2)..(p2 + length st - 1)]
  let ps2 = padState st p2
  let es2_x1 = tickTock rs upto ps2
  let es2_x2 = tickTock rs 1 es2_x1
  let s1 = sum $ zipWith (*) es2_x1 i2
  let s2 = sum $ zipWith (*) es2_x2 i2
  let a = s2 - s1
  let b = s2 - (a * (upto + 1))
  putStrLn . show $ (a * 50000000000) + b
