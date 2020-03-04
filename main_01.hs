import qualified Data.Set as Set

stringToInt :: String -> Int
stringToInt x
   | head x == '+' = read (tail x)::Int
   | head x == '-' = -1 * read (tail x)::Int
   | otherwise     = 0

rollingSum :: [Int] -> Int
rollingSum (x:xs) = x + sum(xs)
rollingSum [] = 0

partTwoSum :: [Int] -> [Int]
partTwoSum (x:y:ys) = x:partTwoSum(x + y:ys)
partTwoSum xs = xs

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)

main :: IO()     
main = do 
    f <- readFile "input"  
    let l =  map (stringToInt) . lines $ f
    putStr "Part 1: "
    putStr . show . rollingSum $ l
    putStr "\nPart 2: "
    let rep = dup . partTwoSum . cycle $ l
    putStr . show $ rep
    putStr "\n"
