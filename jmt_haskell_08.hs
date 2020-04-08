import Control.Monad
import Control.Monad.State

data Node = Node [Int] [Node] deriving (Show, Eq)

strSplit :: String -> Char -> [String]
strSplit [] d = [""]
strSplit (c:cs) d
    | c == d    = ("" : rest)
    | otherwise = (c : head rest) : tail rest
    where
        rest = strSplit cs d

str2Int :: String -> Int
str2Int s = read (s)::Int

parseInput :: String -> [Int]
parseInput s = map (str2Int) (strSplit s ' ')

getNext :: State [Int] Int
getNext = state (\(x:xs) -> (x, xs))

buildTree :: State [Int] Node
buildTree = do
  nChildren <- getNext
  nMetadata <- getNext
  children <- replicateM nChildren buildTree
  metadata <- replicateM nMetadata getNext
  return (Node metadata children)

sumMeta :: Node -> Int
sumMeta (Node m c) = sum m + sum (map sumMeta c)

sumMeta2 :: Node -> Int
sumMeta2 (Node m []) = sum m
sumMeta2 (Node m c)  = sum t
  where m' = map (subtract 1) m
        t  = [ sumMeta2 (c !! i) | i <- m', i < (length c) ]

main = do 
  f <- readFile "input_08.txt"
  let ps = parseInput $ (head . lines $ f)
  let qs = evalState buildTree (ps)
  putStrLn "Part 1: "
  putStrLn . show $ sumMeta qs
  putStrLn "Part 2: "
  putStrLn . show $ sumMeta2 qs
