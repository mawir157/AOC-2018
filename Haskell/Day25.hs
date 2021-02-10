import Data.List
import Data.List.Split

type Point = (Integer, Integer, Integer, Integer)
type Constellation = [Point]

parseLine :: String -> Point
parseLine s = (t!!0, t!!1, t!!2, t!!3)
  where t = map (read) (splitOn "," s) :: [Integer]

manDist :: Point -> Point -> Integer
manDist (x1,y1,z1,t1) (x2,y2,z2,t2) = d
  where d = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (t1 - t2)

buildConsts :: [Constellation] -> Point -> [Constellation]
buildConsts [] p = [[p]]
buildConsts (c:cs) p
  | inConst c p = [c ++ [p]] ++ cs
  | otherwise   = [c] ++ buildConsts cs p

inConst :: Constellation -> Point -> Bool
inConst c p = or $ map (\x -> f x) c
  where f q = (manDist p q <= 3)

overlap :: Constellation -> Constellation -> Bool
overlap c d = or $ map (inConst c) d

collapse :: [Constellation] -> [Constellation]
collapse [] = []
collapse (c:cs) = [c ++ concat inGroup] ++ (collapse outGroup)
  where inGroup = filter (overlap c) cs
        outGroup = filter (not . overlap c) cs

collapseRecur :: [Constellation] -> [Constellation]
collapseRecur c
  | length c == length c' = c'
  | otherwise             = collapseRecur c'
  where c' = collapse c

main :: IO()
main = do
  f <- readFile "../input/input_25.txt"
  let l = lines $ f
  let t = map (parseLine) l
  let r = collapseRecur $ foldl (buildConsts) [] t
  putStr "Part 1: "
  putStrLn . show $ length r
