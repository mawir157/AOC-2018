import Data.List
import Data.List.Split

type Point = (Integer, Integer, Integer)
type Nanobot = (Point, Integer)

type Octohedron = (Integer, Integer, Integer, Integer,
                   Integer, Integer, Integer, Integer)

parseLine :: String -> Nanobot
parseLine s = ((p!!0, p!!1, p!!2), r')
  where v = takeWhile (\x -> x /= '>') . drop 1 $ dropWhile (\x -> x /= '<') s
        p = map (read) (splitOn "," v) :: [Integer]
        r = drop 2 $ dropWhile (\x -> x /= 'r') s
        r' = read r :: Integer

manDist :: Point -> Point -> Integer
manDist (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

inRange :: Nanobot -> Nanobot -> Bool
inRange (p1, r1) (p2, r2) = (manDist p1 p2) <= r1

parseToOctohedron :: Nanobot -> Octohedron
parseToOctohedron ((x,y,z),r) = (x + y + z + r, x + y + z - r,
                                 x + y - z + r, x + y - z - r,
                                 z + x - y + r, z + x - y - r,
                                 y + z - x + r, y + z - x - r)

slice :: Octohedron -> Octohedron -> Octohedron
slice (at, ab, bt, bb, ct, cb, dt, db)
      (at', ab', bt', bb', ct', cb', dt', db')
  | (anb > ant) || (bnb > bnt) || (cnb > cnt) || (dnb > dnt)  =  (at, ab, bt, bb, ct, cb, dt, db)
  | otherwise = (ant, anb, bnt, bnb, cnt, cnb, dnt, dnb)
  where ant = min at at'
        anb = max ab ab'
        bnt = min bt bt'
        bnb = max bb bb'
        cnt = min ct ct'
        cnb = max cb cb'
        dnt = min dt dt'
        dnb = max db db'

main :: IO()
main = do
  f <- readFile "../input/input_23.txt"
  let l = lines $ f
  let t = map (parseLine) l
  let t' = reverse $ sortBy (\x y -> compare (snd x) (snd y)) t
  let n = head t'
  putStr "Part 1: "
  putStrLn . show . length $ filter (inRange n) t

  let o = map (parseToOctohedron) t'
  let (oat, oab, obt, obb, oct, ocb, odt, odb) = foldl1 slice o 
  putStr "Part 2: "
  putStrLn $ show oat