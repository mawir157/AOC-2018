import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Maybe

if' True  x _ = x
if' False _ x = x

type Scores = Map.Map Int Int 
type Players = (Int, Int)
type Game = (Int, Players, Seq.Seq Int, Scores)

next :: Players -> Players
next (c,m) = (mod (c+1) m, m)


move :: Game -> Int -> Game
move (i, p, xs, s) val
  | mod val 23 /= 0 = (i', next p, Seq.insertAt i' val xs, s)
  | otherwise = (d, next p, xs', s')
  where i' = 1 + mod (i + 1) (length xs)
        p' = fst p
        s' = if' (Map.member p' s) (Map.adjust (diff +) p' s) (Map.insert p' diff s)
        d =  mod (i - 7) (length xs)
        diff = ((fromJust $ Seq.lookup d xs) + val)
        xs' = Seq.deleteAt d xs

playGame :: Int -> Int -> Game
playGame players marbles = foldl move g [1..marbles]
  where g = (0, (0,players), Seq.fromList [0], Map.empty)

maxScore :: Game -> Int
maxScore (_,_,_,s) = maximum . map (snd) $ Map.toList s

main = do
  putStr "Part 1: "
  putStrLn $ show $ maxScore $ playGame 477 70851

  putStr "Part 2: "
  putStrLn $ show $ maxScore $ playGame 477 7085100
