import Data.List
import qualified Data.Sequence as Seq

mod10 :: Int -> [Int]
mod10 x
  | x > 9     = [1, x `mod` 10]
  | otherwise = [x]
       
tailRecState :: [Int]
tailRecState = 3 : 7 : f 0 1 (Seq.fromList [3,7])
  where
    f p1 p2 x = newDigits ++ f p1' p2' x'
      where
        v1        = x `Seq.index` p1
        v2        = x `Seq.index` p2
        newDigits = mod10 $ v1 + v2
        x'        = x Seq.>< (Seq.fromList newDigits)
        p1'       = (p1 + v1 + 1) `mod` (length x')
        p2'       = (p2 + v2 + 1) `mod` (length x')

part1 :: [Int] -> Int -> [Int]
part1 seq n = take 10 (drop n seq)

part2 :: [Int] -> [Int] -> Maybe Int
part2 xs ws = findIndex (==ws) sublists
  where sublists = map (take $ length ws) $ tails xs

--------------------------------------------------------------------------------
main = do
  let initStateR = ([7,3], (1, 0))
  putStr "Part 1:"
  putStrLn . show $ part1 tailRecState 293801

  putStr "Part 2:"
  putStrLn . show $ part2 tailRecState [2,9,3,8,0,1]