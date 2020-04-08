import Debug.Trace
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

type MachineState = ([Int], (Int, Int))

machineLen :: MachineState -> Int
machineLen (r, (a, b)) = length r

collapse :: [String] -> String
collapse [] = ""
collapse (s:ss) = s ++ collapse ss

-- do everything the other way around - it is much faster to apply operations to
-- the start of a list than to the end. This mean all the indices are mangled
-- but the code will run in a `reasonable` amount of time.
--------------------------------------------------------------------------------
intToArray :: Int -> [Int]
intToArray x
  | x > 9     = [x `mod` 10, 1]
  | otherwise = [x]

nextState :: MachineState -> MachineState
nextState (r, (a, b)) = (rnew, (anew, bnew))
  where r1   = r!!a
        r2   = r!!b
        r12  = r1 + r2
        ir12 = intToArray (r12)
        rnew = ir12 ++ r
        ll   = traceShowId (length rnew)
        anew = (a + length ir12 - r1 - 1) `mod` (ll)
        bnew = (b + length ir12 - r2 - 1) `mod` (ll)

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

firstSubString :: Seq.Seq Int -> Seq.Seq Int -> Int
firstSubString xs wd = length $ Seq.takeWhileL (\x -> x /= wd) hds
  where tls = Seq.tails xs
        hds = fmap (Seq.take $ length wd) tls --list of arrays all of same length as wd

part2 :: [Int] -> [Int] -> Int
part2 x w = firstSubString (Seq.fromList x) (Seq.fromList w)




--------------------------------------------------------------------------------
main = do
  let initStateR = ([7,3], (1, 0))
  putStrLn "Part 1:"
  putStrLn . show $ take 100 tailRecState
  putStrLn . show $ part1 tailRecState 9
  putStrLn . show $ part1 tailRecState 5
  putStrLn . show $ part1 tailRecState 18
  putStrLn . show $ part1 tailRecState 2018
  putStrLn . show $ part1 tailRecState 147061

  --putStrLn . show $ game1 initStateR 147061
  putStrLn "Part 2:"

  -- putStrLn . show $ part2 tailRecState [5,1,5,8,9]