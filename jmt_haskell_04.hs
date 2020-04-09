import Data.List
import Data.Function

if' True  x _ = x
if' False _ y = y

type TimeStamp = (Integer, Integer, Integer, Integer, Integer)

mins :: TimeStamp -> Integer
mins (_,_,_,_,x) = x

data Mode = WAKE | SLEEP | BEGIN deriving (Eq, Show)

type StateChange = (Integer, Mode, TimeStamp)

parseLine :: String -> StateChange
parseLine s = (i, st, (yr, mon, day, hr, mnt))
  where yr  = read (take 4 $ drop 1 s)  :: Integer
        mon = read (take 2 $ drop 6 s)  :: Integer
        day = read (take 2 $ drop 9 s)  :: Integer
        hr  = read (take 2 $ drop 12 s) :: Integer
        mnt = read (take 2 $ drop 15 s) :: Integer
        (st, i) = parseHelper (drop 19 s)

parseHelper :: String -> (Mode, Integer)
parseHelper s
  | c == 'G' = (BEGIN, id)
  | c == 'f' = (SLEEP, -1)
  | c == 'w' = (WAKE, -1)
  where c = head s
        id = read (takeWhile (\x -> x /= ' ') $ drop 7 s) :: Integer

data Status = AWAKE | ASLEEP deriving (Eq, Show)

 -- (id, last status change, current status)
type CurrentGuard = (Integer, Status, TimeStamp)

updateLog :: [(Integer, [Integer])] -> Integer -> (Integer, Integer) -> [(Integer, [Integer])]
updateLog sLog gId (start, end) = sLog ++ [(gId, interval)]
  where interval = if' (start > end) ([start..59] ++ [0..end]) ([start..end])

reduceLog :: [(Integer, [Integer])] -> [(Integer, [Integer])]
reduceLog g = zip r p'
  where p = groupBy ((==) `on` fst) $ sort g
        p' = map (concat . map snd) p
        r = map (fst . head) p

buildSleepLog :: ([(Integer, [Integer])], CurrentGuard)  -> StateChange -> ([(Integer, [Integer])], CurrentGuard) 
buildSleepLog (sLog, (gId, gSt, gTs)) (nId, nSt, nTs)
  | nSt == SLEEP = (sLog, (gId, ASLEEP, nTs)) -- guard falls asleep, do not update log, current guard falls alseep
  | nSt == WAKE  = (sLog', (gId, AWAKE, nTs)) 
  | otherwise    = (sLog, (nId, AWAKE, nTs)) -- change of guard do not update log, do update current guard 
  where sLog' = updateLog sLog gId (mins gTs, mins nTs - 1)

mostCommonElem list = fst $ maximumBy (compare `on` snd) elemCounts where
  elemCounts = nub [(element, count) | element <- list, let count = length (filter (==element) list)]

countMostCommon x = length [ y | y <- x,  y == mostCommonElem x]

main :: IO()     
main = do
  f <- readFile "input_04.txt"
  let t = map (parseLine) $ lines f
  let t' = sortBy (\(_,_,x) (_,_,y) -> compare x y) t

  let q = ([], (0, AWAKE, (0,0,0,0,0)))
  let (h,_) = foldl (buildSleepLog) q t'
  let o = reduceLog h

  let k = maximumBy (compare `on` length . snd) o
  putStr "Part 1: "
  putStrLn . show $ (fst k) * (mostCommonElem $ snd k)

  let r = maximumBy (compare `on` countMostCommon . snd) o
  putStr "Part 2: "
  putStrLn . show $ (fst r) * (mostCommonElem $ snd r)
