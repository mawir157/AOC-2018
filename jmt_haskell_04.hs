import Data.List
import Data.Function

if' True  x _ = x
if' False _ y = y

-- min, hr, day, mon, yr
type TimeStamp = (Integer, Integer, Integer, Integer, Integer)

tDiff :: TimeStamp -> TimeStamp -> Integer
tDiff (_, _, _, hrA, mntA) (_, _, _, hrB, mntB) = (mntA - mntB) + extra
  where extra = if' (hrA < hrB) 60 0

data Mode = WAKE | SLEEP | BEGIN deriving (Eq, Show)

type StateChange = (Integer, Mode, TimeStamp)
time (t, _, _) = t
-- [1518-05-04 23:48] Guard #1663 begins shift
-- [1518-10-31 00:24] falls asleep
-- [1518-11-07 00:54] wakes up

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

type GuardSleepLog = [(Integer, Integer)] -- [(id, sleeptime)]
data Status = AWAKE | ASLEEP deriving (Eq, Show)

 -- (id, last status change, current status)
type CurrentGuard = (Integer, Status, TimeStamp)
type GuardState = (GuardSleepLog, CurrentGuard) 

-- 
updateLog :: GuardSleepLog -> Integer -> Integer -> GuardSleepLog
updateLog sLog gId sleepTime = sLog ++ [(gId, sleepTime)]

reduceLog :: GuardSleepLog -> [(Integer, Integer)]
reduceLog g = zip r p'
  where p = groupBy ((==) `on` fst) $ sort g
        p' = map (sum . map snd) p
        r = map (fst . head) p

buildSleepLog :: GuardState -> StateChange -> GuardState
buildSleepLog (sLog, (gId, gSt, gTs)) (nId, nSt, nTs)
  | nSt == SLEEP = (sLog, (gId, ASLEEP, nTs)) -- guard falls asleep, do not update log, current guard falls alseep
  | nSt == WAKE  = (sLog', (gId, AWAKE, nTs)) 
  | otherwise    = (sLog, (nId, AWAKE, nTs)) -- change of guard do not update log, do update current guard 
  where sLog' = updateLog sLog gId (tDiff nTs gTs)

main :: IO()     
main = do
  f <- readFile "input_04.txt"
  let t = map (parseLine) $ lines f
  -- putStrLn . show $ take 10 t
  let t' = sortBy (\(_,_,x) (_,_,y) -> compare x y) t
  putStrLn . show $ take 10 t'
  let g = ([], (0, AWAKE, (0,0,0,0,0))) :: GuardState

  let (g',_) = foldl (buildSleepLog) g t'
  let o = reduceLog g'
  let k = maximumBy (compare `on` snd) o
  putStrLn $ show o
  putStrLn $ show (fst k * snd k)

