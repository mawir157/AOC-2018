import Data.Bits
import Data.List
import Data.List.Split

data OpCode = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI |
              SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR |
              BAD deriving (Eq, Enum, Show)

codes :: [OpCode]
codes = [ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI, SETR, SETI, GTIR, GTRI,
         GTRR, EQIR, EQRI, EQRR]

data Device = Device Int Int Int Int deriving (Show, Eq)
devID :: Device -> Int
devID (Device x _ _ _) = x
av :: Device -> Int
av (Device _ x _ _) = x
bv :: Device -> Int
bv (Device _ _ x _) = x
cv :: Device -> Int
cv (Device _ _ _ x) = x

data DevTriple = DevTriple Device Device Device deriving (Show)
fst3 :: DevTriple -> Device
fst3 (DevTriple x _ _) = x
snd3 :: DevTriple -> Device
snd3 (DevTriple _ x _) = x
trd3 :: DevTriple -> Device
trd3 (DevTriple _ _ x) = x

tripleGood :: [OpCode] -> DevTriple -> Int
tripleGood ops devTri = match
  where after = map (applyOpCode (fst3 devTri) (snd3 devTri)) ops
        match = length $ filter(\x -> x == (trd3 devTri)) after

tripleWhich :: [OpCode] -> DevTriple -> [OpCode]
tripleWhich ops devTri = match
  where match = filter (\x -> f' x == (trd3 devTri)) ops
        f' = applyOpCode (fst3 devTri) (snd3 devTri)

removable :: [OpCode] -> [DevTriple] -> [(OpCode, Int)]
removable ops devs = nub d
  where a = map (tripleWhich ops) devs
        b = map devID $ map (snd3) devs
        c = filter(\x -> length (fst x) == 1) $ zip a b
        d = map (\x -> (head $ fst x, snd x)) c

reduce :: [DevTriple] ->  ([OpCode], [(OpCode, Int)]) -> ([OpCode], [(OpCode, Int)])
reduce devs ([], p) = ([], p)
reduce devs (c, d) = reduce devs (newCodes, codeDict)
  where toDict = removable c devs
        toRemove = map (fst) toDict
        newCodes = filter (\x -> not (x `elem` toRemove)) c
        codeDict = d ++ toDict

setValue :: Device -> Int -> Int -> Device
setValue d at val
  | at == 0   = Device val (av d) (bv d) (cv d)
  | at == 1   = Device (devID d) val (bv d) (cv d)
  | at == 2   = Device (devID d) (av d) val (cv d)
  | at == 3   = Device (devID d) (av d) (bv d) val
  | otherwise = error $ show ("setValue", d, at, val)

getValue :: Device -> Int -> Int
getValue d at
  | at == 0   = devID d
  | at == 1   = av d
  | at == 2   = bv d
  | at == 3   = cv d
  | otherwise = error $ show ("getValue", d, at)

isInt :: Char -> Bool
isInt c = elem c ['0','1','2','3','4','5','6','7','8','9']

parseInput1 :: [String] -> DevTriple
parseInput1 ss = DevTriple d1 d2 d3
  where c11 = read (take 1 $ drop 9 (ss!!0))::Int
        c12 = read (take 1 $ drop 12 (ss!!0))::Int
        c13 = read (take 1 $ drop 15 (ss!!0))::Int
        c14 = read (take 1 $ drop 18 (ss!!0))::Int
        c2  = splitOn " " (ss!!1)
        c21 = read (c2!!0)::Int
        c22 = read (c2!!1)::Int
        c23 = read (c2!!2)::Int
        c24 = read (c2!!3)::Int
        c31 = read (take 1 $ drop 9 (ss!!2))::Int
        c32 = read (take 1 $ drop 12 (ss!!2))::Int
        c33 = read (take 1 $ drop 15 (ss!!2))::Int
        c34 = read (take 1 $ drop 18 (ss!!2))::Int
        d1 = Device c11 c12 c13 c14
        d2 = Device c21 c22 c23 c24
        d3 = Device c31 c32 c33 c34

parseInput2 :: [String] -> Device
parseInput2 ss = Device d1 d2 d3 d4
  where k = splitOn " " (ss!!1)
        d1 = read (k!!0)::Int
        d2 = read (k!!0)::Int
        d3 = read (k!!0)::Int
        d4 = read (k!!0)::Int

applyOpCode :: Device -> Device -> OpCode -> Device
applyOpCode dev devApply opp 
  | opp == ADDR = setValue dev cVal ( aReg + bReg )
  | opp == ADDI = setValue dev cVal ( aReg + bVal )
  | opp == MULR = setValue dev cVal ( aReg * bReg )
  | opp == MULI = setValue dev cVal ( aReg * bVal )
  | opp == BANR = setValue dev cVal ( (.&.) aReg bReg )
  | opp == BANI = setValue dev cVal ( (.&.) aReg bVal )
  | opp == BORR = setValue dev cVal ( (.|.) aReg bReg )
  | opp == BORI = setValue dev cVal ( (.|.) aReg bVal )
  | opp == SETR = setValue dev cVal ( aReg )
  | opp == SETI = setValue dev cVal ( aVal )
  | opp == GTIR = setValue dev cVal ( if (aVal > bReg) then 1 else 0 )
  | opp == GTRI = setValue dev cVal ( if (aReg > bVal) then 1 else 0 )
  | opp == GTRR = setValue dev cVal ( if (aReg > bReg) then 1 else 0 )
  | opp == EQIR = setValue dev cVal ( if (aVal == bReg) then 1 else 0 )
  | opp == EQRI = setValue dev cVal ( if (aReg == bVal) then 1 else 0 )
  | opp == EQRR = setValue dev cVal ( if (aReg == bReg) then 1 else 0 )
  | opp == BAD  = error $ show opp
  where aVal = av devApply
        bVal = bv devApply
        cVal = cv devApply
        aReg = getValue dev aVal
        bReg = getValue dev bVal

main :: IO()
main = do
  f <- readFile "input"  
  let l = lines $ f
  let part1 = chunksOf 4 $ take 3116 l
  let devs = map(parseInput1) part1
  let count = map (tripleGood codes) devs
  putStr "Part 1: "
  putStrLn $ show $ length $ filter(\x -> x > 2) count

  ------------------------------------------------------------------------------
  let t = reduce devs (codes, [ ])
  putStrLn $ show $ snd t

  let part2 = map(parseInput2) $ drop 3118 l
  putStrLn $ show part2
  -- let initDevice = Device 0 3 1 1
  -- let applyDevice = Device 9 0 0 3
  -- let outDevice = Device 0 3 1 0
  -- let test = DevTriple initDevice applyDevice outDevice
  -- let good = tripleWhich test
  -- putStrLn $ show good
  -- putStr "ADDR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice ADDR
  -- putStr "ADDI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice ADDI
  -- putStr "MULR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice MULR
  -- putStr "MULI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice MULI
  -- putStr "BANR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice BANR
  -- putStr "BANI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice BANI
  -- putStr "BORR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice BORR
  -- putStr "BORI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice BORI
  -- putStr "SETR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice SETR
  -- putStr "SETI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice SETI
  -- putStr "GTIR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice GTIR
  -- putStr "GTRI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice GTRI
  -- putStr "GTRR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice GTRR
  -- putStr "EQIR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice EQIR
  -- putStr "EQRI: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice EQRI
  -- putStr "EQRR: "
  -- putStrLn $ show $ applyOpCode initDevice applyDevice EQRR
