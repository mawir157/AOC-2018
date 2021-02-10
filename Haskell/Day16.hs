import Data.Bits
import Data.List
import Data.List.Split

data OpCode = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI |
              SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR |
              BAD deriving (Eq, Enum, Show)

codes :: [OpCode]
codes = [ADDR, ADDI, MULR, MULI, BANR, BANI, BORR, BORI,
         SETR, SETI, GTIR, GTRI, GTRR, EQIR, EQRI, EQRR]

data Device = Device Integer Integer Integer Integer deriving (Show, Eq)
devID :: Device -> Integer
devID (Device x _ _ _) = x
av :: Device -> Integer
av (Device _ x _ _) = x
bv :: Device -> Integer
bv (Device _ _ x _) = x
cv :: Device -> Integer
cv (Device _ _ _ x) = x

type DevTriple = (Device, Device, Device)
fst' (x, _, _) = x
snd' (_, x, _) = x
trd' (_, _, x) = x

tripleWhich :: [OpCode] -> DevTriple -> [OpCode]
tripleWhich ops devTri = filter (\x -> f' x == (trd' devTri)) ops
  where f' = applyOpCode (fst' devTri) (snd' devTri)

tripleGood :: [OpCode] -> DevTriple -> Int
tripleGood ops devTri = length $ tripleWhich ops devTri

removable :: [OpCode] -> [DevTriple] -> [(OpCode, Integer)]
removable ops devs = nub d
  where a = map (tripleWhich ops) devs
        b = map devID $ map (snd') devs
        c = filter(\x -> length (fst x) == 1) $ zip a b
        d = map (\x -> (head $ fst x, snd x)) c

reduce :: [DevTriple] ->  ([OpCode], [(OpCode, Integer)]) -> ([OpCode], [(OpCode, Integer)])
reduce devs ([], p) = ([], p)
reduce devs (c, d) = reduce devs (newCodes, codeDict)
  where toDict = removable c devs
        toRemove = map (fst) toDict
        newCodes = filter (\x -> not (x `elem` toRemove)) c
        codeDict = d ++ toDict

runReduced :: [(OpCode, Integer)] -> Device -> Device -> Device
runReduced dict st inst = applyOpCode st inst code
  where p = devID inst
        code = fst . head $ dropWhile (\(x, y) -> y /= p) dict

setValue :: Device -> Integer -> Integer -> Device
setValue d at val
  | at == 0   = Device val (av d) (bv d) (cv d)
  | at == 1   = Device (devID d) val (bv d) (cv d)
  | at == 2   = Device (devID d) (av d) val (cv d)
  | at == 3   = Device (devID d) (av d) (bv d) val
  | otherwise = error $ show ("setValue", d, at, val)

getValue :: Device -> Integer -> Integer
getValue d at
  | at == 0   = devID d
  | at == 1   = av d
  | at == 2   = bv d
  | at == 3   = cv d
  | otherwise = error $ show ("getValue", d, at)

parseInput1 :: [String] -> DevTriple
parseInput1 ss = (d1, d2, d3)
  where c11 = read (take 1 $ drop 9 (ss!!0))::Integer
        c12 = read (take 1 $ drop 12 (ss!!0))::Integer
        c13 = read (take 1 $ drop 15 (ss!!0))::Integer
        c14 = read (take 1 $ drop 18 (ss!!0))::Integer
        c2  = splitOn " " (ss!!1)
        c21 = read (c2!!0)::Integer
        c22 = read (c2!!1)::Integer
        c23 = read (c2!!2)::Integer
        c24 = read (c2!!3)::Integer
        c31 = read (take 1 $ drop 9 (ss!!2))::Integer
        c32 = read (take 1 $ drop 12 (ss!!2))::Integer
        c33 = read (take 1 $ drop 15 (ss!!2))::Integer
        c34 = read (take 1 $ drop 18 (ss!!2))::Integer
        d1 = Device c11 c12 c13 c14
        d2 = Device c21 c22 c23 c24
        d3 = Device c31 c32 c33 c34

parseInput2 :: String -> Device
parseInput2 ss = Device d1 d2 d3 d4
  where k = splitOn " " ss
        d1 = read (k!!0)::Integer
        d2 = read (k!!1)::Integer
        d3 = read (k!!2)::Integer
        d4 = read (k!!3)::Integer

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
  f <- readFile "i../input/nput_16.txt"  
  let l = lines $ f
  let part1 = chunksOf 4 $ take 3228 l
  let devs = map (parseInput1) part1
  -- putStrLn $ show devs
  let count = map (tripleGood codes) devs
  putStr "Part 1: "
  putStrLn . show . length $ filter(\x -> x >= 3) count

  let (_, t) = reduce devs (codes, [ ])
  let part2 = map (parseInput2) $ drop 3230 l
  -- putStrLn $ show part2
  let q = foldl (runReduced t) (Device 0 0 0 0) (part2)
  putStr "Part 2: "
  putStrLn . show $ devID q
