import Data.Bits
import Data.List.Split

data OpCode = ADDR | ADDI | MULR | MULI | BANR | BANI | BORR | BORI |
              SETR | SETI | GTIR | GTRI | GTRR | EQIR | EQRI | EQRR
              deriving (Eq, Enum, Show)

codes = [ADDR,ADDI,MULR,MULI,BANR,BANI,BORR,BORI,
         SETR,SETI,GTIR,GTRI,GTRR,EQIR,EQRI,EQRR]

codesStr = ["addr","addi","mulr","muli","banr","bani","borr","bori",
            "setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"]

strToOpCode :: String -> OpCode
strToOpCode s = fst $ head $ dropWhile(\x -> snd x /= s) z
  where z = zip codes codesStr

parseInput :: String -> Instruction
parseInput s = Instruction op t0 t1 t2
  where op = strToOpCode $ take 4 s
        t = splitOn " " (drop 5 s)
        t0 = read(t!!0)::Int
        t1 = read(t!!1)::Int
        t2 = read(t!!2)::Int

data Instruction = Instruction OpCode Int Int Int deriving (Show, Eq)
op :: Instruction -> OpCode
op (Instruction x _ _ _) = x
av :: Instruction -> Int
av (Instruction _ x _ _) = x
bv :: Instruction -> Int
bv (Instruction _ _ x _) = x
cv :: Instruction -> Int
cv (Instruction _ _ _ x) = x

type Machine = ([Int], Int)

setValue :: Machine -> Int -> Int -> Machine
setValue (m,p) at val = (values, p)
  where values = (take at m) ++ [val] ++ (drop (at + 1) m)

getValue :: Machine -> Int -> Int
getValue (m,p) at = m!!at

updateIntInd :: Machine -> Machine
updateIntInd m = setValue m (snd m) (newValue + 1)
  where newValue = getValue m $ snd m

apply :: Machine -> Instruction -> Machine
apply m i = updateIntInd $ applyOpCode m i $ op i

run :: Machine -> [Instruction] -> Machine
run m i
  | a >= length i = m
  | otherwise     = run m' i 
  where a  = getValue m $ snd m
        j  = i!!a
        m' = apply m j

brokenRun :: Machine -> [Instruction] -> Machine
brokenRun m i
  | a == 3        = m
  | a >= length i = m
  | otherwise     = brokenRun m' i 
  where a  = getValue m $ snd m
        j  = i!!a
        m' = apply m j

applyOpCode :: Machine -> Instruction -> OpCode -> Machine
applyOpCode m i opp
  | opp == ADDR = setValue m cVal ( aReg + bReg )
  | opp == ADDI = setValue m cVal ( aReg + bVal )
  | opp == MULR = setValue m cVal ( aReg * bReg )
  | opp == MULI = setValue m cVal ( aReg * bVal )
  | opp == BANR = setValue m cVal ( (.&.) aReg bReg )
  | opp == BANI = setValue m cVal ( (.&.) aReg bVal )
  | opp == BORR = setValue m cVal ( (.|.) aReg bReg )
  | opp == BORI = setValue m cVal ( (.|.) aReg bVal )
  | opp == SETR = setValue m cVal ( aReg )
  | opp == SETI = setValue m cVal ( aVal )
  | opp == GTIR = setValue m cVal ( if (aVal > bReg) then 1 else 0 )
  | opp == GTRI = setValue m cVal ( if (aReg > bVal) then 1 else 0 )
  | opp == GTRR = setValue m cVal ( if (aReg > bReg) then 1 else 0 )
  | opp == EQIR = setValue m cVal ( if (aVal == bReg) then 1 else 0 )
  | opp == EQRI = setValue m cVal ( if (aReg == bVal) then 1 else 0 )
  | opp == EQRR = setValue m cVal ( if (aReg == bReg) then 1 else 0 )
  where aVal = av i
        bVal = bv i
        cVal = cv i
        aReg = getValue m aVal
        bReg = getValue m bVal

sumOfDivisors :: Int -> Int
sumOfDivisors n = sum (s ++ t)
  where r = takeWhile (\x -> (x * x) < n) [1..]
        s = filter (\x -> n `mod` x == 0) r
        t = map (\x -> n `div` x) s

main :: IO()
main = do
  f <- readFile "input_18.txt"
  let l = lines $ f
  let ptr = read (take 1 $ drop 4 $ head l)::Int
  let i' = map (parseInput) $ drop 1 l

  putStr "part 1: "
  --putStrLn . show $ getValue (run ([0,0,0,0,0,0], ptr) i') 0
  putStrLn . show $ sumOfDivisors $ getValue (brokenRun ([0,0,0,0,0,0], ptr) i') 1
  putStr "part 2: "
  putStrLn . show $ sumOfDivisors $ getValue (brokenRun ([1,0,0,0,0,0], ptr) i') 1
