import Data.Bits
import Data.List.Split
import qualified Data.Set as Set

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
        t0 = read(t!!0)::Integer
        t1 = read(t!!1)::Integer
        t2 = read(t!!2)::Integer

data Instruction = Instruction OpCode Integer Integer Integer deriving (Show, Eq)
op :: Instruction -> OpCode
op (Instruction x _ _ _) = x
av :: Instruction -> Integer
av (Instruction _ x _ _) = x
bv :: Instruction -> Integer
bv (Instruction _ _ x _) = x
cv :: Instruction -> Integer
cv (Instruction _ _ _ x) = x

type Machine = ([Integer], Int)

setValue :: Machine -> Int -> Integer -> Machine
setValue (m,p) at val = (values, p)
  where values = (take at m) ++ [val] ++ (drop (at + 1) m)

getValue :: Machine -> Int -> Integer
getValue (m,p) at = m!!at

updateIntInd :: Machine -> Machine
updateIntInd m = setValue m (ptr) (newValue + 1)
  where ptr = fromIntegral $ snd m
        newValue = getValue m $ ptr

nextStep :: Machine -> [Instruction] -> Machine
nextStep m i = m'
  where ptr = fromIntegral $ snd m
        a   = fromIntegral $ getValue m ptr
        j   = i!!a
        m'  = apply m j

apply :: Machine -> Instruction -> Machine
apply m i = updateIntInd $ applyOpCode m i $ op i

run :: Machine -> [Instruction] -> Machine
run m i
  | a >= length i = m
  | otherwise     = run m' i 
  where ptr = fromIntegral $ snd m
        a   = fromIntegral $ getValue m ptr
        j   = i!!a
        m'  = apply m j

brokenRun :: Machine -> [Instruction] -> Machine
brokenRun m i
  | a == 28       = m
  | a >= length i = m
  | otherwise     = brokenRun m' i 
  where ptr = fromIntegral $ snd m
        a   = fromIntegral $ getValue m ptr
        j  = i!!a
        m' = apply m j

firstHalt :: Machine -> [Instruction] -> Integer
firstHalt m i = getValue m' 3
  where m' = brokenRun m i

nextHalt :: [Instruction] -> Machine -> Machine
nextHalt i m = m2
  where m'  = nextStep m i
        m2  = brokenRun m' i

next3Change :: [Instruction] -> Machine -> Machine
next3Change i m
  | getValue m' 3 /= getValue m 3 = m'
  | a >= length i                 = m
  | otherwise                     = next3Change i m' 
  where ptr = fromIntegral $ snd m
        a   = fromIntegral $ getValue m ptr
        j  = i!!a
        m' = apply m j

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs Set.empty
  where
    go x cont set
      | Set.member x set = []
      | otherwise        = x : cont (Set.insert x set)

nextHaltInt :: Integer -> Integer
nextHaltInt x = r3
  where (r1,r2,r3) = nHIRec (0, (.|.) x 65536, 14070682)

nHIRec :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) 
nHIRec (r1,r2,r3)
  | r2 == 0   = (r1,r2,r3)
  | otherwise = nHIRec (r1', r2', r3')
  where r2' = r2 `div` 256
        r1' = (.&.) r2 255
        r3' = (.&.) (( (.&.) (r3 + r1') (16777215)) * 65899) 16777215

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
        cVal = fromIntegral $ cv i
        aReg = getValue m (fromIntegral aVal)
        bReg = getValue m (fromIntegral bVal)

main :: IO()
main = do
  f <- readFile "input_21.txt"
  let l = lines $ f
  let ptr = read (take 1 $ drop 4 $ head l)::Int
  let i = map (parseInput) $ drop 1 l
  let m = ([0,0,0,0,0,0], ptr)

  putStr "Part 1: "
  putStrLn . show $ firstHalt m i 

  putStr "Part 2: "
  let ss = iterate nextHaltInt 0
  putStrLn . show . last $ takeUntilDuplicate ss
