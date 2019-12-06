module DayFive
    (
    ) where

type Program = [Int]

data Opcode = Add | Multiply | Input | Output | End deriving (Show, Eq)

data Result = Result {
    output :: Int,
    outputLocation :: Int
}

data Instruction = Instruction {
    opcode :: Opcode,
    firstLocation :: Int,
    secondLocation :: Int,
    location :: Int
} deriving (Show)

runProgram :: Program -> Program
runProgram program = runProgramHelper program 0 (program !! 0)

-- Given a program and current position and current opcode, then execute instruction and continue
runProgramHelper :: Program -> Int -> Int -> Program
runProgramHelper program _ 99 = program
runProgramHelper program pos currentOpcode = runProgramHelper updatedProgram newPos (program !! newPos)
        where 
            currentInstruction = parseInstruction [program !! pos, program !! (pos+1), program !! (pos+2), program !! (pos+3)]
            updatedProgram = updateProgram program (computeInstruction currentInstruction program)
            newPos = pos + (getOffset currentInstruction)

getOffset :: Instruction -> Int
getOffset (Instruction Add _ _ _) = 4
getOffset (Instruction Multiply _ _ _) = 4
getOffset (Instruction Input _ _ _) = 2
getOffset (Instruction Output _ _ _) = 2
getOffset (Instruction End _ _ _) = 0 -- todo: do we need an offset for End? and if so, what? 

-- Process actionable instructions
computeInstruction :: Instruction -> Program -> Result
computeInstruction instruction program
        | opcode instruction == Add = Result (firstVal+secondVal) (location instruction)
        | opcode instruction == Multiply = Result (firstVal*secondVal) (location instruction)
        where
            firstVal = program !! (firstLocation instruction)
            secondVal = program !! (secondLocation instruction)

-- todo refactor to take opcode and remaining program so you can take the number of instructions you want
-- todo: deal nicely with End opcode 99
parseInstruction :: [Int] -> Instruction
parseInstruction (opcode:firstLocation:secondLocation:location:rest) 
    | opcode == 1 = Instruction Add firstLocation secondLocation location
    | opcode == 2 = Instruction Multiply firstLocation secondLocation location
    | opcode == 3 = Instruction Input firstLocation secondLocation location
    | opcode == 4 = Instruction Output firstLocation secondLocation location   
parseInstruction (opcode:rest) 
    | opcode == 99 = Instruction End 0 0 0

-- given a program, and the result of an instruction, return a new program with the value a the location updated
updateProgram :: Program -> Result -> Program
updateProgram program result = replaceNth program (outputLocation result) (output result)

replaceNth :: [Int] -> Int -> Int -> [Int]
replaceNth [] _ _ = []
replaceNth (x:xs) n newVal 
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth xs (n-1) newVal

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits x 
    | digitNumber < 5 = padZeroes digits
    | digitNumber == 5 = digits
    where 
        digits = getDigits (x `div` 10) ++ [x `mod` 10]
        digitNumber = length digits

-- pad out zeroes on a list to add leading zeroes up to five total digits
-- todo: I'm sure there is a more elegant way to do this...
padZeroes :: [Int] -> [Int]
padZeroes [a,b,c,d,e] = [a,b,c,d,e]
padZeroes [a,b,c,d] = [0,a,b,c,d]
padZeroes [a,b,c] = [0,0,a,b,c]
padZeroes [a,b] = [0,0,0,a,b]
padZeroes [a] = [0,0,0,0,a]