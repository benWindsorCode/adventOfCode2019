module DayTwo
    (
    ) where

-- todo: do we want to represent program as a list of instructions?
type Program = [Int]

data Opcode = Add | Multiply | End deriving (Show, Eq)

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

-- I solved part two by running the following query, and then calculating 100*a + b as requested
-- [(a,b) | a <- [0..99], b <- [0..99], (runProgram [1,a,b,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,6,23,27,1,6,27,31,2,31,9,35,1,35,6,39,1,10,39,43,2,9,43,47,1,5,47,51,2,51,6,55,1,5,55,59,2,13,59,63,1,63,5,67,2,67,13,71,1,71,9,75,1,75,6,79,2,79,6,83,1,83,5,87,2,87,9,91,2,9,91,95,1,5,95,99,2,99,13,103,1,103,5,107,1,2,107,111,1,111,5,0,99,2,14,0,0]) !! 0 == 19690720]

runProgram :: Program -> Program
runProgram program = runProgramHelper program 0 (program !! 0)

-- Given a program and current position and current opcode, then execute instruction and continue
runProgramHelper :: Program -> Int -> Int -> Program
runProgramHelper program pos 99 = program
runProgramHelper program pos currentOpcode = runProgramHelper updatedProgram newPos (program !! newPos)
        where 
            currentInstruction = parseInstruction [program !! pos, program !! (pos+1), program !! (pos+2), program !! (pos+3)]
            updatedProgram = updateProgram program (computeInstruction currentInstruction program)
            newPos = pos+4

-- Process actionable instructions
computeInstruction :: Instruction -> Program -> Result
computeInstruction instruction program
        | opcode instruction == Add = Result (firstVal+secondVal) (location instruction)
        | opcode instruction == Multiply = Result (firstVal*secondVal) (location instruction)
        where
            firstVal = program !! (firstLocation instruction)
            secondVal = program !! (secondLocation instruction)

-- todo: deal nicely with End opcode 99
parseInstruction :: [Int] -> Instruction
parseInstruction (opcode:firstLocation:secondLocation:location:rest) 
    | opcode == 1 = Instruction Add firstLocation secondLocation location
    | opcode == 2 = Instruction Multiply firstLocation secondLocation location
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