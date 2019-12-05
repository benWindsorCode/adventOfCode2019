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