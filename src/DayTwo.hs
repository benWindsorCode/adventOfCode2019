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
    firstVal :: Int,
    secondVal :: Int,
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
            updatedProgram = updateProgram program (computeInstruction currentInstruction)
            newPos = pos+4

-- Process actionable instructions
computeInstruction :: Instruction -> Result
computeInstruction instruction 
        | opcode instruction == Add = Result ((firstVal instruction)+(secondVal instruction)) (location instruction)
        | opcode instruction == Multiply = Result ((firstVal instruction)*(secondVal instruction)) (location instruction)

-- todo: deal nicely with End opcode 99
parseInstruction :: [Int] -> Instruction
parseInstruction (opcode:firstVal:secondVal:location:rest) 
    | opcode == 1 = Instruction Add firstVal secondVal location
    | opcode == 2 = Instruction Multiply firstVal secondVal location
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