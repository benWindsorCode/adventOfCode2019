module DayTwo
    (
    ) where

-- todo: do we want to represent program as a list of instructions?
type Program = [Int]

data Opcode = Add | Multiply | End deriving (Show)

data Instruction = Instruction {
    opcode :: Opcode,
    firstVal :: Int,
    secondVal :: Int,
    location :: Int
} deriving (Show)

-- todo: deal nicely with End opcode 99
parseInstruction :: [Int] -> Instruction
parseInstruction (opcode:firstVal:secondVal:location:rest) 
    | opcode == 1 = Instruction Add firstVal secondVal location
    | opcode == 2 = Instruction Multiply firstVal secondVal location
parseInstruction (opcode:rest) 
    | opcode == 99 = Instruction End 0 0 0

-- given a program, a location and a new value, return a new program with the value a the location updated
updateProgram :: Program -> Int -> Int -> Program
updateProgram program location newVal = replaceNth program location newVal

replaceNth :: [Int] -> Int -> Int -> [Int]
replaceNth _ _ [] = []
replaceNth (x:xs) n newVal 
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs