module DayFour
    (
    ) where
        
import Debug.Trace

-- Will output the solution of the problem
calculateSolution :: Int
calculateSolution = length [x | x <-[158126..624574], isCorrectFormat x]

calculateSolution2 :: Int
calculateSolution2 = length [x | x <-[158126..624574], isCorrectFormat2 x]

isCorrectFormat :: Int -> Bool
isCorrectFormat number = areDigitsIncreasing digits && areTwoAdjTheSame digits
        where digits = getDigits number

-- todo: this is still missing the case where two are same but also part of the three consecutive e.g. 12223 is bad but would pass
isCorrectFormat2 :: Int -> Bool
isCorrectFormat2 number = areDigitsIncreasing digits && areTwoAdjTheSame digits && noThreeConsecutive digits
        where digits = getDigits number

noThreeConsecutive :: [Int] -> Bool
noThreeConsecutove [x,y] = True
noThreeConsecutive [x,y,z] = ((x /= y) || (y /= z) || (x /= z)) 
noThreeConsecutive (x:y:z:xs) = ((x /= y) || (y /= z) || (x /= z)) && noThreeConsecutive ([y] ++ [z] ++ xs)

areTwoAdjTheSame :: [Int] -> Bool
areTwoAdjTheSame [] = False
areTwoAdjTheSame [x] = False
areTwoAdjTheSame [x,y] = x == y
areTwoAdjTheSame (x:y:xs) = (x == y) || (areTwoAdjTheSame ([y]++xs))

areDigitsIncreasing :: [Int] -> Bool
areDigitsIncreasing [] = True
areDigitsIncreasing [x] = True
areDigitsIncreasing [x,y] = x <= y
areDigitsIncreasing (x:y:xs) = (x <= y) && (areDigitsIncreasing ([y]++xs))

getDigits :: Integral x => x -> [x]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]