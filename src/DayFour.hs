module DayFour
    (
    ) where

-- isOnlyIncrease :: Int -> Bool
-- isOnlyIncrease number = 

getDigits :: Integral x => x -> [x]
getDigits 0 = []
getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]