module DayOne
    (
        fuelForShip
    ) where

type Mass = Int

fuelForShip :: [Mass] -> Int
fuelForShip masses = foldl (+) 0 fuelList
        where fuelList = map fuelPerModule masses

fuelPerModule :: Mass -> Int
fuelPerModule mass = (quot mass 3) - 2