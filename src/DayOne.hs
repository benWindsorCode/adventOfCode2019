module DayOne
    (
    ) where

type Mass = Int

fuelForShip :: [Mass] -> Int
fuelForShip [] = 0
fuelForShip (mass:remaining) = (fuelPerModule mass) + (fuelForShip remaining)

fuelPerModule :: Mass -> Int
fuelPerModule mass = (quot mass 3) - 2
