module DayOne
    (
        fuelForShip,
        allFuelForShip
    ) where

type Mass = Int

fuelForShip :: [Mass] -> Int
fuelForShip masses = foldl (+) 0 fuelList
        where fuelList = map fuelPerMass masses

-- Calculate fuel per mass, returning zero if negative fuel
fuelPerMass :: Mass -> Int
fuelPerMass mass 
    | result <= 0 = 0
    | otherwise = result
    where result = (quot mass 3) - 2

-------- part 2

allFuelForShip :: [Mass] -> Int
allFuelForShip masses = foldl (+) 0 fuelList
        where fuelList = map allFuelPerMass masses

-- Takes into account the mass of the fuel
allFuelPerMass :: Mass -> Int
allFuelPerMass mass = foldl (+) 0 fuelList
        where fuelList = fuelListPerMass mass []

fuelListPerMass :: Mass -> [Int] -> [Int]
fuelListPerMass mass [] = fuelListPerMass mass [fuelPerMass mass]
fuelListPerMass mass fuelList 
    | finalFuel == 0 = fuelList
    | otherwise = fuelListPerMass finalFuel updatedList
    where 
        finalFuel = last fuelList
        updatedList = fuelList ++ [fuelPerMass finalFuel]