module DayThree
    (
    ) where

data Direction = U | D | L | R deriving (Show, Eq)

data Move = Move {
    direction :: Direction,
    length :: Int
}

data Coordinate = Coordinate Int Int deriving (Show, Eq)

-- Given a Move and a start coordinate, return a list of the path 
singlePath :: Move -> Coordinate -> [Coordinate]
singlePath (Move direction length) startCoord = singlePathHelper direction length startCoord [startCoord]

singlePathHelper :: Direction -> Int -> Coordinate -> [Coordinate] -> [Coordinate]
singlePathHelper _ 0 _ coords = coords
singlePathHelper direction remainingDist currentCoord coords 
        | direction == U = singlePathHelper direction (remainingDist-1) (moveUp currentCoord) (coords ++ [moveUp currentCoord])

moveUp :: Coordinate -> Coordinate
moveUp (Coordinate x y) = Coordinate x (y+1)

moveDown :: Coordinate -> Coordinate
moveDown (Coordinate x y) = Coordinate x (y-1)

moveLeft :: Coordinate -> Coordinate
moveLeft (Coordinate x y) = Coordinate (x-1) y

moveRight :: Coordinate -> Coordinate
moveRight (Coordinate x y) = Coordinate x (y+1)

distance :: Coordinate -> Coordinate -> Int
distance (Coordinate x1 y1) (Coordinate x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

distanceFromOrigin :: Coordinate -> Int
distanceFromOrigin = distance (Coordinate 0 0)