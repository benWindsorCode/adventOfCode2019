module DayThree
    (
    ) where

import Data.List.Split
import Data.Set (Set, fromList, elems, intersection)

data Direction = U | D | L | R deriving (Show, Eq, Read)

data Move = Move {
    direction :: Direction,
    length :: Int
} deriving (Show)

data Coordinate = Coordinate Int Int deriving (Show, Eq, Ord)

type Path = [Coordinate]

-- This will calculate the part one solution of min dist to intersection from origin
calculateSolution :: (String, String) -> Int
calculateSolution inputStrings = shortestDist crosses
    where
        paths = parseInput inputStrings
        firstPath = fst paths
        secondPath = snd paths
        crosses = filter (\coord -> not (coord == (Coordinate 0 0))) (crossPoints firstPath secondPath)

-- This will calculate part two solution of min length along wires to an intersection point
calculateSolution2 :: (String, String) -> Int
calculateSolution2 inputStrings = minTravelToIntersection paths
    where paths = parseInput inputStrings 

parseInput :: (String, String) -> (Path, Path)
parseInput (first, second) = (pathFromOrigin moves1, pathFromOrigin moves2)
    where
        moves1 = map parseMove (splitOn "," first)
        moves2 = map parseMove (splitOn "," second)

parseMove :: String -> Move
parseMove inputString = Move (read firstChar :: Direction) (read rest :: Int)
        where
            firstChar = [head inputString]
            rest = tail inputString

minTravelToIntersection :: (Path, Path) -> Int
minTravelToIntersection (path1, path2) = foldr1 min distances
    where
        crosses = filter (\coord -> not (coord == (Coordinate 0 0))) (crossPoints path1 path2)
        distances = [totalDistanceAlongWires (path1, path2) x | x <- crosses]

-- Given two wires and a point on them both, total the distance each wire will take
totalDistanceAlongWires :: (Path, Path) -> Coordinate -> Int
totalDistanceAlongWires (path1, path2) coord = (distanceToPoint path1 coord) + (distanceToPoint path2 coord)

-- Given a path and a point on that path, calculate how far along the path you have to go to get to that point
distanceToPoint :: Path -> Coordinate -> Int
distanceToPoint path coord = Prelude.length $ takeWhile (/= coord) path

shortestDist :: [Coordinate] -> Int
shortestDist coords = foldr1 min distances
        where distances = map distanceFromOrigin coords

crossPoints :: Path -> Path -> [Coordinate]
crossPoints firstPath secondPath = elems $ intersection (fromList firstPath) (fromList secondPath)

-- Use currying to create a path from origin function, by passing only one argument to the path function
pathFromOrigin :: [Move] -> Path
pathFromOrigin = path (Coordinate 0 0)

-- Recursively join together sections of the path, where the next section starts where the previous ends
path :: Coordinate -> [Move] -> Path
path _ [] = []
path start (move:remainder) = joinPaths pathPiece (path newStart remainder)
        where
            pathPiece = singlePath move start
            newStart = last pathPiece

-- Helper function with avoid repetition of the end of first path and start of second path when we join
joinPaths :: Path -> Path -> Path
joinPaths firstPath [] = firstPath
joinPaths [] secondPath = secondPath
joinPaths firstPath secondPath = firstPath ++ (tail secondPath)

singlePath :: Move -> Coordinate -> Path
singlePath (Move direction length) startCoord = singlePathHelper direction length startCoord [startCoord]

singlePathHelper :: Direction -> Int -> Coordinate -> Path -> Path
singlePathHelper _ 0 _ coords = coords
singlePathHelper direction remainingDist currentCoord coords 
        | direction == U = singlePathHelper direction (remainingDist-1) (moveUp currentCoord) (coords ++ [moveUp currentCoord])
        | direction == D = singlePathHelper direction (remainingDist-1) (moveDown currentCoord) (coords ++ [moveDown currentCoord])
        | direction == L = singlePathHelper direction (remainingDist-1) (moveLeft currentCoord) (coords ++ [moveLeft currentCoord])
        | direction == R = singlePathHelper direction (remainingDist-1) (moveRight currentCoord) (coords ++ [moveRight currentCoord])

moveUp :: Coordinate -> Coordinate
moveUp (Coordinate x y) = Coordinate x (y+1)

moveDown :: Coordinate -> Coordinate
moveDown (Coordinate x y) = Coordinate x (y-1)

moveLeft :: Coordinate -> Coordinate
moveLeft (Coordinate x y) = Coordinate (x-1) y

moveRight :: Coordinate -> Coordinate
moveRight (Coordinate x y) = Coordinate (x+1) y

distance :: Coordinate -> Coordinate -> Int
distance (Coordinate x1 y1) (Coordinate x2 y2) = (abs (x1-x2)) + (abs (y1-y2))

distanceFromOrigin :: Coordinate -> Int
distanceFromOrigin = distance (Coordinate 0 0)