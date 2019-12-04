module Main where

import DayOne
import System.IO  
import Control.Monad

main = do  
    let list = []
    handle <- openFile "DayOneData.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = wordsToInts singlewords
    print list
    let result = allFuelForShip list
    print result
    hClose handle   

wordsToInts :: [String] -> [Int]
wordsToInts = map read