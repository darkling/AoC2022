module Main where

import Data.List
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    totals = parseTotals lines
    cals = maximum totals
    topThree = topThreeTotals totals
  putStrLn $ show cals
  putStrLn $ show topThree

getLines = fmap lines . readFile

topThreeTotals :: [Int] -> Int
topThreeTotals = sum . (take 3) . reverse . sort

parseTotals :: [String] -> [Int]
parseTotals lines =
  let
    elvesText = splitSections lines
    elves = map (map (read :: String -> Int)) elvesText
  in
    map sum elves

splitSections :: [String] -> [[String]]
splitSections = foldr splitSectionsAcc [[]]

splitSectionsAcc :: String -> [[String]] -> [[String]]
splitSectionsAcc "" acc = []:acc
splitSectionsAcc s (h:t) = (s:h):t
