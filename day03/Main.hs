module Main where

import Data.List (splitAt)
import Data.List.Split (chunksOf)
import Data.Set (Set, empty, take, union, intersection, fromList, toList)
import Data.Char (ord, isAsciiUpper, isAsciiLower)
import Data.Tuple (uncurry)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    itemsA = map (overlapElement . parseSack) lines
    itemsB = map uniqueElement . chunksOf 3 $ map (uncurry union . parseSack) lines
  putStrLn $ show $ score itemsA
  putStrLn $ show $ score itemsB

getLines = fmap lines . readFile

type Sack = (Set Char, Set Char)

parseSack :: String -> Sack
parseSack line =
  let
    (a, b) = splitAt ((length line) `quot` 2) line
  in
    (fromList a, fromList b)

score :: [Char] -> Int
score = sum . map scoreElement

overlapElement :: Sack -> Char
overlapElement = head . toList . uncurry intersection

uniqueElement :: [Set Char] -> Char
uniqueElement cs = head . toList $ foldr intersection (head cs) (tail cs)

scoreElement :: Char -> Int
scoreElement c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
