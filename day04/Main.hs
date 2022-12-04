module Main where

import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    items = map parseItem lines
  putStrLn $ show $ count contained items
  putStrLn $ show $ count overlap items

getLines = fmap lines . readFile

type Range = (Int, Int)

parseItem :: String -> [Range]
parseItem = map parseRange . splitOn ","

parseRange :: String -> Range
parseRange s =
  let
    [a, b] = splitOn "-" s
  in
    (read a, read b)

contained :: [Range] -> Bool
contained [(a1, a2), (b1, b2)]
  = a1 <= b1 && a2 >= b2
  || a1 >= b1 && a2 <= b2

overlap :: [Range] -> Bool
overlap [(a1, a2), (b1, b2)]
  = not (a2 < b1 || b2 < a1)

count :: Show a => (a -> Bool) -> [a] -> Int
count p = (length . filter p)
