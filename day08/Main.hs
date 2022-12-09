module Main where

import Data.List (nub, transpose, concat)
import Data.List.Index (imap, ifoldl)
import Data.Tuple (swap)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    heights = map (map ((read::String -> Int) . (\x -> [x]))) lines
    maxx = length (head heights)
    maxy = length heights
    locs = getVisibleLocs maxx maxy heights
  putStrLn $ (show . length) locs

getLines = fmap lines . readFile

visible :: [[Int]] -> [(Int, Int)]
visible = concat . (imap visibleRow)

visibleRow :: Int -> [Int] -> [(Int, Int)]
visibleRow y =
  let
    accHt (prev, res) x h
      | h > prev = (h, ((x, y):res))
      | h <= prev = (prev, res)
  in
    snd . ifoldl accHt (-1, [])

getVisibleLocs :: Int -> Int -> [[Int]] -> [(Int, Int)]
getVisibleLocs maxx maxy hs =
  let
    fs = [visible,
          (map swap . visible . transpose),
          (map (revCoordX maxx) . visible . map reverse),
          (map swap . map (revCoordX maxy) . visible . map reverse . transpose)
         ]
  in
    (nub . concat . map ($ hs)) fs

revCoordX :: Int -> (Int, Int) -> (Int, Int)
revCoordX max (x, y) = (max-x-1, y)
