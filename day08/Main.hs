module Main where

import Data.List (nub, transpose, reverse, concat, inits, tails, zipWith, maximum)
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

    hviews = findViews heights
    vviews = (transpose . findViews . transpose) heights
    views = zipWith (*) (concat hviews) (concat vviews)
  putStrLn $ (show . length) locs
  putStrLn $ (show . maximum) views

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

findViews :: [[Int]] -> [[Int]]
findViews = map viewsRow

viewsRow :: [Int] -> [Int]
viewsRow hs =
  let
    rights = rightView hs
    lefts = (reverse . rightView . reverse) hs
  in
    zipWith (*) rights lefts

rightView :: [Int] -> [Int]
rightView hs =
  let
    rhs = (tail . tails) hs
    params = zip hs rhs
    countTaller (h, hs') =
      let
        (shorter, taller) = span (< h) hs'
      in
        length shorter + if (length taller) > 0 then 1 else 0
  in
    map countTaller params
