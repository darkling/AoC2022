module Main where

import Control.Applicative
import Data.Array (Array, array, bounds, assocs, (!), (//))
import Data.Char (ord)
import Data.List.Index (imap)
import Data.List (nub)
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    locs = parse lines
    end = head (getEnd locs)
    dists = distances locs (getStart locs)
    steps = dists ! end
  putStrLn $ show steps

getLines = fmap lines . readFile

type Pos = (Int, Int)
data LocType = E | S | N deriving Show
type Loc = (Int, Maybe Int, LocType)

parse :: [String] -> Array Pos Loc
parse lines =
  let
    maxx = (length . head) lines
    maxy = length lines
    maprow y l = imap (\x c -> ((x+1, y+1), charToLoc c)) l
    items = (concat . imap maprow) lines
  in
    array ((1, 1), (maxx, maxy)) items

charToLoc 'S' = (0, Just 0, S)
charToLoc 'E' = (25, Nothing, E)
charToLoc c = (ord c - ord 'a', Nothing, N)

getStart :: Array Pos Loc -> [Pos]
getStart =
  let
    isStart (_, _, S) = True
    isStart _ = False
  in
    (map fst) . (filter (\ (i, e) -> isStart e)) . assocs

getEnd :: Array Pos Loc -> [Pos]
getEnd =
  let
    isEnd (_, _, E) = True
    isEnd _ = False
  in
    (map fst) . (filter (\ (i, e) -> isEnd e)) . assocs

distances :: Array Pos Loc -> [Pos] -> Array Pos Loc
distances ls [] = ls
distances ls qs =
  let
    maxidx = (snd . bounds) ls
    q2n q = map (\n -> (q, n)) (nbrs maxidx q)
    ns = (concat . map q2n) qs
    updates = mapMaybe (updateFor ls) ns
    qs' = nub $ map fst updates
  in
    distances (trace (dbgFmt (ls // updates)) (ls // updates)) qs'

minDistance (h, Nothing, lt) x' = (h, Just x', lt)
minDistance (h, Just x, lt) x' = (h, Just (min x x'), lt)

updateFor :: Array Pos Loc -> (Pos, Pos) -> Maybe (Pos, Loc)
updateFor ls (p, p') =
  let
    l@(h, d, lt) = ls ! p
    l'@(h', d', lt') = ls ! p'
  in
    if (h'-h <= 1) && fromMaybe True (liftA2 (<) (fmap (1+) d) d')
    then Just (p', (h', fmap (1+) d, lt))
    else Nothing

nbrs (maxx, maxy) (x, y) =
  let
    inBounds (x', y') =
      1 <= x' && x' <= maxx
      && 1 <= y' && y' <= maxy
  in
    filter inBounds [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

dbgFmt :: Array Pos Loc -> String
dbgFmt a =
  let
    (_, (maxx, maxy)) = bounds a
    fmtCell (_, Nothing, _) = ". "
    fmtCell (_, Just d, _) = [last (show d), ' ']
    fmtLine a y =
      concat (map (\x -> fmtCell (a ! (x, y))) [1..maxx])
  in
    unlines (map (fmtLine a) [1..maxy])
