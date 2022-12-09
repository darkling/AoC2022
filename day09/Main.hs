module Main where

import Data.List (mapAccumL)
import Data.Set (Set, insert, empty, size)
import Data.Tuple.Extra (dupe)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    ins = parseInstructions lines
    (_, _, positionsA) = runModel ins ((0, 0), replicate 1 (0, 0), empty)
    (_, _, positionsB) = runModel ins ((0, 0), replicate 9 (0, 0), empty)
  putStrLn $ show $ size positionsA
  putStrLn $ show $ size positionsB

getLines = fmap lines . readFile

data Instruction = IUp | IDown | ILeft | IRight deriving Show
type Position = (Int, Int)
type State = (Position, [Position], Set Position)

parseInstructions = (concat . map parseInstruction)
parseInstruction line =
  let
    [d', n'] = words line
    d = mapToInstr d'
    n = read n' :: Int
  in
    replicate n d

mapToInstr "U" = IUp
mapToInstr "D" = IDown
mapToInstr "L" = ILeft
mapToInstr "R" = IRight

runModel [] state = state
runModel (i:is) (hp, ks, seen) =
  let
    hp' = move i hp
    accFn h k = dupe (follow h k)
    (k9', ks') = mapAccumL accFn hp' ks
    seen' = insert k9' seen
  in
    runModel is (hp', ks', seen')

move IUp (x, y) = (x, y+1)
move IDown (x, y) = (x, y-1)
move ILeft (x, y) = (x-1, y)
move IRight (x, y) = (x+1, y)

follow (hx, hy) (tx, ty)
  | abs (hx-tx) <= 1 && abs (hy-ty) <= 1 = (tx, ty)
  | otherwise = (tx + signum (hx-tx), ty + signum (hy-ty))
