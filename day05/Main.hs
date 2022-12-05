module Main where

import Data.List (isPrefixOf, transpose)
import Data.List.Split (splitOn, chunksOf)
import Data.Map.Strict (Map, fromAscList, toAscList, (!), insert)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    stacks = parseStacks $ takeWhile (/= "") lines
    instructions = parseInstructions $ dropWhile (\l -> not ("move " `isPrefixOf` l)) lines
    resultA = exec moveA instructions stacks
    topsA = map (head . snd) $ toAscList resultA
    resultB = exec moveB instructions stacks
    topsB = map (head . snd) $ toAscList resultB
  putStrLn topsA
  putStrLn topsB

getLines = fmap lines . readFile

type Stack = [Char]
type Stacks = Map Int Stack
data Instruction = Instruction Int Int Int deriving Show

parseStacks :: [[Char]] -> Stacks
parseStacks =
  fromAscList . zip [1..]
   . map (reverse . filter (/= ' '))
   . transpose . map (parseStackRow)
   . tail . reverse

parseStackRow :: String -> [Char]
parseStackRow = (map (head . tail)) . (chunksOf 4)

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction l =
  let
    nums = (map (head . tail) . (chunksOf 2) . (splitOn " ")) l
    (n, s, d) = listToTriple $ map (read :: String -> Int) nums
  in
    Instruction n s d

exec :: (Int -> Stack -> Stack -> (Stack, Stack)) -> [Instruction] -> Stacks -> Stacks
exec mv [] ss = ss
exec mv ((Instruction n s d):is) ss =
  let
    src = ss ! s
    dst = ss ! d
    (newSrc, newDst) = mv n src dst
    ss' = insert s newSrc $ insert d newDst ss
  in
    exec mv is ss'

moveA :: Int -> [a] -> [a] -> ([a], [a])
moveA 0 src dst = (src, dst)
moveA n (x:src) dst = moveA (n-1) src (x:dst)

moveB :: Int -> [a] -> [a] -> ([a], [a])
moveB n src dst =
  let
    (prefix, suffix) = splitAt n src
  in
    (suffix, prefix ++ dst)

listToTriple (a:b:c:_) = (a, b, c)
listToTriple _ = (-1, -1, -1)
