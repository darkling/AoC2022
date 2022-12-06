module Main where

import Data.List (take)
import Data.Set (fromList, size)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    line = head lines
    pkt = findPacketStart line
    msg = findMessageStart line
  putStrLn $ show pkt
  putStrLn $ show msg

getLines = fmap lines . readFile

findPacketStart :: [Char] -> Int
findPacketStart = findSeqStart 4 4

findMessageStart :: [Char] -> Int
findMessageStart = findSeqStart 14 14

findSeqStart :: Int -> Int -> [Char] -> Int
findSeqStart seq pos cs
  | seq == (size $ fromList $ take seq cs)
  = pos
  | otherwise
  = findSeqStart seq (pos+1) $ tail cs
