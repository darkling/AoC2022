module Main where

import Data.List (mapAccumL)
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    ins = (map parseInstruction lines) ++ [Halt]
    state = traceShowId $ (makeIntervals . ((0, 1):) . trackState 1 . ticks) ins
    stateSamples = sample state (takeWhile (<= 220) [20, 60..])
    result = (sum . map (uncurry (*))) stateSamples
  putStrLn $ show $ result

getLines = fmap lines . readFile

data Instruction =
  Noop
  | AddX Int
  | Halt
  deriving Show

parseInstruction l =
  let
    w = words l
  in
    case head w of
      "noop" -> Noop
      "addx" -> AddX ((read . head . tail) w)

-- The time that the instruction takes effect
-- Note that clk=1 is the start of the second cycle
ticks :: [Instruction] -> [(Int, Instruction)]
ticks is =
  let
    fn clk Noop = clk + 1
    fn clk (AddX _) = clk + 2
    fn clk Halt = clk + 100000
    times = scanl fn 0 is
  in
    zip (tail times) is

-- The state is simply the value of the rx register
trackState :: Int -> [(Int, Instruction)] -> [(Int, Int)]
trackState rx is =
  let
    fn rx' (clk, Noop) = rx'
    fn rx' (clk, AddX n) = rx'+n
    fn rx' (clk, Halt) = rx'
    rxs = scanl fn rx is
  in
    zip (map fst is) (tail rxs)

makeIntervals :: [(Int, Int)] -> [((Int, Int), Int)]
makeIntervals =
  let
    fn (t, rx) (t', rx') = ((t', rx'), ((t, t'), rx))
  in
    snd . (mapAccumL fn (0, 1))

sample :: [((Int, Int), Int)] -> [Int] -> [(Int, Int)]
sample [] _ = []
sample _ [] = []
sample (((u, u'), rx):ss) (t:ts)
  | u < t && t <= u' = (t, rx):(sample ss ts)
  | otherwise = sample ss (t:ts)
