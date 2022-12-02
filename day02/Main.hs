module Main where

import Data.List
import Debug.Trace

main :: IO ()
main = do
  lines <- getLines "input-a.txt"
  let
    strategiesA = map parseStrategy lines
    strategiesB = map parseStrategyB lines
    scoreA = sum $ map scoreStrategy strategiesA
    scoreB = sum $ map scoreStrategy $ map choosePlay strategiesB
  putStrLn $ show scoreA
  putStrLn $ show scoreB

getLines = fmap lines . readFile

data Choice = Rock | Paper | Scissors deriving Eq

parseTheirs :: String -> Choice
parseTheirs "A" = Rock
parseTheirs "B" = Paper
parseTheirs "C" = Scissors

parseMine :: String -> Choice
parseMine "X" = Rock
parseMine "Y" = Paper
parseMine "Z" = Scissors

scoreChoice Rock = 1
scoreChoice Paper = 2
scoreChoice Scissors = 3

data Result = Win | Lose | Draw deriving Eq

parseOutcome :: String -> Result
parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win

scoreWin Win = 6
scoreWin Draw = 3
scoreWin Lose = 0

type Strategy = (Choice, Choice)
type StrategyOutcome = (Choice, Result)

(<<<) :: Choice -> Choice -> Result
theirs <<< mine | theirs == mine = Draw
Rock <<< Paper = Win
Paper <<< Scissors = Win
Scissors <<< Rock = Win
theirs <<< mine = Lose

parseStrategy :: String -> Strategy
parseStrategy str =
  let
    [theirs, mine] = words str
  in
    (parseTheirs theirs, parseMine mine)

parseStrategyB :: String -> StrategyOutcome
parseStrategyB str =
  let
    [theirs, outcome] = words str
  in
    (parseTheirs theirs, parseOutcome outcome)

scoreStrategy :: Strategy -> Int
scoreStrategy (theirs, mine) = scoreChoice mine + scoreWin (theirs <<< mine)

choosePlay :: StrategyOutcome -> Strategy
choosePlay (theirs, outcome) =
  (theirs,
   head [mine | mine <- [Rock, Paper, Scissors], theirs <<< mine == outcome]
  )
