module Main where

import Lib
import Types
import Games
import Teams
import Numeric
import Control.Lens
import Control.Parallel
import Text.Printf
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "Enter the team, you want to know the most probable result of"
  teamName <- getLine
  putStrLn "Enter the saisons, on which the data should be based"
  saisonsInput <- getLine
  let saisons = read saisonsInput :: [Int]
  results <- (finalTeams saisons) `par` (getResults saisons)
  teams <- finalTeams saisons
  let maybeTeam = view (at teamName) (_getTeams teams)
  let makeResult team = extractIt $ head $ Map.toList $ probResult results teams team saisons
      extractIt (result, value) = "Das wahrscheinlichste Ergebnis ist " ++ result ++ " mit einem (internen) Wert von "
                                    ++ (printf "%.8f\n" $ value*100) ++ "%."
      result = case maybeTeam of
                 (Just team) -> makeResult team
                 Nothing -> "Dein Team wurde leider nicht gefunden :("
  putStrLn result
  return ()
--  putStr "Die Wahrscheinlichkeit, dass das nächste Spiel des FC Bayern 0:1 ausgeht beträgt: "
--  putStr $ showFFloat (Just 10) (testProb*100) "%"
--  putStrLn ""
