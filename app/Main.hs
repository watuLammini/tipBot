module Main where

import Lib
import Types
import Numeric

main :: IO ()
main = do
  putStr "Die Wahrscheinlichkeit, dass das nächste Spiel des FC Bayern 0:1 ausgeht beträgt: "
  putStr $ showFFloat (Just 10) (testProb01*100) "%"
  putStrLn ""
