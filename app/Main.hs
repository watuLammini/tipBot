module Main where

import Lib
import Types

main :: IO ()
main = do
  putStrLn "Die Wahrscheinlichkeit, dass das nächste Spiel des FC Bayern 0:1 ausgeht beträgt: "
  putStr $ show testProb01
