module Lib
    ( someFunc,
      getNormDistrProb,
      getCategorialProb
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getCategorialProb :: Double -> Double -> Double
getCategorialProb numberOfUnion totalNumber = numberOfUnion / totalNumber

getNormDistrProb :: Double -> Double -> Double -> Double
getNormDistrProb x mean sd = exp (-((x-mean)^2 / (2*sd*sd))) / sqrt (2*sd*sd*pi)

