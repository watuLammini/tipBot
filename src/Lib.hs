module Lib
    {-( someFunc,
      getNormDistrProb,
      getCategorialProb,
      getMean
    )-} where

import Types
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getCategorialProb :: Double -> Double -> Double
getCategorialProb numberOfUnion totalNumber = numberOfUnion / totalNumber

getNormDistrProb :: Double -> Double -> Double -> Double
getNormDistrProb x mean sd = exp (-((x-mean)^2 / (2*sd*sd))) / sqrt (2*sd*sd*pi)

getMean :: [Double] -> Double
getMean xs = let sum = foldr (+) 0 xs
                 len = length xs
             in sum / (fromIntegral len)

--getStandardDeviation :: [Double] -> Double -> Double
--getStandardDeviation xs mean = undefined

stan xs mean = sqrt (totalSum / (fromIntegral $ length xs))
       where innerSum = map (flip (-) mean) xs
             quadrSum = map (^2) innerSum
             totalSum = foldr (+) 0 $ quadrSum

allPoints2019 = map fromIntegral $ map points2019 $ Map.elems dummyTeams :: [Double]
allPoints2018 = map fromIntegral $ map points2018 $ Map.elems dummyTeams :: [Double]
mean2019 = getMean allPoints2019
mean2018 = getMean allPoints2018
stan2019 = stan allPoints2019 mean2019
stan2018 = stan allPoints2018 mean2018

-- Vorläufige Berechnung, es fehlt aber noch die bedingte Wahrscheinlichkeit
-- dass Mannschaft Punkte x hat gegeben Spielergebnis y
prob = (getNormDistrProb 82 mean2019 stan2019) * (getNormDistrProb 78 mean2018 stan2018) * 0.0879
-- TODO: Fehlerbehandlung!: