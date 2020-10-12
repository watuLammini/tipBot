module Lib
    {-( someFunc,
      getNormProb,
      getCategorialProb,
      getMean
    )-} where

import Types
import Data
import qualified Data.Map as Map

getCategorialProb :: Double -> Double -> Double
getCategorialProb numberOfUnion totalNumber = numberOfUnion / totalNumber

getNormProb :: Double -> Double -> Double -> Double
getNormProb x mean sd = exp (-((x-mean)^2 / (2*sd*sd))) / sqrt (2*sd*sd*pi)

getNormProbSample :: Double -> [Double] -> Double
getNormProbSample x xs = getNormProb x mean sd
                         where mean = getMean xs
                               sd = getStan xs

getMean :: [Double] -> Double
getMean xs = let sum = foldr (+) 0 xs
                 len = length xs
             in sum / (fromIntegral len)

getStan xs = sqrt (totalSum / (fromIntegral $ length xs))
       where mean = getMean xs
             innerSum = map (flip (-) mean) xs
             quadrSum = map (^2) innerSum
             totalSum = foldr (+) 0 $ quadrSum

allPoints2019 = map fromIntegral $ map points2019 $ Map.elems $ getTeams dummyTeams :: [Double]
allPoints2018 = map fromIntegral $ map points2018 $ Map.elems $ getTeams dummyTeams :: [Double]
mean2019 = getMean allPoints2019
mean2018 = getMean allPoints2018
stan2019 = getStan allPoints2019
stan2018 = getStan allPoints2018

-- Vorläufige Berechnung
testProb01 =   (getNormProbSample (fromIntegral $ points2019 bayern) probPoints201901)
             * (getNormProbSample (fromIntegral $ points2018 bayern) probPoints201801)
             * (dummyResultProbs Map.! "0:1")
           where bayern = getTeams dummyTeams Map.! "FC Bayern"
                 probPoints201901 = map fromIntegral $ map points2019 (dummyResults'''' Map.! "0:1")
                 probPoints201801 = map fromIntegral $ map points2018 (dummyResults'''' Map.! "0:1")
-- TODO: Fehlerbehandlung!