module Lib where

import Types
import Data
import Data.Maybe
import qualified Data.Map as Map
import Control.Lens
import Control.Parallel

getCategorialProb :: Fractional a => a -> a -> a
getCategorialProb numberOfUnion totalNumber = numberOfUnion / totalNumber

getNormProb :: Floating a => a -> a -> a -> a
getNormProb x mean sd = exp (-((x-mean)^2 / (2*sd*sd))) / sqrt (2*sd*sd*pi)

getNormProbSample :: Floating a => a -> [a] -> a
getNormProbSample x xs = getNormProb x mean sd
                         where mean = getMean xs
                               sd = getStan xs

getMean :: Fractional a => [a] -> a
getMean xs = let sum = foldr (+) 0 xs
                 len = length xs
             in sum / (fromIntegral len)

getStan :: Floating a => [a] -> a
getStan xs = sqrt (totalSum / (fromIntegral $ length xs))
       where mean = getMean xs
             innerSum = map (flip (-) mean) xs
             quadrSum = map (^2) innerSum
             totalSum = foldr (+) 0 $ quadrSum

-- OLD

allPoints2019 = map fromIntegral $ map points2019 $ Map.elems $ getTeamsOld dummyTeams :: [Double]
allPoints2018 = map fromIntegral $ map points2018 $ Map.elems $ getTeamsOld dummyTeams :: [Double]
mean2019 = getMean allPoints2019
mean2018 = getMean allPoints2018
stan2019 = getStan allPoints2019
stan2018 = getStan allPoints2018

-- Vorläufige Berechnung
testProb =   (getNormProbSample (fromIntegral $ points2019 bayern) probPoints201901)
             * (getNormProbSample (fromIntegral $ points2018 bayern) probPoints201801)
             * (dummyResultProbs Map.! "0:1")
           where bayern = getTeamsOld dummyTeams Map.! "FC Bayern"
                 probPoints201901 = map fromIntegral $ map points2019 (dummyResults'''' Map.! "0:1")
                 probPoints201801 = map fromIntegral $ map points2018 (dummyResults'''' Map.! "0:1")

testProbPar :: IO ()
testProbPar = do
  let bayern = getTeamsOld dummyTeams Map.! "FC Bayern"
  let probPoints201901 = map fromIntegral $ map points2019 (dummyResults'''' Map.! "0:1")
  let probPoints201801 = map fromIntegral $ map points2018 (dummyResults'''' Map.! "0:1")
  let pPointsCondResult19 = (getNormProbSample (fromIntegral $ points2019 bayern) probPoints201901)
  let pPointsCondResult18 = (getNormProbSample (fromIntegral $ points2018 bayern) probPoints201801)
  let pPoints = (dummyResultProbs Map.! "0:1")
  let result = (pPointsCondResult19 `par` pPointsCondResult18 `par` pPoints)
                 `pseq` (pPointsCondResult19 * pPointsCondResult19 * pPoints)
  putStrLn $ show result
  return ()

-- END OLD

-- Give back all the probabilites for results
probabilitiesResults :: Map.Map String (Map.Map String Int) -> Map.Map String Double
probabilitiesResults results =
  Map.map (\noMatches -> (fromIntegral noMatches) / (fromIntegral total)) summedUp
  where
    summedUp = Map.map sumItUp results
    sumItUp = Map.foldr (+) 0
    total = Map.foldr (+) 0 summedUp

probsPointsCondResultsIt :: Map.Map String (Map.Map String Int) -> Teams -> Int -> Map.Map String [Int]
probsPointsCondResultsIt results rawTeams saison =
  Map.map (filter (/= (-99))) pointsMap
  where
    teams = _getTeams rawTeams
    pointsMap :: Map.Map String [Int]
    pointsMap = Map.map generatePointsList results
    generatePointsList :: Map.Map String Int -> [Int]
    generatePointsList = Map.foldrWithKey gPLit []
    gPLit teamName 1 list = (getPoints teamName) : list
    gPLit teamName counter list = (getPoints teamName) `par` (gPLit teamName (counter-1) list)
                                    `pseq` (getPoints teamName) : (gPLit teamName (counter-1) list)
    getPoints :: String -> Int
    getPoints teamName = getPointsIt $ view (at teamName) teams
    getPointsIt (Just team) = getPointsItIt (view points team)
    getPointsIt Nothing = -99
    getPointsItIt teamPoints = fromMaybe (-99) $ teamPoints Map.!? saison
--    getPointsItIt teamPoints = fromMaybe (-99) (view (at saison) teamPoints)

probsPointsCondResults :: Map.Map String (Map.Map String Int) -> Teams -> Team -> Int -> Map.Map String Double
probsPointsCondResults results rawTeams team saison =
  Map.map (getNormProbSample (fromIntegral pointsTeam)) pointsMap
  where
    teams = _getTeams rawTeams
    pointsMap = Map.map (map fromIntegral) $ probsPointsCondResultsIt results rawTeams saison
    pointsTeam = view points team Map.! saison

probsPointsCondResultsSaisons :: Map.Map String (Map.Map String Int) -> Teams -> Team -> [Int]
                                   -> Map.Map Int (Map.Map String Double)
probsPointsCondResultsSaisons results rawTeams team saisons =
  saisonProbs
  where
    teams = _getTeams rawTeams
    saisonProbs = Map.fromList $ zip saisons $
                    map (probsPointsCondResults results rawTeams team) saisons

probResult :: Map.Map String (Map.Map String Int) -> Teams -> Team -> [Int] -> Map.Map String Double
probResult results rawTeams team saisons =
  Map.filter (>= maximum finalProbs) finalProbs
  where
   teams = _getTeams rawTeams
   probsResults = probabilitiesResults results
   probsCondSaisons = Map.elems $ probsPointsCondResultsSaisons results rawTeams team saisons
   probsCondSaisonsUnited = Map.unionsWith (*) probsCondSaisons
   finalProbs = probsCondSaisons `par` probsCondSaisonsUnited
                  `pseq` Map.unionWith (*) probsResults probsCondSaisonsUnited

-- For testing:
-- results <- getResults
-- teams <- finalTeams
-- fcb = (_getTeams teams) Map.! "FC Bayern"
