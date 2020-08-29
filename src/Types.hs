module Types where

import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution.Normal as Nor

data Team = Team {
  name :: String,
  points2019 :: Int,
  points2018 :: Int
  } deriving Show

-- Test data

fcb = Team {
  name = "FC Bayern",
  points2019 = 82,
  points2018 = 78
}

bdortm = Team {
  name = "Borussia Dortmund",
  points2019 = 69,
  points2018 = 76
}

testMap = Map.insert (name bdortm) bdortm .
          Map.insert (name fcb) fcb $ Map.empty

testResults = Map.insert "1:1" 0.1173 .
              Map.insert "2:1" 0.0879 .
              Map.insert "1:0" 0.0813 .
              Map.insert "2:0" 0.0794 $ Map.empty

lookMeUp = case (Map.lookup "FC Bayern" testMap) of Just team -> name team

-- Haut nicht hin :'(
-- points2019V = V.fromList $ (Prelude.map fromIntegral (Map.elems $ Map.map points2019 testMap) :: [Double])
-- normDistr = Distr.fromSample points2019V