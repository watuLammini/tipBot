module Types where

import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Statistics.Distribution.Normal as Nor

data Team = Team {
  name :: String,
  points2019 :: Int,
  points2018 :: Int
  } deriving Show

data Game = Game {
  gameID :: Int,
  team1 :: Team,
  team2 :: Team,
  goalsT1 :: Int,
  goalsT2 :: Int,
  spieltag :: Int,
  saison :: Int
} deriving Show

getResult :: Game -> String
getResult Game { goalsT1=goalsT1, goalsT2=goalsT2 } = show goalsT1 ++ ":" ++ show goalsT2

getPoints :: Game -> Int
getPoints Game { goalsT1=goalsT1, goalsT2=goalsT2 }
  | goalsT1 > goalsT2 = 3
  | goalsT1 < goalsT2 = 0
  | otherwise = 1

-- Test data

fcb = Team {
  name = "FC Bayern",
  points2019 = 82,
  points2018 = 78
}

dortmund = Team {
  name = "Borussia Dortmund",
  points2019 = 69,
  points2018 = 76
}

hoffenheim = Team {
  name = "TSG 1899 Hoffenheim",
  points2019 = 52,
  points2018 = 51
}

leverkusen = Team {
  name = "Bayer Leverkusen",
  points2019 = 63,
  points2018 = 58
}

mainz = Team {
  name = "1. FSV Mainz 05",
  points2019 = 37,
  points2018 = 43
}

g9991 = Game {
  gameID = 9991,
  team1 = dortmund,
  team2 = hoffenheim,
  goalsT1 = 0,
  goalsT2 = 4,
  spieltag = 34,
  saison = 2019
}

g9993 = Game {
  gameID = 9993,
  team1 = hoffenheim,
  team2 = dortmund,
  goalsT1 = 2,
  goalsT2 = 1,
  spieltag = 17,
  saison = 2019
}

g9994 = Game {
  gameID = 9994,
  team1 = mainz,
  team2 = leverkusen,
  goalsT1 = 0,
  goalsT2 = 1,
  spieltag = 17,
  saison = 2019
}

g9992 = Game {
  gameID = 9992,
  team1 = leverkusen,
  team2 = mainz,
  goalsT1 = 1,
  goalsT2 = 0,
  spieltag = 34,
  saison = 2019
}

dummyGames = Map.insert (gameID g9991) g9991 .
             Map.insert (gameID g9992) g9992 .
             Map.insert (gameID g9993) g9993 .
             Map.insert (gameID g9994) g9994 $ Map.empty

dummyTeams = Map.insert (name dortmund) dortmund .
          Map.insert (name fcb) fcb .
          Map.insert (name hoffenheim) hoffenheim $ Map.empty

dummyResults = map getData (Map.elems dummyGames)
                 where getData game = [(getResult game, team1 game)]
--                 [("Team", (name (team1 game))), ("Result", (getResult game))]

--dummyResultsOld = foldl (\map value -> Map.insert ("Test" ++ value) value map) Map.empty (map getResult
--                  (Map.elems dummyGames))

dummyResultsNew = Map.foldlWithKey (\map key game -> Map.insertWith (\newGames oldGames -> oldGames ++ newGames)
                    (getResult game) [(name $ team1 game)] map) Map.empty dummyGames

dummyResultProbs = Map.insert "1:1" 0.1173 .
                   Map.insert "2:1" 0.0879 .
                   Map.insert "1:0" 0.0813 .
                   Map.insert "2:0" 0.0794 $ Map.empty

-- DEBUG
lookMeUp = case (Map.lookup "FC Bayern" dummyTeams) of Just team -> name team

-- Haut nicht hin :'(
-- points2019V = V.fromList $ (Prelude.map fromIntegral (Map.elems $ Map.map points2019 testMap) :: [Double])
-- normDistr = Distr.fromSample points2019V