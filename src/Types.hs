module Types where

import qualified Data.Map.Strict as Map
--import qualified Data.Vector as V
--import qualified Statistics.Distribution.Normal as Nor

data Team = Team {
  name :: String,
  points2019 :: Int,
  points2018 :: Int
  } deriving Show

data TeamNew = TeamNew {
  nameNew :: String,
  points :: Map.Map Int Int
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

data Game' = Game' {
  gameID' :: Int,
  team1' :: String,
  team2' :: String,
  goalsT1' :: Int,
  goalsT2' :: Int,
  spieltag' :: Int,
  saison' :: Int
} deriving Show

newtype Teams = Teams { getTeams :: Map.Map String Team } deriving Show
newtype Games = Games { getGames :: Map.Map String Game } deriving Show
newtype Games' = Games' { getGames' :: Map.Map String Game' } deriving Show


getResult :: Game -> String
getResult Game { goalsT1=goalsT1, goalsT2=goalsT2 } = show goalsT1 ++ ":" ++ show goalsT2

getPoints :: Game -> Int
getPoints Game { goalsT1=goalsT1, goalsT2=goalsT2 }
  | goalsT1 > goalsT2 = 3
  | goalsT1 < goalsT2 = 0
  | otherwise = 1