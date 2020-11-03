{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Lens.TH
--import qualified Data.Vector as V
--import qualified Statistics.Distribution.Normal as Nor

data Team = Team {
  name :: String,
  points2019 :: Int,
  points2018 :: Int
  } deriving Show

data LTeam = LTeam {
  _lname :: String,
  _lpoints2019 :: Int,
  _lpoints2018 :: Int
  } deriving Show

data LTeam' = LTeam' {
  _lname' :: String,
  _lpoints' :: Map.Map Int Int
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

data LGame = LGame {
  _lgameID :: Int,
  _lteam1 :: String,
  _lteam2 :: String,
  _lgoalsT1 :: Int,
  _lgoalsT2 :: Int,
  _lspieltag :: Int,
  _lsaison :: Int
} deriving Show

newtype Teams = Teams { getTeams :: Map.Map String Team } deriving Show
newtype LTeams = LTeams { _getLTeams :: Map.Map String LTeam } deriving Show
newtype Games = Games { getGames :: Map.Map String Game } deriving Show
newtype Games' = Games' { getGames' :: Map.Map String Game' } deriving Show
newtype LGames = LGames { _getLGames :: Map.Map String LGame } deriving Show
newtype LTeams' = LTeams' { _getLTeams' :: Map.Map String LTeam' } deriving Show

makeLenses ''LTeam
makeLenses ''LTeam'
makeLenses ''LTeams
makeLenses ''LTeams'
makeLenses ''LGame
makeLenses ''LGames

getResult :: Game -> String
getResult Game { goalsT1=goalsT1, goalsT2=goalsT2 } = show goalsT1 ++ ":" ++ show goalsT2

getResult' :: Game' -> String
getResult' Game' { goalsT1'=goalsT1', goalsT2'=goalsT2' } = show goalsT1' ++ ":" ++ show goalsT2'

getLResult :: LGame -> String
getLResult LGame { _lgoalsT1=_lgoalsT1, _lgoalsT2=_lgoalsT2 } = show _lgoalsT1 ++ ":" ++ show _lgoalsT2

getPoints :: Game -> Int
getPoints Game { goalsT1=goalsT1, goalsT2=goalsT2 }
  | goalsT1 > goalsT2 = 3
  | goalsT1 < goalsT2 = 0
  | otherwise = 1