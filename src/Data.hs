{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import Types
import Games
import Teams
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Maybe
import Data.Either
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import GHC.Generics
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BCh8
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Network.HTTP.Req
import Debug.Trace
-- Lens Part

instance FromJSON LTeams where
  parseJSON = withArray "Teams" $ \array -> do
    parsedArray <- V.mapM parseLTeam array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (_lname team) team theMap)
                              Map.empty parsedArray
    return $ LTeams parsedMap

parseLTeam :: Value -> Parser LTeam
parseLTeam = withObject "LTeam" (\o -> do
 _lname <- o .: "TeamName"
 _lpoints2019 <- o .: "Points"
 let _lpoints2018 = 0
 return LTeam {..})

decodeLTeams :: IO LTeams
decodeLTeams = do
  object2018 <- decodeLJSON 2018
  object2019 <- decodeLJSON 2019
  let insertUpdate newTeam = set lpoints2018 (view lpoints2019 newTeam)
  let t2018 = case object2018 of
                Left error -> LTeams { _getLTeams = Map.empty }
                Right teams -> teams
  let t2019 = case object2019 of
                Left error -> LTeams { _getLTeams = Map.empty }
                Right teams -> teams
-- With help from https://www.reddit.com/r/haskell/comments/7l0qzb/hashmap_insertwith_lens_equivalent/
  let result = Map.foldr (\team map -> over (at (_lname team)) (Just . maybe team (insertUpdate team)) map)
                 (_getLTeams t2019) (_getLTeams t2018)
  return $ LTeams result

decodeLJSON year = do
  json <- getJSON year
  let jsonEitherValue = eitherDecode json :: Either String Value
  case jsonEitherValue of
    Left error -> putStr error
    Right value -> return ()
  let jsonValue = case jsonEitherValue of
                    Left error -> Null
                    Right value -> value
  return (parseEither parseJSON jsonValue :: Either String LTeams)


-- Some demo code for myself
--t1 = LTeam "fcb" 1 2
--t2 = LTeam "freunde" 3 4
--set lname "hi" t1
--over lname (const "hi") t1
--lt = LTeams (Map.insert "Test"< t1 Map.empty)
--view (at "Test") (_getLTeams lt)
--set (at "Test") (Just t2) (_getLTeams lt)
--over (at "Test") (const $ Just t2) (_getLTeams lt)
--over (at "Test") (\t1 -> case t1 of (Just tt1) -> Just $ tt1 { _lpoints2019 = 66 }; Nothing -> Just t2) (_getLTeams lt)

viewTestTeam = do
  teams <- finalTeams
  let fcb = fromMaybe (Team "" Map.empty) $ view (at "FC Bayern") (_getTeams teams)
--  let result = view points fcb
  let result = (view (at "FC Bayern") (_getTeams teams)) >>= (\x -> Just $ view points x)
  return result
-- End Lens Part


instance FromJSON TeamsOld where
  parseJSON = withArray "TeamsOld" $ \array -> do
    parsedArray <- V.mapM parseTeamOld array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (nameOld team) team theMap) Map.empty
                      parsedArray
    return $ TeamsOld parsedMap

instance FromJSON Games' where
  parseJSON = withArray "Games'" $ \array -> do
    parsedArray <- V.mapM parseGame' array
    let parsedMap = V.foldr (\game theMap -> Map.insert (show (gameID' game)) game theMap) Map.empty parsedArray
    return $ Games' parsedMap

parseTeamOld :: Value -> Parser TeamOld
parseTeamOld = withObject "TeamOld" (\o -> do
  nameOld <- o .: "TeamName"
  points2019 <- o .: "Points"
  let points2018 = 0
  return TeamOld {..})

parseGame' :: Value -> Parser Game'
parseGame' = withObject "Game" (\o -> do
  gameID' <-  o .: "MatchID" :: Parser Int
  team1Object <- (o .: "Team1") :: Parser Object
  team1' <- team1Object .: "TeamName"
  team2Object <- o .: "Team2" :: Parser Object
  team2' <- team2Object .: "TeamName" :: Parser String
  results <- o .: "MatchResults" :: Parser Array
  let resultsObject = V.head results
  (goalsT1', goalsT2') <-
    withObject "resultsObject"
      (\o -> do
               pointsTeam1 <- o .: "PointsTeam1" :: Parser Int
               pointsTeam2 <- o .: "PointsTeam2" :: Parser Int
               return (pointsTeam1, pointsTeam2)) resultsObject
  let spieltag' = 1
  let saison' = 2019
  return Game' {..})

jsonPath :: Int -> FilePath
jsonPath year =  "src/data/table" ++ (show year) ++ ".json"

getJSON :: Int -> IO B.ByteString
getJSON year = B.readFile $ jsonPath year

decodeJSON year = do
  json <- getJSON year
  let jsonEitherValue = eitherDecode json :: Either String Value
  case jsonEitherValue of
    Left error -> putStr error
    Right value -> return ()
  let jsonValue = case jsonEitherValue of
                    Left error -> Null
                    Right value -> value
  return (parseEither parseJSON jsonValue :: Either String TeamsOld)

decodeTeamsOld :: IO TeamsOld
decodeTeamsOld = do
  object2018 <- decodeJSON 2018
  object2019 <- decodeJSON 2019
  let insertUpdate newTeam oldTeam = oldTeam { points2018 = (points2019 newTeam) }
  let t2018 = case object2018 of
                Left error -> TeamsOld { getTeamsOld = Map.empty }
                Right teams -> teams
  let t2019 = case object2019 of
                Left error -> TeamsOld { getTeamsOld = Map.empty }
                Right teams -> teams
  let result = Map.foldr (\team theMap -> Map.insertWith insertUpdate (nameOld team) team theMap)
                 (getTeamsOld t2019) (getTeamsOld t2018)
  return $ TeamsOld result

jsonGamePath :: FilePath
jsonGamePath = "src/data/match201901.json"

decodeGamesIt = do
  jsonBString <- B.readFile jsonGamePath
  let json = eitherDecode jsonBString :: Either String Games'
  return json

decodeGames :: IO Games'
decodeGames = do
  decoded <- decodeGamesIt
  let games = case decoded of
                Left error -> Games' Map.empty
                Right g -> g
  return games
-- Test data

fcb = TeamOld {
  nameOld = "FC Bayern",
  points2019 = 82,
  points2018 = 78
}

lfcb = LTeam {
  _lname = "FC Bayern",
  _lpoints2019 = 82,
  _lpoints2018 = 78
}

dortmund = TeamOld {
  nameOld = "Borussia Dortmund",
  points2019 = 69,
  points2018 = 76
}

ldortmund = LTeam {
  _lname = "Borussia Dortmund",
  _lpoints2019 = 69,
  _lpoints2018 = 76
}


hoffenheim = TeamOld {
  nameOld = "TSG 1899 Hoffenheim",
  points2019 = 52,
  points2018 = 51
}

leverkusen = TeamOld {
  nameOld = "Bayer Leverkusen",
  points2019 = 63,
  points2018 = 58
}

mainz = TeamOld {
  nameOld = "1. FSV Mainz 05",
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
  goalsT1 = 0,
  goalsT2 = 1,
  spieltag = 34,
  saison = 2019
}

dummyGames = Map.insert (gameID g9991) g9991 .
             Map.insert (gameID g9992) g9992 .
             Map.insert (gameID g9993) g9993 .
             Map.insert (gameID g9994) g9994 $ Map.empty

dummyTeams = TeamsOld $ Map.insert (nameOld dortmund) dortmund .
             Map.insert (nameOld fcb) fcb .
             Map.insert (nameOld hoffenheim) hoffenheim $ Map.empty

dummyLTeams = LTeams $ Map.insert (view lname lfcb) lfcb .
              Map.insert (view lname ldortmund) ldortmund $ Map.empty

dummyResultProbs = Map.insert "1:1" 0.1173 .
                   Map.insert "2:1" 0.0879 .
                   Map.insert "1:0" 0.0813 .
                   Map.insert "2:0" 0.0794 .
                   Map.insert "0:1" 0.0578 $ Map.empty

dummyResults = map getData (Map.elems dummyGames)
                 where getData game = (getResult game, team1 game)
--                 [("Team", (name (team1 game))), ("Result", (getResult game))]

dummyResults' = [(getData game) | game <- (Map.elems dummyGames)]
                where getData game = (getResult game, team1 game)

dummyResults'' = foldl (\map value -> Map.insert ("Test" ++ value) value map) Map.empty (map getResult
                  (Map.elems dummyGames))

dummyResults''' = Map.foldlWithKey (\map key game -> Map.insertWith (\newGames oldGames -> oldGames ++ newGames)
                    (getResult game) [(nameOld $ team1 game)] map) Map.empty dummyGames
-- Key wird gar nicht gebraucht...

dummyResults'''' = Map.foldr (\game map -> Map.insertWith (\new old -> new ++ old)
                     (getResult game) [(team1 game)] map) Map.empty dummyGames

dummyResults''''' = do
  games <- decodeGames
  let result = Map.foldr (\game map -> Map.insertWith (\new old -> new ++ old)
                           (getResult' game) [(team1' game)] map) Map.empty (getGames' games)
  return result

kindaRealResults = do
  games <- getGamesApi 2019 2
  let result = Map.foldr (\game map -> Map.insertWith (\new old -> new ++ old)
                         (getLResult game) [(_lteam1 game)] map) Map.empty (_getLGames games)
  return result