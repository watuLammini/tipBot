{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Games where

import Types
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types as AT
import Data.Either
import Data.Maybe
import Control.Lens
import Network.HTTP.Req
import Control.Monad.IO.Class

instance FromJSON LGames where
  parseJSON = withArray "LGames" $ \array -> do
    parsedArray <- V.mapM parseLGame array
    let parsedMap = V.foldr (\game theMap -> Map.insert (show (view lgameID game)) game theMap) Map.empty parsedArray
    return $ LGames parsedMap

parseLGame :: Value -> Parser LGame
parseLGame = withObject "LGame" (\o -> do
  _lgameID <-  o .: "MatchID" :: Parser Int
  team1Object <- (o .: "Team1") :: Parser Object
  _lteam1 <- team1Object .: "TeamName" :: Parser String
  team2Object <- o .: "Team2" :: Parser Object
  _lteam2 <- team2Object .: "TeamName" :: Parser String
  results <- o .: "MatchResults" :: Parser Array
--  We just need the first of the results object, as the second is for the halftime
  let resultsObject = V.head results
  (_lgoalsT1, _lgoalsT2) <-
    withObject "resultsObject" (\o ->
      do
        pointsTeam1 <- o .: "PointsTeam1" :: Parser Int
        pointsTeam2 <- o .: "PointsTeam2" :: Parser Int
        return (pointsTeam1, pointsTeam2))
    resultsObject
--  Spieltag and Saison are of type Scientific, so we need to parse them to Int
  tempSpieltag <- o .: "Spieltag" :: Parser String
  let _lspieltag = read tempSpieltag :: Int
  tempSaison <- o .: "Saison" :: Parser String
  let _lsaison = read tempSaison :: Int
  return LGame {..})

getGamesApi :: Int -> Int -> IO LGames
getGamesApi year day = runReq defaultHttpConfig $ do
  js <- req GET (https "www.openligadb.de" /: "api" /: "getmatchdata" /: "bl1" /~ year /~ day) NoReqBody jsonResponse
    mempty
  let jsValue :: Value
      jsValue = responseBody js
--  Next try:
  let games :: Parser LGames
      games =
        do
          array <- parseJSON jsValue :: Parser (V.Vector (HM.HashMap Text.Text Value))
--          Alternativen:
--          array <- parseJSON jsValue :: Parser (V.Vector (HM.HashMap Text.Text Text.Text))
--          array <- parseJSON jsValue :: Parser Array
--          array <- parseJSON jsValue :: Parser (V.Vector Object)
          modifiedGames <- insertIntoGames year day array
          let parsedEither = parseEither parseJSON modifiedGames :: Either String LGames
          let parsed = case parsedEither of
                               Left error -> LGames Map.empty
                               Right teams -> teams
          let errorH =
                if isLeft parsedEither
                  then putStrLn (fromLeft "" parsedEither)
                  else return ()
          return parsed

  let parsedGamesEither = parseEither (\_ -> games) Null
  let parsedGames = case parsedGamesEither of
                       Left error -> LGames Map.empty
                       Right teams -> teams
  return parsedGames

-- Some type help for myself:
-- Value (1) = Array
-- Array = Vector Value
-- Value (2) = Object = HashMap Text Value
-- Value (1) = Vector (HashMap Text Value)

insertIntoGames :: Int -> Int -> (V.Vector (HM.HashMap Text.Text Value)) -> Parser Value
insertIntoGames year day x =
  let insertSpieltag = HM.insert "Spieltag" (AT.String $ Text.pack $ show day)
                        :: HM.HashMap Text.Text Value -> HM.HashMap Text.Text Value
      insertSaison = HM.insert "Saison" (AT.String $ Text.pack $ show year)
                        :: HM.HashMap Text.Text Value -> HM.HashMap Text.Text Value
      inserted :: V.Vector Object
      inserted = V.map insertSpieltag $
                  V.map insertSaison x
      insertedObject = V.map (\v -> Object v) inserted
  in
    return $ Array insertedObject
-- Alternative 1: return $ Array insertedObject
-- Dann ist die Typsignatur Parser Value
-- Alternative 2: return $ insertedObject
-- Dann ist die Typsignatur Parser Array

finalGames :: [Int] -> IO LGames
finalGames saisons = do
  let rawGamesFunction :: [(Int -> IO LGames)]
      rawGamesFunction = map getGamesApi saisons
      rawGames :: [IO LGames]
      rawGames = rawGamesFunction <*> [1..34]
  games :: [LGames] <- sequence rawGames
  let gamesMap :: [Map.Map String LGame]
      gamesMap = map _getLGames games
--  let foldedGames = foldr (\game map -> Map.insert (_lgameID game) game)
--                      Map.empty gamesMap
      gamesUnited = LGames $ Map.unions gamesMap
  return gamesUnited

results = do
  games <- finalGames [2018..2019]
  let result = Map.foldr (\game map -> Map.insertWith (\new old -> new ++ old)
                           (getLResult game) [(_lteam1 game)] map) Map.empty (_getLGames games)
  return result