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
  let resultsObject = V.head results
  (_lgoalsT1, _lgoalsT2) <-
    withObject "resultsObject"
      (\o -> do
               pointsTeam1 <- o .: "PointsTeam1" :: Parser Int
               pointsTeam2 <- o .: "PointsTeam2" :: Parser Int
               return (pointsTeam1, pointsTeam2)) resultsObject
  tempSpieltag <- o .: "Spieltag" :: Parser String
  let _lspieltag = read tempSpieltag :: Int
  tempSaison <- o .: "Saison" :: Parser String
  let _lsaison = read tempSaison :: Int
  return LGame {..})

--getGamesApi :: Int -> Int -> IO [LGames]
getGamesApi :: Int -> Int -> IO LGames
getGamesApi year day = runReq defaultHttpConfig $ do
  js <- req GET (https "www.openligadb.de" /: "api" /: "getmatchdata" /: "bl1" /~ year /~ day) NoReqBody jsonResponse
    mempty
  let jsValue :: Value
      jsValue = responseBody js

--  Next try:
--  let please :: Parser (V.Vector Object)
--  let please :: Parser Value
--  let please :: Parser (Either String LGames)
  let please :: Parser LGames
      please =
        do
          array <- parseJSON jsValue :: Parser (V.Vector (HM.HashMap Text.Text Value))
--          Alternativen:
--          array <- parseJSON jsValue :: Parser (V.Vector (HM.HashMap Text.Text Text.Text))
--          array <- parseJSON jsValue :: Parser Array
--          array <- parseJSON jsValue :: Parser (V.Vector Object)
          insertedArray <- insertIntoLGames' year day array
          let pleased = parseEither parseJSON insertedArray :: Either String LGames
          let parsePleased = case pleased of
                               Left error -> LGames Map.empty
                               Right teams -> teams
          let errorH =
                if isLeft pleased
                  then putStrLn (fromLeft "" pleased)
                  else return ()
          return parsePleased
--          return insertedArray

  let pleaseResultEither = parseEither (\_ -> please) Null
  let pleaseResult = case pleaseResultEither of
                       Left error -> LGames Map.empty
                       Right teams -> teams

-- End next try :(

  let oneMore = insertIntoLGames day year jsValue
--  let oneMoreBlock = do
--                       oneMoreArray <- oneMore
--                       inserted <- insertIntoLGames' day year oneMoreArray
--                       return oneMoreArray
--  let debug = id oneMoreBlock

  let tempResult = case jsValue of
                      (Array arr) -> arr
                      _ -> V.empty
--  let insertSpieltag (Object teams) = HM.insert "Spieltag" (Text.pack $ show day) teams
--      insertSpieltag _ = HM.insert "Spieltag" (Text.pack $ show day) HM.empty
  let insertSpieltag = HM.insert "Spieltag" (Text.pack $ show day)
                       :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
  let insertSaison = HM.insert "Saison" (Text.pack $ show year)
                     :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
--      tempResults1 = V.map insertSpieltag tempResult
  let updatedVector = V.mapM parseJSON tempResult :: Parser (V.Vector (HM.HashMap Text.Text Text.Text))
  let parsed = parseEither (V.mapM parseJSON) tempResult :: Either String (V.Vector LGames)
  let finalVector = do
      realVector <- updatedVector
--      let updatedVector2 = AT.Array realVector
      let insertedVector = V.map insertSpieltag realVector
--      let resultEither = parseEither parseJSON insertedVector :: Either String LGames
--          result = case resultEither of
--            Left error -> LGames Map.empty
--            Right teams -> teams
--      if isLeft resultEither
--          then putStrLn (fromLeft "" resultEither)
--          else return ()
      return realVector
--  let resultEither = finalVector >>= (\x -> parseEither parseJSON x) :: Either String LGames
--  let resultEither = parseEither parseJSON finalVector :: Either String LGames
--      result = case resultEither of
--        Left error -> LGames Map.empty
--        Right teams -> teams
--  if isLeft resultEither
--    then liftIO $ putStrLn (fromLeft "" resultEither)
--    else return ()
  let doubleParsedResult = parseEither (\_ -> finalVector)
  let result = case parsed of
        Left error -> [LGames Map.empty]
        Right teams -> V.toList teams
--  return result
  return pleaseResult

-- Some help:
-- Value (1) = Array
-- Array = Vector Value
-- Value (2) = Object = HashMap Text Value
-- Value (1) = Vector (HashMap Text Value)


insertIntoLGames :: Int -> Int -> Value -> Parser (Array)
insertIntoLGames year day = withArray "insertIntoLGames" (\x ->
  let insertSpieltag = HM.insert "Spieltag" (Text.pack $ show day)
                      :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
      insertSaison = HM.insert "Saison" (Text.pack $ show year)
                      :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
      objected = V.mapM parseJSON x :: Parser (V.Vector Object)
--      result1 = objected >>= (map insertSpieltag (V.toList x)
  in
  return $ V.fromList [Object $ HM.fromList [("Hi", "du")]]
--  return result
  )

insertIntoLGames' :: Int -> Int -> (V.Vector (HM.HashMap Text.Text Value)) -> Parser Value
insertIntoLGames' year day x =
  let insertSpieltag = HM.insert "Spieltag" (AT.String $ Text.pack $ show day)
--                      :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
      insertSaison = HM.insert "Saison" (AT.String $ Text.pack $ show year)
--                      :: HM.HashMap Text.Text Text.Text -> HM.HashMap Text.Text Text.Text
      result1 :: V.Vector Object
      result1 = V.map insertSpieltag $
                  V.map insertSaison x
--      result2 = V.map (\v1 -> HM.map (\v2 -> String v2) v1) result1
      result3 = V.map (\v -> Object v) result1
  in
--  return $ V.fromList [Object $ HM.fromList [("Hi", "du")]]
  return $ Array result3
-- Alternative: return $ Array result3
-- Dann ist die Typsignatur Parser Value
-- Alternative: return $ result3
-- Dann ist die Typsignatur Parser Array