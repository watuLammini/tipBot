{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import Types
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
--import qualified Text.URI as URI
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

instance FromJSON LTeams' where
  parseJSON = withArray "LTeams'" $ \array -> do
    parsedArray <- V.mapM parseLTeam' array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (_lname' team) team theMap)
                              Map.empty parsedArray
    return $ LTeams' parsedMap

parseLTeam' :: Value -> Parser LTeam'
parseLTeam' = withObject "LTeam'" (\o -> do
 _lname' <- o .: "TeamName"
 _lpointsRaw' <- o .: "Points"
 let _lpoints' = Map.insert 0 _lpointsRaw' Map.empty
 return LTeam' {..})

decodeLTeams' :: Int -> IO LTeams'
decodeLTeams' year = do
  decodedRaw <- decodeLJSON' year
  let insertUpdate newTeam = set lpoints2018 (view lpoints2019 newTeam)
  let decoded = case decodedRaw of
                Left error -> LTeams' { _getLTeams' = Map.empty }
                Right teams -> teams
-- With help from https://www.reddit.com/r/haskell/comments/7l0qzb/hashmap_insertwith_lens_equivalent/
-- let result = Map.foldr (\team map -> over (at (_lname team)) (Just . maybe team (insertUpdate team)) map)
--                 (_getLTeams t2019) (_getLTeams t2018)
  return decoded

decodeLJSON' year = do
  json <- getJSON year
  let jsonEitherValue = eitherDecode json :: Either String Value
  case jsonEitherValue of
    Left error -> putStr error
    Right value -> return ()
  let jsonValue = case jsonEitherValue of
                    Left error -> Null
                    Right value -> value
  return (parseEither parseJSON jsonValue :: Either String LTeams')

decodeAllLTeams' :: IO LTeams'
decodeAllLTeams' = do
    let years = [2018..2019]
    decodedList <- mapM decodeLTeams' years
    let decodedMap = Map.fromList (zip years decodedList)
    let changeLP year lp = Map.insert year (fromMaybe (-1) (view (at 0) lp)) (Map.delete 0 (lp))
    let updateTeams year teams = Map.map (\team -> set lpoints' (changeLP year (view lpoints' team)) team)
                                  (_getLTeams' teams)
    let updatedMap = Map.mapWithKey updateTeams decodedMap
    let unitedMap = Map.unionsWith
                        (\t1 t2 -> t1 { _lpoints' = Map.union (_lpoints' t1) (_lpoints' t2) })
                        (Map.elems updatedMap)
    return $ LTeams' unitedMap

getAllLTeams' :: (Int -> IO LTeams') -> [Int] -> IO LTeams'
getAllLTeams' getFunction years = do
    decodedList <- mapM getFunction years
    let decodedMap = Map.fromList (zip years decodedList)
    let changeLP year lp = Map.insert year (fromMaybe (-1) (view (at 0) lp)) (Map.delete 0 (lp))
    let updateTeams year teams = Map.map (\team -> set lpoints' (changeLP year (view lpoints' team)) team)
                                  (_getLTeams' teams)
    let updatedMap = Map.mapWithKey updateTeams decodedMap
    let unitedMap = Map.unionsWith
                        (\t1 t2 -> t1 { _lpoints' = Map.union (_lpoints' t1) (_lpoints' t2) })
                        (Map.elems updatedMap)
    return $ LTeams' unitedMap

finalTeams :: IO LTeams'
finalTeams = do
  result <- getAllLTeams' getTeamsApi [2016..2019]
  return result

getTeamsApi :: Int -> IO LTeams'
getTeamsApi year = runReq defaultHttpConfig $ do
  js <- req GET (https "www.openligadb.de" /: "api" /: "getbltable" /: "bl1" /~ year) NoReqBody jsonResponse
    mempty
  let jsValue :: Value
      jsValue = responseBody js
  let resultEither = parseEither parseJSON jsValue :: Either String LTeams'
      result = case resultEither of
        Left error -> LTeams' Map.empty
        Right teams -> teams
  if isLeft resultEither
    then liftIO $ putStrLn (fromLeft "" resultEither)
    else return ()
  return result

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
  let fcb = fromMaybe (LTeam' "" Map.empty) $ view (at "FC Bayern") (_getLTeams' teams)
--  let result = view lpoints' fcb
  let result = (view (at "FC Bayern") (_getLTeams' teams)) >>= (\x -> Just $ view lpoints' x)
  return result
-- End Lens Part


instance FromJSON Teams where
  parseJSON = withArray "Teams" $ \array -> do
    parsedArray <- V.mapM parseTeam array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (name team) team theMap) Map.empty parsedArray
    return $ Teams parsedMap

instance FromJSON Games' where
  parseJSON = withArray "Games'" $ \array -> do
    parsedArray <- V.mapM parseGame' array
    let parsedMap = V.foldr (\game theMap -> Map.insert (show (gameID' game)) game theMap) Map.empty parsedArray
    return $ Games' parsedMap

parseTeam :: Value -> Parser Team
parseTeam = withObject "Team" (\o -> do
  name <- o .: "TeamName"
  points2019 <- o .: "Points"
  let points2018 = 0
  return Team {..})

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
  return (parseEither parseJSON jsonValue :: Either String Teams)

decodeTeams :: IO Teams
decodeTeams = do
  object2018 <- decodeJSON 2018
  object2019 <- decodeJSON 2019
  let insertUpdate newTeam oldTeam = oldTeam { points2018 = (points2019 newTeam) }
  let t2018 = case object2018 of
                Left error -> Teams { getTeams = Map.empty }
                Right teams -> teams
  let t2019 = case object2019 of
                Left error -> Teams { getTeams = Map.empty }
                Right teams -> teams
  let result = Map.foldr (\team theMap -> Map.insertWith insertUpdate (name team) team theMap)
                 (getTeams t2019) (getTeams t2018)
  return $ Teams result

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

fcb = Team {
  name = "FC Bayern",
  points2019 = 82,
  points2018 = 78
}

lfcb = LTeam {
  _lname = "FC Bayern",
  _lpoints2019 = 82,
  _lpoints2018 = 78
}

dortmund = Team {
  name = "Borussia Dortmund",
  points2019 = 69,
  points2018 = 76
}

ldortmund = LTeam {
  _lname = "Borussia Dortmund",
  _lpoints2019 = 69,
  _lpoints2018 = 76
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
  goalsT1 = 0,
  goalsT2 = 1,
  spieltag = 34,
  saison = 2019
}

dummyGames = Map.insert (gameID g9991) g9991 .
             Map.insert (gameID g9992) g9992 .
             Map.insert (gameID g9993) g9993 .
             Map.insert (gameID g9994) g9994 $ Map.empty

dummyTeams = Teams $ Map.insert (name dortmund) dortmund .
             Map.insert (name fcb) fcb .
             Map.insert (name hoffenheim) hoffenheim $ Map.empty

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
                    (getResult game) [(name $ team1 game)] map) Map.empty dummyGames
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