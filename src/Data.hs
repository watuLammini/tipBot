{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import Types
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Maybe
import Data.Aeson.Types as AT
import qualified Data.Vector as V
import GHC.Generics
import Control.Applicative
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens

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
--  t2018 <- object2018
--  t2019 <- object2019
--  let t2018 = object2018 >>= getTeams
--  let t2019 = object2019 >>= getTeams
  let t2018 = case object2018 of
                Left error -> LTeams { _getLTeams = Map.empty }
                Right teams -> teams
  let t2019 = case object2019 of
                Left error -> LTeams { _getLTeams = Map.empty }
                Right teams -> teams
--  let result = Map.foldr (\team theMap -> Map.insertWith insertUpdate (_lname team) team theMap)
--                 (_getLTeams t2019) (_getLTeams t2018)
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
--  let result = Map.foldr (\team map -> over (at (_lname team)) (Just . maybe team (insertUpdate team)) map)
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

--decodeAllLTeams' :: IO LTeams'
decodeAllLTeams' = do
    let years = [2018..2019]
    decodedList <- mapM decodeLTeams' years
    let decodedMap = Map.fromList (zip years decodedList)
    let changeLP year lp = Map.insert year (fromMaybe (-1) (view (at 0) lp)) (Map.delete 0 (lp))
    let updateTeams year Nothing = Nothing
        updateTeams year (Just teams) = Just $ LTeams' $ Map.map
                                          (\team -> team { _lpoints' = (changeLP year (_lpoints' team))})
                                          (_getLTeams' teams)
    let updateTeams' year teams = Map.map (\team -> set lpoints' (changeLP year (view lpoints' team)) team)
                                  (_getLTeams' teams)
    let betterMap = over (at 2018) (updateTeams 2018) decodedMap
    let bettererMap = Map.mapWithKey updateTeams' decodedMap
    let muchBetterMap = Map.foldr (\newTeams oldTeams -> Map.insert newTeams)
    return bettererMap
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
-- End Lens Part


--newtype ParserT m a = ParserT { runParserT :: Parser (m a) }
--
--instance (Monad m) => Monad (ParserT m) where
--    return = lift . return
--    x >>= f = ParserT $ do
--        v <- runParserT x
--        return (runParserT (ParserT v))

instance FromJSON Teams where
  parseJSON = withArray "Teams" $ \array -> do
    parsedArray <- V.mapM parseTeam array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (name team) team theMap) Map.empty parsedArray
    return $ Teams parsedMap

instance FromJSON Games' where
  parseJSON = withArray "Games'" $ \array -> do
    parsedArray <- V.mapM parseGame' array
--    let insertUpdate newTeam oldTeam = oldTeam { points2018=666 }
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
--  goalsT2' <-
--      withObject "resultsObject"
--        (\o -> do
--                 pointsTeam2 <- o .: "PointsTeam2" :: Parser Int
--                 return pointsTeam1) resultsObject
--  goalsT2' <- (V.head results) .: "PointsTeam2"
  return Game' {..})

--instance FromJSON Games where
--  parseJSON = withArray "Games" $ \array -> do
--    parsedArray <- V.mapM parseGame array
----    let insertUpdate newTeam oldTeam = oldTeam { points2018=666 }
--    let parsedMap = V.foldr (\game theMap -> Map.insert (show (gameID game)) game theMap) Map.empty parsedArray
--    return $ Games parsedMap

--parseGame :: Value -> Parser Game
--parseGame = withObject "Game" (\o -> do
--  gameID <- o .: "MatchID" :: Parser String
--  team1Object <- (o .: "Team1") :: Parser Object
--  team1Name <- team1Object .: "TeamName" :: Parser String
--  teams <- decodeJSONs
--  let team = teams Map.! team1Name
----  team1 <- ((Map.!) team1Name') =<< (getTeams decodeJSONs)
----  team2 <- o .: "Team2" :: Parser String
--  return Game { gameID = 99999 })

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
--  t2018 <- object2018
--  t2019 <- object2019
--  let t2018 = object2018 >>= getTeams
--  let t2019 = object2019 >>= getTeams
  let t2018 = case object2018 of
                Left error -> Teams { getTeams = Map.empty }
                Right teams -> teams
  let t2019 = case object2019 of
                Left error -> Teams { getTeams = Map.empty }
                Right teams -> teams
  let result = Map.foldr (\team theMap -> Map.insertWith insertUpdate (name team) team theMap)
                 (getTeams t2019) (getTeams t2018)
--  result <- (object2018 >>= (\o18 -> object2019 >>= (\o19 -> Map.foldr (\team theMap -> Map.insertWith insertUpdate
--                                                             (name team) team theMap) o18 o19)))
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

-- DEBUG
lookMeUp = case (Map.lookup "FC Bayern" (getTeams dummyTeams)) of Just team -> name team
