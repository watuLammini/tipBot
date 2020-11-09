{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Teams where

import Types
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types as AT
import Data.Either
import Data.Maybe
import Control.Lens
import Network.HTTP.Req
import qualified Data.Vector as V
import Control.Monad.IO.Class
import Control.Parallel


instance FromJSON Teams where
  parseJSON = withArray "Teams" $ \array -> do
    parsedArray <- V.mapM parseLTeam' array
    let insertUpdate newTeam oldTeam = oldTeam
    let parsedMap = V.foldr (\team theMap -> Map.insertWith insertUpdate (_name team) team theMap)
                              Map.empty parsedArray
    return $ Teams parsedMap

parseLTeam' :: Value -> Parser Team
parseLTeam' = withObject "LTeam'" (\o -> do
 _name <- o .: "TeamName"
 _lpointsRaw' <- o .: "Points"
 let _points = Map.insert 0 _lpointsRaw' Map.empty
 return Team {..})

getAllTeams :: (Int -> IO Teams) -> [Int] -> IO Teams
getAllTeams getFunction years = do
    decodedList <- mapM getFunction years
    let decodedMap = Map.fromList (zip years decodedList)
    let changeLP year lp = Map.insert year (fromMaybe (-1) (view (at 0) lp)) (Map.delete 0 (lp))
    let updateTeams year teams = Map.map (\team -> set points (changeLP year (view points team)) team)
                                  (_getTeams teams)
    let updatedMap = Map.mapWithKey updateTeams decodedMap
    let unitedMap = Map.unionsWith
                        (\t1 t2 -> t1 { _points = Map.union (_points t1) (_points t2) })
                          (Map.elems updatedMap)
    return $ Teams unitedMap

getTeamsApi :: Int -> IO Teams
getTeamsApi year = runReq defaultHttpConfig $ do
  js <- req GET (https "www.openligadb.de" /: "api" /: "getbltable" /: "bl1" /~ year) NoReqBody jsonResponse
    mempty
  let jsValue :: Value
      jsValue = responseBody js
  let resultEither = parseEither parseJSON jsValue :: Either String Teams
      result = case resultEither of
        Left error -> Teams Map.empty
        Right teams -> teams
  if isLeft resultEither
    then liftIO $ putStrLn (fromLeft "" resultEither)
    else return ()
  return result

finalTeams :: [Int] -> IO Teams
finalTeams saison = do
  result <- getAllTeams getTeamsApi saison
  return result