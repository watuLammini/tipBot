module Types
  ( Team
  ) where

import qualified Data.Map as Map

data Team = Team {
  name :: String,
  points :: Int
  }

-- Test data

fcb = Team {
  name="FC Bayern",
  points=82
}

testMap = Map.insert (name fcb) fcb Map.empty

lookMeUp = case (Map.lookup "FC Bayern" testMap) of Just team -> name team
