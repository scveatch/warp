{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Warp.Types (WarpPoint(..), WarpDB) where 

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data WarpPoint = WarpPoint
  { name :: Text,
    path :: FilePath
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type WarpDB = [WarpPoint]
