{-# LANGUAGE OverloadedStrings #-}

module Warp.Storage (
      loadDB
    , saveDB 
    , addPoint 
    , removePoint
    , lookupPoint
    , renamePoint 
) where

import Warp.Types 
import Warp.Config 
import Data.Aeson (encode, decodeFileStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import System.Directory 
import Data.Maybe (fromMaybe)

------------------------------
-- Load and Save 
------------------------------

loadDB :: IO WarpDB
loadDB = do 
    pth <- warpConfigPath
    _ <- ensureDB pth
    exists <- doesFileExist pth
    if exists 
      then fromMaybe [] <$> decodeFileStrict pth
      else saveDB [] >> return []

saveDB :: WarpDB -> IO ()
saveDB db = do 
    pth <- warpConfigPath
    _ <- ensureDB pth
    BL.writeFile pth (encode db)

------------------------------
-- Operations on WarpDB
------------------------------

addPoint :: WarpPoint -> IO () 
addPoint wp = do 
    db <- loadDB
    -- Remove any values with the same name:
    let filtered = filter ((/= name wp) . name) db
    saveDB (wp : filtered)

removePoint :: T.Text  -> IO ()
removePoint nm = do 
    db <- loadDB 
    -- Select only points which do not match given name
    let filtered = filter ((/= nm) . name) db
    saveDB filtered

lookupPoint :: T.Text -> IO (Maybe WarpPoint)
lookupPoint nm = do findName nm <$> loadDB 

renamePoint :: T.Text -> T.Text -> IO Bool
renamePoint old new = do 
    db <- loadDB 
    case break ((==old) . name) db of 
        (_, []) -> return False
        (before, wp:after) -> do 
            let renamed = wp {name = new}
            saveDB (before ++ renamed:after)
            return True

------------------------------
-- Helper Functions
------------------------------

-- Search through WarpDB until name is found, 
-- or none match 
findName :: T.Text -> WarpDB -> Maybe WarpPoint
findName nm [] = Nothing
findName nm (x:xs) 
    | name x == nm = Just x
    | otherwise    = findName nm xs

