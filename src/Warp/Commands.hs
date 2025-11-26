
module Warp.Commands (
      cmdAdd
    , cmdRemove 
    , cmdList
    , cmdResolve
) where

import Warp.Types 
import Warp.Storage
import qualified Data.Text as T
import System.Directory (makeAbsolute)

------------------------------
-- Add Warp Point
------------------------------

cmdAdd :: T.Text -> FilePath -> IO ()
cmdAdd nm pth = do 
    absPath <- makeAbsolute pth
    addPoint (WarpPoint nm absPath)

------------------------------
-- Remove Warp Point
------------------------------

cmdRemove :: T.Text -> IO ()
cmdRemove = removePoint -- delegates to Warp.Storage

------------------------------
-- List Warp Points
------------------------------

cmdList :: IO ()
cmdList = do 
    db <- loadDB
    mapM_ printWarp db

printWarp :: WarpPoint -> IO ()
printWarp wp = putStrLn $ T.unpack (name wp) ++ ": " ++ path wp

----------------------------------
-- Resolve Point for warp <point>
----------------------------------

cmdResolve :: T.Text -> IO (Maybe FilePath)
cmdResolve nm = do 
    maybeWP <- lookupPoint nm
    pure $ fmap path maybeWP


