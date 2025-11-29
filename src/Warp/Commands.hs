
module Warp.Commands (
      cmdAdd
    , cmdRemove 
    , cmdRename 
    , cmdList
    , cmdResolve
) where

import Warp.Types 
import Warp.Storage
import qualified Data.Text as T
import System.Directory (makeAbsolute)
import Data.Char (isAlphaNum)
import System.Exit (die)
import Control.Monad (unless)


------------------------------
-- Add Warp Point
------------------------------

cmdAdd :: T.Text -> FilePath -> IO ()
cmdAdd nm pth = do 
    absPath <- makeAbsolute pth
    if isValidName nm 
    then addPoint (WarpPoint nm absPath)
    else die "Error: WarpPoint names must have at least one AlphaNumeric character"

------------------------------
-- Remove Warp Point
------------------------------

cmdRemove :: T.Text -> IO ()
cmdRemove = removePoint -- delegates to Warp.Storage


------------------------------
-- Rename a Warp Point
------------------------------

cmdRename :: T.Text -> T.Text -> IO ()
cmdRename old new = do 
    ok <- renamePoint old new 
    unless ok $ 
        die "Error: WarpPoint not found"
    
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


----------------------------------
-- Helper Functions
----------------------------------

-- String must have at least one AlphaNumeric character
isValidName :: T.Text -> Bool
isValidName = T.all isAlphaNum 

