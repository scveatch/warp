
module Warp.Config (warpConfigPath, ensureDB) where 

import System.Directory
import System.FilePath (takeDirectory, (</>))
import Warp.Types
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL


warpConfigPath :: IO FilePath
warpConfigPath = do 
    home <- getHomeDirectory 
    pure (home </> ".warp/points.json")

ensureDB :: FilePath -> IO ()
ensureDB pth = do
    let dir = takeDirectory pth
    createDirectoryIfMissing True dir
    exists <- doesFileExist pth
    if not exists
        then BL.writeFile pth (encode ([] :: WarpDB))  -- empty DB
        else return ()

