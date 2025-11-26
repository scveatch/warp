{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-} 

module Main (main) where 

import Options.Applicative
-- import Options.Applicative (Parser, (<**>), command, execParser, info, fullDesc, progDesc, helper)
import qualified Data.Text as T
import Warp.Commands
import System.Exit (die)
-- import Options.Applicative (subparser, progDesc, argument, metavar, execParser)
-- import qualified Data.Aeson.Types (Parser)

data Command 
    = Add T.Text FilePath 
    | Remove FilePath
    | List 
    | Jump T.Text


cmdParser :: Parser Command
cmdParser = subparser 
    ( command "add" (info addParser (progDesc "Add a Warp Point"))
   <> command "remove" (info removeParser (progDesc "Remove a Warp Point"))
   <> command "list" (info (pure List) (progDesc "List all Warp Points"))
   )
   <|> jumpParser -- default to Jump <name>

------------------------------
-- Individual Command Parsers
------------------------------

addParser :: Parser Command
addParser = Add 
    <$> argument str (metavar "NAME")
    <*> argument str (metavar "PATH")

removeParser :: Parser Command
removeParser = Remove
    <$> argument str (metavar "NAME")

jumpParser :: Parser Command 
jumpParser = Jump <$> argument str (metavar "NAME")


------------------------------
-- Main Entry and Execution
------------------------------

runCommand :: Command -> IO ()
runCommand cmd = case cmd of 
    Add name path       -> cmdAdd name path
    Remove name         -> cmdRemove (T.pack name)
    List                -> cmdList  
    Jump name           -> cmdResolve name >>= \case 
                                Just path -> putStrLn path
                                Nothing   -> die $ "Warp Point not found" ++ T.unpack name
main :: IO ()
main = execParser opts >>= runCommand
    where 
        opts = info (cmdParser <**> helper)
               (fullDesc <> progDesc "Warp CLI -- Quickly Jump to Directories") 
