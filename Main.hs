{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

import Prelude hiding (init)
import Core hiding (name, exec)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)

data Options = Init
               deriving (Show, Data, Typeable, Eq)

init :: Options
init = Init &= details ["Starts all enabled services."]


prgModes :: Mode (CmdArgs Options)
prgModes = cmdArgsMode $ modes [init]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "uinitd"
_PROGRAM_VERSION = "0.1.0.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A WM agnostic userspace init system"

main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun prgModes
    optionHandler opts

optionHandler :: Options -> IO ()
optionHandler Init = exec Init

exec :: Options -> IO()
exec Init = putStrLn "works"

