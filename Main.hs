{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

import Prelude hiding (init)
import Core hiding (name, exec)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import qualified System.IO as S
import Data.Either.Utils
import Data.ConfigFile
import Control.Exception
import qualified Data.Text as T

data Options = Init
             | TestMode
               deriving (Show, Data, Typeable, Eq)

init :: Options
init = Init &= details ["Starts all enabled services."]

testMode :: Options
testMode = TestMode &= details ["test"]


prgModes :: Mode (CmdArgs Options)
prgModes = cmdArgsMode $ modes [init, testMode]
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
optionHandler TestMode = exec TestMode

exec :: Options -> IO()
exec Init = initHandler "test.cfg"
exec TestMode = putStrLn "test"

initHandler :: S.FilePath -> IO ()
initHandler confFile = do
        config <- openServicesFile confFile
        case config of
            (Left (ParseError s, _)) -> putStrLn $ "Parse error in " ++ confFile ++ ": " ++ s
            (Left (OtherProblem s, _)) -> putStrLn $ "Init error: " ++ s
            (Left e) -> putStrLn $ "Unexpected error in init:\n" ++ (show e)
            (Right cp) -> let (errors, servs) = services cp
                          in runServices servs

