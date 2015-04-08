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
import Control.Monad.Writer (runWriter)
import Shelly hiding (readfile)
import Control.Monad (when)

data Options = Init { config :: S.FilePath
                    }
             | TestMode
               deriving (Show, Data, Typeable, Eq)

init :: Options
init = Init {config = def &= help "uinitd configuration file"
            }
            &= details ["Starts all enabled services."]

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
optionHandler Init{..} = do
        let opts = if (null config) then Init {config="uinitd.cfg"} else Init {config=config}
        exec opts
optionHandler TestMode = exec TestMode

exec :: Options -> IO ()
exec Init{..} = do
        cp <- readfile emptyCP config
        let conf = loadConfig $ forceEither cp
        case conf of
            (Left e) -> error $ errorToString e
            (Right c) -> initHandler c
exec TestMode = putStrLn "test"

initHandler :: Configuration -> IO ()
initHandler Configuration{..} = do
        servList <- openServicesFile serviceList
        case servList of
            (Left e) -> putStrLn $ errorToString e
            (Right cp) -> let (servs, errors) = runWriter $ services cp
                          in runServices servs >> (shelly $ appendfile logFile (T.pack errors))


