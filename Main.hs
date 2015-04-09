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
import Control.Monad (when, liftM)

data Options = Init { config :: S.FilePath
                    }
             | Start { config :: S.FilePath
                     , sname :: String
                     }
               deriving (Show, Data, Typeable, Eq)

init :: Options
init = Init {config = def &= help "uinitd configuration file"
            }
            &= details ["Starts all enabled services."]

start :: Options
start = Start { config = def &= help "uinitd configuration file"
              , sname = def &= help "name of service"
              }
              &= details ["Start a service."]

prgModes :: Mode (CmdArgs Options)
prgModes = cmdArgsMode $ modes [init, start]
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
        conf <- configurationLoader config
        initHandler conf
optionHandler Start{..} = do
        conf <- configurationLoader config
        -- startHandler conf sname
        putStrLn "placeholder"

initHandler :: Configuration -> IO ()
initHandler Configuration{..} = do
        (servs, errors) <- serviceLoader serviceList
        shelly $ appendfile logFile (T.pack errors)
        case null servs of
            True -> putStrLn $ "No services loaded.\n" ++ errors
            False -> runServices pidPath servs

configurationLoader :: S.FilePath -> IO Configuration
configurationLoader conf = do
        check <- shelly $ test_f "~/.config/uinitd.conf"
        let file = case (null conf) of
                       False -> conf
                       True -> case check of
                                   True -> "~/.config/uinitd.conf"
                                   False -> "/etc/uinitd.conf"
        cp <- readfile emptyCP file
        -- Use of forceEither here is fine since failure on init would exit
        -- anyways
        let configuration = loadConfig $ forceEither cp
        case configuration of
            (Left e) -> error $ errorToString e
            (Right c) -> return c

serviceLoader :: S.FilePath -> IO ([Service], String)
serviceLoader serviceList = do
        servList <- openServicesFile serviceList
        case servList of
            (Left e) -> return ([], errorToString e)
            (Right cp) -> return $ runWriter $ services cp
