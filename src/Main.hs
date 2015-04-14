{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}

import Types hiding (sname, exec)
import Core
import Sys

import Prelude hiding (init)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import qualified System.IO as S
import Control.Monad (when)
import Control.Monad.Except

data Options = Init { config :: S.FilePath
                    }
             | Start { config :: S.FilePath
                     , sname :: SName
                     }
             | Stop { config :: S.FilePath
                    , sname :: SName
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

stop :: Options
stop = Stop { config = def &= help "uinitd configuration file"
            , sname = def &= help "name of service"
            }
            &= details ["Stop a service."]

prgModes :: Mode (CmdArgs Options)
prgModes = cmdArgsMode $ modes [init, start, stop]
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
        conf <- loadConfUnsafe config
        initHandler conf

initHandler :: Config -> IO ()
initHandler conf = do
        (_, s1) <- runUinitd conf defUinitdSt (loadAllServices)
        (_, s2) <- runUinitd conf s1 (startAllServices)
        return ()




