{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}

import Types hiding (sname, exec)
import Core
import Sys

import Prelude hiding (init)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Daemon
import qualified System.IO as S
import Control.Monad (when)
import Control.Monad.Except
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

data Options = Init { config :: S.FilePath
                    }
             | Start { config :: S.FilePath
                     , sname :: SName
                     }
             | Stop { config :: S.FilePath
                    , sname :: SName
                    }
             | Restart { config :: S.FilePath
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

restart :: Options
restart = Restart { config = def &= help "uinitd configuration file"
                  , sname = def &= help "name of service"
                  }
                  &= details ["Restart a service."]


prgModes :: Mode (CmdArgs Options)
prgModes = cmdArgsMode $ modes [init, start, stop, restart]
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
optionHandler Start{..} = do
        conf <- loadConfUnsafe config
        startHandler conf sname
optionHandler Stop{..} = do
        conf <- loadConfUnsafe config
        stopHandler conf sname
optionHandler Restart{..} = do
        conf <- loadConfUnsafe config
        restartHandler conf sname

initHandler :: Config -> IO ()
initHandler conf = do
        let opts = DaemonOptions {daemonPort = port conf,
                                  daemonPidFile = PidFile $ (pidDir conf) ++ "uinitd.pid",
                                  printOnDaemonStarted = False}
        (_, s1) <- runUinitd conf defUinitdSt (loadAllServices)
        (_, s2) <- runUinitd conf s1 (startAllServices)
        stateMVar <- newMVar s2
        ensureDaemonRunning "uinitd" opts (daemon conf stateMVar)

startHandler :: Config -> SName -> IO ()
startHandler conf service = runClientCmd conf (CmdStart service)

stopHandler :: Config -> SName -> IO ()
stopHandler conf service = runClientCmd conf (CmdStop service)

restartHandler :: Config -> SName -> IO ()
restartHandler conf service = runClientCmd conf (CmdRestart service)

runClientCmd :: Config -> Cmd -> IO ()
runClientCmd Config{..} cmd = do
        resp <- runClient "localhost" port cmd
        case resp of
            Nothing -> putStrLn "No response from server."
            (Just r) -> case r of
                            Success -> return ()
                            (Failure f) -> putStrLn f

daemon :: Config -> MVar UinitdState -> Cmd -> IO Response
daemon conf stateMVar cmd = do
        st <- takeMVar stateMVar
        let toDo = case cmd of
                       (CmdStart s) -> startServiceByName s
                       (CmdStop r) -> stopServiceByName r
                       (CmdRestart r) -> restartServiceByName r
        (response, state) <- runUinitd conf st toDo
        putMVar stateMVar state
        return response


