{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Sys where

import Types
import System.Process
import qualified Data.ConfigFile as C
import Control.Exception
import Control.Monad.Except
import Control.Monad.State

startService :: Service -> Uinitd ()
startService Service{..} = do
        pid <- liftIO $ runCommand exec
        UinitdState{..} <- get
        let newRServ = RService {rname = sname, pid = pid}
        put UinitdState {available = available, running = newRServ:running}


loadConfig :: MonadError C.CPError m => C.ConfigParser -> m Config
loadConfig cp = do
        serviceDir <- C.get cp "" "services"
        execDir <- C.get cp "" "executables"
        serviceList <- C.get cp "" "enabled"
        logFile <- C.get cp "" "logfile"
        pidDir <- C.get cp "" "pid_directory"
        return $ Config {serviceDir = serviceDir,
                         execDir = execDir,
                         serviceList = serviceList,
                         logFile = logFile,
                         pidDir = pidDir}

loadParser :: MonadError C.CPError m => FilePath -> IO (m C.ConfigParser)
loadParser filepath = do
        parser <- try $ C.readfile C.emptyCP filepath
        case parser of
            (Left (SomeException e)) -> return $ throwError (C.OtherProblem (show e), "loadParser")
            (Right cp) -> return cp
