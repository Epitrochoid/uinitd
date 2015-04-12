{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Core where

import Types
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ConfigFile as C
import Control.Monad.Except

runUinitd :: Config -> UinitdState -> Uinitd a -> IO (a, UinitdState)
runUinitd conf state (Uinitd a) = runStateT (runReaderT a conf) state

defConfig :: Config
defConfig = Config {
          serviceDir = "",
          execDir = "",
          serviceList = "",
          logFile = "",
          pidDir = ""
}

defUinitdSt :: UinitdState
defUinitdSt = UinitdState {
            available = [],
            running = []
}

addService :: Service -> Uinitd ()
addService service = do
        UinitdState{..} <- get
        put UinitdState{available = service:available, running = running}

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

loadService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
loadService cp sec = do
        name <- C.get cp sec "name"
        exec <- C.get cp sec "exec"
        return $ Service {sname = name, exec = exec}

loadServices ::MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
loadServices cp = fmap (loadService cp)

