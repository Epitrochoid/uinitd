{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Core where

import Types
import Sys
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ConfigFile as C
import Control.Monad.Except
import System.IO

runUinitd :: Config -> UinitdState -> Uinitd a -> IO (Either C.CPError (a, UinitdState))
runUinitd conf state (Uinitd a) = runExceptT (runStateT (runReaderT a conf) state)

initUinitd :: FilePath -> Uinitd ()
initUinitd confFile = do
        conf <- confOrDefault confFile
        cp <- loadParser conf
        return ()


defConfig :: Config
defConfig = Config {
          serviceDir = "",
          execDir = "",
          serviceList = "",
          logFile = "",
          pidDir = "",
          port = 5000
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
        port <- C.get cp "" "port"
        return $ Config {serviceDir = serviceDir,
                         execDir = execDir,
                         serviceList = serviceList,
                         logFile = logFile,
                         pidDir = pidDir,
                         port = port}

loadService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
loadService cp sec = do
        name <- C.get cp sec "name"
        exec <- C.get cp sec "exec"
        return $ Service {sname = name, exec = exec}

loadServices :: MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
loadServices cp = fmap (loadService cp)


-- | Finds a service by name, does not check that
--   exec is the same. Presumes uniqueness of services
findServiceByName :: SName -> [Service] -> Maybe Service
findServiceByName sname services = case filter (named sname) services of
                                 [] -> Nothing
                                 s -> Just $ head s
    where
        named serv Service{..} = serv == sname

-- | Finds a service by identity, sname and
--   exec must be the same. Presumes uniqueness of services.
findService :: Service -> [Service] -> Maybe Service
findService serv servs = case filter (== serv) servs of
                             [] -> Nothing
                             s -> Just $ head s
