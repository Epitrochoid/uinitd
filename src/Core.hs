{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Core where

import Types
import Sys
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ConfigFile as C
import Control.Monad.Except
import Control.Monad.Trans.Journal
import System.IO
import Data.Either
import Data.DList

runUinitd :: Config -> UinitdState -> Uinitd a -> IO (a, UinitdState)
runUinitd conf state (Uinitd a) = evalJournalT (runStateT (runReaderT a conf) state)

loadConfUnsafe :: MonadIO m => FilePath -> m Config
loadConfUnsafe filePath = do
        conf <- confOrDefault filePath
        cp <- case conf of
                  Nothing -> error "Program configuration file not found."
                  (Just c) -> loadParser c
        case cp of
            (Left e) -> error $ "Fatal error:\n" ++ (show e)
            (Right c) -> let config = loadConfig c
                         in case config of
                                (Left e) -> error $ "Fatal error:\n" ++ (show e)
                                (Right c) -> return c

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

loadAllServices :: Uinitd ()
loadAllServices = do
        Config{..} <- ask
        UinitdState{..} <- get
        maybeCp <- loadParser serviceList
        case maybeCp of
            (Left e) -> journal $ fromList (show e)
            (Right cp) -> let full = loadServices cp (C.sections cp)
                              servs = rights full
                              errors = lefts full
                          in (mapM (journal . fromList . show) errors) >> (put UinitdState{available=servs, running=running})

-- | Finds a service by name, does not check that
--   exec is the same. Presumes uniqueness of services
findServiceByName :: SName -> [Service] -> Maybe Service
findServiceByName sname services = case filter (named sname) services of
                                 [] -> Nothing
                                 s -> Just $ Prelude.head s
    where
        named serv Service{..} = serv == sname

-- | Finds a service by identity, sname and
--   exec must be the same. Presumes uniqueness of services.
findService :: Service -> [Service] -> Maybe Service
findService serv servs = case filter (== serv) servs of
                             [] -> Nothing
                             s -> Just $ Prelude.head s
