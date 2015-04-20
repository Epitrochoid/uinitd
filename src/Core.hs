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

-- | Run method for program monad stack
runUinitd :: Config -> UinitdState -> Uinitd a -> IO (a, UinitdState)
runUinitd conf state (Uinitd a) = evalJournalT (runStateT (runReaderT a conf) state)

-- | Unsafe method for loading program configuration,
--   terminates on failure and prints error.
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

-- | Adds a service to the list of available services
--   inside Uinitd
addService :: Service -> Uinitd ()
addService service = do
        UinitdState{..} <- get
        put UinitdState{available = service:available, running = running, enabled = enabled}

-- | Builds a Config object from a ConfigParser
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

-- | Creates a single Service from the given section
loadService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
loadService cp sec = do
        name <- C.get cp sec "name"
        exec <- C.get cp sec "exec"
        return $ Service {sname = name, exec = exec}

-- | Minimal wrapper around loadService to load multiple
loadServices :: MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
loadServices cp = fmap (loadService cp)

-- | Loads all available services inside a Uinitd
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
                          in (mapM (journal . fromList . show) errors) >> (put UinitdState{available=servs, running=running, enabled = servs})
        writeOutLog

loadServicesFromDir :: Uinitd ()
loadServicesFromDir = do
        UinitdState{..} <- get
        Config{..} <- ask
        parsers <- loadParserList serviceDir
        let errors = lefts parsers
            cps = rights parsers
        mapM (journal . fromList . (++ "\n") . show) errors
        let services = fmap (flip loadService $ "") cps
        let errors2 = lefts services
            servs = rights services
        mapM (journal . fromList . show) errors2
        put UinitdState{available = servs ++ available, running = running, enabled = enabled}
        writeOutLog


startServiceByName :: SName -> Uinitd Response
startServiceByName service = do
        UinitdState{..} <- get
        let serv = findServiceByName service available
        case serv of
            Nothing -> return $ Failure $ "No service `" ++ service ++ "` found. \n"
            (Just s) -> (startService s) >> (return Success)

stopServiceByName :: SName -> Uinitd Response
stopServiceByName service = do
        UinitdState{..} <- get
        let serv = findRServiceByName service running
        case serv of
            Nothing -> return $ Failure $ "No service `" ++ service ++ "` running.\n"
            (Just s) -> (stopService s) >> (return Success)

restartServiceByName :: SName -> Uinitd Response
restartServiceByName service = do
        stop <- stopServiceByName service
        case stop of
            (Failure f) -> return $ Failure f
            Success -> startServiceByName service

listServices :: Uinitd Response
listServices = do
        UinitdState{..} <- get
        return $ ServList available

-- | Finds a service by name, does not check that
--   exec is the same. Presumes uniqueness of services
findServiceByName :: SName -> [Service] -> Maybe Service
findServiceByName sname services = case filter (named sname) services of
                                        [] -> Nothing
                                        s -> Just $ Prelude.head s
    where
        named serv Service{..} = serv == sname

-- | Finds a running service by name, does not check
--   pid.
findRServiceByName :: SName -> [RService] -> Maybe RService
findRServiceByName rname services = case filter (named rname) services of
                                        [] -> Nothing
                                        r -> Just $ Prelude.head r
    where
        named serv RService{..} = serv == rname

-- | Finds a service by identity, sname and
--   exec must be the same. Presumes uniqueness of services.
findService :: Service -> [Service] -> Maybe Service
findService serv servs = case filter (== serv) servs of
                             [] -> Nothing
                             s -> Just $ Prelude.head s

serviceToCP :: (MonadError C.CPError m) => Bool -> Service -> m C.ConfigParser
serviceToCP sec Service{..} = do
        let cp = C.emptyCP
        let section = case sec of
                          True -> sname
                          False -> ""
        cp <- C.add_section cp section
        cp <- C.set cp section "name" sname
        cp <- C.set cp section "exec" exec
        return cp

serviceListToCP :: (MonadError C.CPError m) => [Service] -> m C.ConfigParser
serviceListToCP services = do
        cps <- mapM (serviceToCP True) services
        return $ foldl C.merge C.emptyCP cps

createService :: Service -> Uinitd Response
createService service = do
        Config{..} <- ask
        let filepath = serviceDir ++ (sname service) ++ ".service"
        result <- runExceptT $ do
            cp <- serviceToCP False service
            writeCPtoFile filepath cp
        case result of
            (Left e) -> return $ Failure (show e)
            _ -> return Success

enableService :: Service -> Uinitd Response
enableService serv@Service{..} = do
        Config{..} <- ask
        UinitdState{..} <- get
        result <- runExceptT $ do
            cpServ <- C.add_section C.emptyCP sname
            cpServ <- C.set cpServ sname "name" sname
            cpServ <- C.set cpServ sname "exec" exec
            cpList <- serviceListToCP enabled
            let final = C.merge cpServ cpList
            writeCPtoFile serviceList final
        case result of
            (Left e) -> return $ Failure (show e)
            _ -> (put UinitdState{running=running, available=available, enabled=serv:enabled}) >> (return Success)

enableServiceByName :: SName -> Uinitd Response
enableServiceByName service = do
        UinitdState{..} <- get
        let serv = findServiceByName service available
        case serv of
            Nothing -> return $ Failure $ "No such service named `" ++ service ++ "`."
            (Just s) -> enableService s


