{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Sys where

import Types
import System.Process
import System.IO
import System.Directory (doesFileExist, getDirectoryContents)
import qualified Data.ConfigFile as C
import Data.DList
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Journal
import Control.Monad.Reader

startService :: Service -> Uinitd RService
startService Service{..} = do
        pid <- liftIO $ runCommand exec
        UinitdState{..} <- get
        let newRServ = RService {rname = sname, pid = pid}
        put UinitdState {available = available, running = newRServ:running}
        return newRServ

startAllServices :: Uinitd ()
startAllServices = do
        UinitdState{..} <- get
        mapM_ startService available

stopService :: RService -> Uinitd ()
stopService service = do
        UinitdState{..} <- get
        liftIO $ terminateProcess $ pid service
        let removed = filter (\x -> x /= service) running
        put UinitdState {available = available, running = removed}

loadParser :: (MonadError C.CPError ma, MonadIO mb) => FilePath -> mb (ma C.ConfigParser)
loadParser filepath = do
        parser <- liftIO $ try $ C.readfile C.emptyCP filepath
        case parser of
            (Left (SomeException e)) -> return $ throwError (C.OtherProblem (show e), "loadParser")
            (Right cp) -> return cp

loadParserList :: (MonadError C.CPError ma, MonadIO mb) => FilePath -> mb ([ma C.ConfigParser])
loadParserList directory = do
        filepaths <- liftIO $ try $ getDirectoryContents directory
        case filepaths of
            (Left (SomeException e)) -> return [throwError (C.OtherProblem (show e), "loadParserList")]
            (Right fps) -> mapM loadParser (fmap (directory ++) fps)

confOrDefault :: MonadIO m => FilePath -> m (Maybe FilePath)
confOrDefault given = do
        let a = not $ null given
        b <- liftIO $ doesFileExist userLoc
        c <- liftIO $ doesFileExist sysLoc
        return $ case (a, b, c) of
                      (False, False, True) -> Just sysLoc
                      (False, True, _) -> Just userLoc
                      (True, _, _) -> Just given
                      _ -> Nothing
    where
        userLoc = "~/.config/uinitd.conf" :: FilePath
        sysLoc = "/etc/uinitd.conf" :: FilePath

writeOutLog :: Uinitd ()
writeOutLog = do
        Config{..} <- ask
        UinitdState{..} <- get
        log <- history
        liftIO $ appendFile logFile (toList log)
        clear

writeCPtoFile :: MonadIO m => FilePath -> C.ConfigParser -> m (Either C.CPError ())
writeCPtoFile filepath cp = do
        let out = C.to_string cp
        result <- runExceptT $ do
            check <- liftIO $ doesFileExist filepath
            when check $ throwError (C.OtherProblem "File already exists.", "")
            write <- liftIO $ try $ writeFile filepath out
            case write of
                (Left (SomeException e)) -> throwError (C.OtherProblem (show e), "writeCPtoFile")
                _ -> return ()
        return result



