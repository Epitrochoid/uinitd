{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Sys where

import Types
import System.Process
import System.IO
import System.Directory (doesFileExist)
import qualified Data.ConfigFile as C
import Control.Exception
import Control.Monad.Except
import Control.Monad.State

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

loadParser :: MonadError C.CPError m => FilePath -> IO (m C.ConfigParser)
loadParser filepath = do
        parser <- try $ C.readfile C.emptyCP filepath
        case parser of
            (Left (SomeException e)) -> return $ throwError (C.OtherProblem (show e), "loadParser")
            (Right cp) -> return cp

confOrDefault :: FilePath -> IO (Maybe FilePath)
confOrDefault given = do
        let a = not $ null given
        b <- doesFileExist userLoc
        c <- doesFileExist sysLoc
        return $ case (a, b, c) of
                      (False, False, True) -> Just sysLoc
                      (False, True, _) -> Just userLoc
                      (True, _, _) -> Just userLoc
                      _ -> Nothing
    where
        userLoc = "~/.config/uinitd.conf" :: FilePath
        sysLoc = "/etc/uinitd.conf" :: FilePath
