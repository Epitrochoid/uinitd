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

loadParser :: FilePath -> Uinitd C.ConfigParser
loadParser filepath = do
        parser <- liftIO $ try $ C.readfile C.emptyCP filepath
        case parser of
            (Left (SomeException e)) -> throwError (C.OtherProblem (show e), "loadParser")
            (Right cp) -> cp

confOrDefault :: FilePath -> Uinitd FilePath
confOrDefault given = do
        let a = not $ null given
        b <- liftIO $ doesFileExist userLoc
        c <- liftIO $ doesFileExist sysLoc
        case (a, b, c) of
             (False, False, True) -> return sysLoc
             (False, True, _) -> return userLoc
             (True, _, _) -> return given
             _ -> throwError (C.OtherProblem "Program configuration file not found.", "confOrDefault")
    where
        userLoc = "~/.config/uinitd.conf"
        sysLoc = "/etc/uinitd.conf"
