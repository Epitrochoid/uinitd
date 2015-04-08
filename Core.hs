{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module Core where

import Prelude hiding (FilePath)
import Shelly
import qualified Data.Text as T
import qualified Data.ConfigFile as C
import Data.Either
import Control.Monad.Error
import Control.Monad.Writer
import Control.Exception
import System.Posix.Daemon
import qualified System.IO as S

data Service = Service { name :: T.Text
                       , exec :: FilePath
                       } deriving Show

buildServices :: MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
buildServices cp = fmap (buildService cp)

buildService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
buildService cp section = do
        name <- C.get cp section "name"
        exec <- C.get cp section "exec"
        return $ Service {name=name, exec=fromText exec}

runService :: Service -> IO ()
runService Service{..} = runDetached (Just $ nameToPID name) DevNull program
    where
        program = shelly $ run_ exec []

runServices :: [Service] -> IO ()
runServices = mapM_ runService

killService :: Service -> IO ()
killService Service{..} = kill $ nameToPID name

nameToPID :: T.Text -> S.FilePath
nameToPID name = T.unpack $ name `T.append` ".pid"

openServicesFile :: MonadError C.CPError m => S.FilePath -> IO (m C.ConfigParser)
openServicesFile path = do
        conf <- try $ C.readfile C.emptyCP path
        case conf of
            (Left (SomeException _)) -> return $ throwError (C.OtherProblem errorstring, "")
            (Right cp) -> return cp
    where
        errorstring = "Could not open config file: " ++ path

services :: C.ConfigParser -> Writer String [Service]
services cp = do
        let secs = C.sections cp
            full = buildServices cp secs
            servs = rights full
            fails = lefts full
        mapM_ (tell . errorToString) fails
        return servs

errorToString :: C.CPError -> String
errorToString (C.ParseError s, loc) = "Parse error :\n" ++ s ++ "\n in location " ++ loc ++ "\n"
errorToString (C.SectionAlreadyExists s, loc) = "Repeated section `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.NoSection s, loc) = "No section `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.NoOption s, loc) = "No option `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.OtherProblem s, loc) = "Unexpected error:\n" ++ s ++ "in: " ++ loc ++ "\n"
errorToString _ = "Uncaught fault\n"

data Configuration = Configuration { serviceDir :: S.FilePath
                                   , execDir :: S.FilePath
                                   , serviceList :: S.FilePath
                                   , logFile :: FilePath
                                   } deriving Show

loadConfig :: MonadError C.CPError m => C.ConfigParser -> m Configuration
loadConfig cp = do
        serviceDir <- C.get cp "" "services"
        execDir <- C.get cp "" "executables"
        serviceList <- C.get cp "" "enabled"
        logFile <- C.get cp "" "logfile"
        return $ Configuration { serviceDir=serviceDir
                               , execDir=execDir
                               , serviceList=serviceList
                               , logFile=fromText logFile}


