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

runService :: S.FilePath -> Service -> IO ()
runService path Service{..} = runDetached (Just $ nameToPID path name) DevNull program
    where
        program = shelly $ run_ exec []

runServices :: S.FilePath -> [Service] -> IO ()
runServices pidpath  = mapM_ (runService pidpath)

killService :: S.FilePath -> Service -> IO ()
killService pidpath Service{..} = kill $ nameToPID pidpath name

nameToPID :: S.FilePath -> T.Text -> S.FilePath
nameToPID path name = (++) path $ T.unpack $ name `T.append` ".pid"

openServicesFile :: MonadError C.CPError m => S.FilePath -> IO (m C.ConfigParser)
openServicesFile path = do
        conf <- try $ C.readfile C.emptyCP path
        case conf of
            (Left (SomeException e)) -> return $ throwError (C.OtherProblem $ errorstring e, "openServicesFile")
            (Right cp) -> return cp
    where
        errorstring e = "Could not open config file: " ++ path ++ "\n" ++ (show e)

services :: C.ConfigParser -> Writer String [Service]
services cp = do
        let secs = C.sections cp
            full = buildServices cp secs
            servs = rights full
            fails = lefts full
        mapM_ (tell . errorToString) fails
        return servs

containsService :: T.Text -> [Service] -> Bool
containsService serv servs = serv `elem` (fmap name servs)

getService :: T.Text -> [Service] -> Maybe Service
getService serv servs = case containsService serv servs of
                            False -> Nothing
                            True -> Just $ head $ filter (\a -> name a == serv) servs

errorToString :: C.CPError -> String
errorToString (C.ParseError s, loc) = "Parse error :\n" ++ s ++ "\n in location " ++ loc ++ "\n"
errorToString (C.SectionAlreadyExists s, loc) = "Repeated section `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.NoSection s, loc) = "No section `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.NoOption s, loc) = "No option `" ++ s ++ "` in: " ++ loc ++ "\n"
errorToString (C.OtherProblem s, loc) = "Unexpected error:\n" ++ s ++ "\nin: " ++ loc ++ "\n"
errorToString _ = "Uncaught fault\n"

data Configuration = Configuration { serviceDir :: S.FilePath
                                   , execDir :: S.FilePath
                                   , serviceList :: S.FilePath
                                   , logFile :: FilePath
                                   , pidPath :: S.FilePath
                                   } deriving Show

loadConfig :: MonadError C.CPError m => C.ConfigParser -> m Configuration
loadConfig cp = do
        serviceDir <- C.get cp "" "services"
        execDir <- C.get cp "" "executables"
        serviceList <- C.get cp "" "enabled"
        logFile <- C.get cp "" "logfile"
        pidPath <- C.get cp "" "pids"
        return $ Configuration { serviceDir=serviceDir
                               , execDir=execDir
                               , serviceList=serviceList
                               , logFile=fromText $ T.pack logFile
                               , pidPath=pidPath}

