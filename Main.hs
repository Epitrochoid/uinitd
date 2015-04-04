{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

import Prelude hiding (FilePath)
import System.Posix.Daemonize
import Shelly
import qualified Data.Text as T
import Data.Default
import qualified Data.ConfigFile as C
import Data.Either.Utils
import Control.Monad.Error

data Service = Service { sname :: T.Text
                       , exec :: FilePath
                       } deriving Show

buildServices :: MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
buildServices cp = fmap (buildService cp)

buildService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
buildService cp section = do
        sname <- C.get cp section "name"
        exec <- C.get cp section "exec"
        return $ Service {sname=sname, exec=fromText exec}

serviceToDaemon :: Service -> CreateDaemon ()
serviceToDaemon Service{..} = CreateDaemon { privilegedAction = return ()
                                           , program = return $ shelly $ run_ exec []
                                           , name = Just $ T.unpack sname
                                           , user = Nothing
                                           , group = Nothing
                                           , syslogOptions = []
                                           , pidfileDirectory = Nothing}

main = shelly $ do
    val <- liftIO $ C.readfile C.emptyCP "test.cfg"
    let cp = forceEither val
    let secs = C.sections cp
    let servs = map (show . forceEither) (buildServices cp secs)

    echo $ T.pack $ show servs
