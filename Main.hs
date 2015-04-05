{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

import Prelude hiding (FilePath)
import Shelly
import qualified Data.Text as T
import Data.Default
import qualified Data.ConfigFile as C
import Data.Either.Utils
import Control.Monad.Error
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

nameToPID :: T.Text -> S.FilePath
nameToPID name = T.unpack $ name `T.append` ".pid"


main = shelly $ do
    val <- liftIO $ C.readfile C.emptyCP "test.cfg"
    let cp = forceEither val
    let secs = C.sections cp
    let servs = map (show . forceEither) (buildServices cp secs)

    echo $ T.pack $ show servs
