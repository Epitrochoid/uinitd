{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Prelude hiding (FilePath)
import System.Posix.Daemonize hiding (name)
import Shelly
import qualified Data.Text as T
import Data.Default
import qualified Data.ConfigFile as C
import Data.Either.Utils
import Control.Monad.Error

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

main = shelly $ do
    val <- liftIO $ C.readfile C.emptyCP "test.cfg"
    let cp = forceEither val
    let secs = C.sections cp
    let servs = map (show . forceEither) (buildServices cp secs)

    echo $ T.pack $ show servs
