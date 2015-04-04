{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import System.Posix.Daemon
import Shelly
import qualified Data.Text as T
import Data.Default
import qualified Data.ConfigFile as C
import Data.Either.Utils
import Control.Monad.Error

data Service = Service { name :: T.Text
                       , start :: FilePath
                       , stop :: FilePath
                       } deriving Show

buildServices :: MonadError C.CPError m => C.ConfigParser -> [C.SectionSpec] -> [m Service]
buildServices cp = fmap (buildService cp)


buildService :: MonadError C.CPError m => C.ConfigParser -> C.SectionSpec -> m Service
buildService cp section = do
        name <- C.get cp section "name"
        start <- C.get cp section "start"
        stop <- C.get cp section "stop" 
        return $ Service {name=name, start=fromText start, stop=fromText stop}

main = shelly $ do
    val <- liftIO $ C.readfile C.emptyCP "test.cfg"
    let cp = forceEither val
    let secs = C.sections cp
    let servs = map (show . forceEither) (buildServices cp secs)

    echo $ T.pack $ show servs
