{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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

-- buildServices :: [C.SectionSpec] -> C.ConfigParser -> [Either Service]


buildService :: MonadError C.CPError m => C.SectionSpec -> C.ConfigParser -> m Service
buildService section cp = do
        name <- C.get cp section "name"
        start <- C.get cp section "start"
        stop <- C.get cp section "stop" 
        return $ Service {name=name, start=fromText start, stop=fromText stop}

main = shelly $ do
    val <- liftIO $ C.readfile C.emptyCP "test.cfg"
    let cp = forceEither val

    let a = buildService "section1" cp
    echo $ T.pack $ show $ forceEither a
