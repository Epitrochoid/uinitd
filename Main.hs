{-# LANGUAGE OverloadedStrings #-}

import System.Posix.Daemon
import Shelly
import qualified Data.Text as T
import Data.Default


main = runDetached (Just "test.pid") def $ shelly $ do
    sleep 10
