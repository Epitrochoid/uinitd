{-# LANGUAGE RecordWildCards #-}

module Sys where

import Types
import System.Process

runService :: Service -> IO RService
runService Service{..} = do
        handle <- runCommand exec
        return RService {rname = sname, pid = handle}
