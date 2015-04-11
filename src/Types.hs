module Types where

import Prelude hiding (FilePath)
import System.IO (FilePath)
import System.Process

-- | Type for a loaded, but not running service
data Service = Service {
             name :: String,  -- ^ Name of service
             exec :: FilePath -- ^ File to execute
}

-- | Type for a running service
data RService = RService {
              name :: String,        -- ^ Name of service
              pid  :: ProcessHandle  -- ^ ProcessHandle for service
}

data Configuration = Configuration {
                   serviceDir  :: FilePath,
                   execDir     :: FilePath,
                   serviceList :: FilePath,
                   logFile     :: FilePath,
                   pidDir      :: FilePath
}
