{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Prelude hiding (FilePath)
import System.IO (FilePath)
import System.Process
import Control.Monad.State
import Control.Monad.Reader

-- | Type for a loaded, but not running service
data Service = Service {
             sname :: String,  -- ^ Name of service
             exec  :: FilePath -- ^ File to execute
}

-- | Type for a running service
data RService = RService {
              rname :: String,        -- ^ Name of service
              pid   :: ProcessHandle  -- ^ ProcessHandle for service
}

data Config = Config {
              serviceDir  :: FilePath,
              execDir     :: FilePath,
              serviceList :: FilePath,
              logFile     :: FilePath,
              pidDir      :: FilePath
}

data UinitdState = UinitdState {
                 available :: [Service],
                 running   :: [RService]
}

newtype Uinitd a = Uinitd (ReaderT Config (StateT UinitdState IO) a)
                   deriving (Monad, Applicative, Functor, MonadState UinitdState)
