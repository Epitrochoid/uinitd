{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

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
} deriving Show

-- | Type for a running service
data RService = RService {
              rname :: String,        -- ^ Name of service
              pid   :: ProcessHandle  -- ^ ProcessHandle for service
}

instance Show RService where
        show RService{..} = rname

data Config = Config {
            serviceDir  :: FilePath,
            execDir     :: FilePath,
            serviceList :: FilePath,
            logFile     :: FilePath,
            pidDir      :: FilePath
} deriving Show

data UinitdState = UinitdState {
                 available :: [Service],
                 running   :: [RService]
} deriving Show

newtype Uinitd a = Uinitd (ReaderT Config (StateT UinitdState IO) a)
                   deriving (Monad, Applicative, Functor, MonadState UinitdState, MonadIO)
