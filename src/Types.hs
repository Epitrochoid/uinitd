{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveGeneric #-}

module Types where

import Prelude hiding (FilePath)
import GHC.Generics
import System.IO (FilePath)
import System.Process
import System.Daemon
import Control.Monad.State
import Control.Monad.Reader
import Data.Serialize (Serialize)

-- | Name of a service
type SName = String

-- | Type for a loaded, but not running service
data Service = Service {
             sname :: SName,  -- ^ Name of service
             exec  :: FilePath -- ^ File to execute
} deriving Show

-- | Type for a running service
data RService = RService {
              rname :: SName,        -- ^ Name of service
              pid   :: ProcessHandle  -- ^ ProcessHandle for service
}

instance Eq RService where
        a == b = (rname a) == (rname b)
        a /= b = not (a == b)

instance Show RService where
        show RService{..} = rname

-- | Global program configuration
data Config = Config {
            serviceDir  :: FilePath,
            execDir     :: FilePath,
            serviceList :: FilePath,
            logFile     :: FilePath,
            pidDir      :: FilePath,
            port        :: Port
} deriving Show

-- | Global program state
data UinitdState = UinitdState {
                 available :: [Service],
                 running   :: [RService]
} deriving Show

-- | Monad stack that carries configuration and state
newtype Uinitd a = Uinitd (ReaderT Config (StateT UinitdState IO) a)
                   deriving (Monad, Applicative, Functor, MonadState UinitdState, MonadIO)

-- | Daemon commands
data Cmd = Start SName
         | Stop SName
         | Restart SName
         deriving (Generic, Show)

instance Serialize Cmd

-- | Daemon responses
data Response = Failure String
              | Success
              deriving (Generic, Show)

instance Serialize Response
