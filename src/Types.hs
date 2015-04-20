{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveGeneric #-}

module Types where

import Prelude hiding (FilePath)
import GHC.Generics
import System.IO (FilePath)
import System.Process
import System.Daemon
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Journal
import Control.Monad.Trans.Journal
import Data.Serialize (Serialize)
import Data.DList
import Data.Default
import qualified Data.ConfigFile as C

-- | Name of a service
type SName = String

-- | Efficient structure for appending logs
type Log = DList Char

-- | Type for a loaded, but not running service
data Service = Service {
             sname :: SName,  -- ^ Name of service
             exec  :: FilePath -- ^ File to execute
} deriving (Eq, Generic)

instance Show Service where
        show Service{..} = sname ++ ": " ++ exec

instance Serialize Service

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

instance Default Config where
        def = Config {serviceDir = "",
                      execDir = "",
                      serviceList = "",
                      logFile = "",
                      pidDir = "",
                      port = 5000}

-- | Global program state
data UinitdState = UinitdState {
                 available :: [Service],
                 running   :: [RService],
                 enabled   :: [Service]
} deriving Show

instance Default UinitdState where
        def = UinitdState {available = [], running = [], enabled = []}

-- | Monad stack that carries configuration and state
newtype Uinitd a = Uinitd (ReaderT Config (StateT UinitdState (JournalT Log IO)) a)
                   deriving (Monad, Applicative, Functor, MonadState UinitdState, MonadIO, MonadReader Config, MonadJournal Log)

-- | Daemon commands
data Cmd = CmdStart SName
         | CmdStop SName
         | CmdRestart SName
         | CmdList
         | CmdCreate SName FilePath
         | CmdEnable SName
         deriving (Generic, Show)

instance Serialize Cmd

-- | Daemon responses
data Response = Failure String
              | Success
              | ServList [Service]
              deriving (Generic, Show)

instance Serialize Response
