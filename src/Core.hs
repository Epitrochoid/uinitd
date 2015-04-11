{-# LANGUAGE RecordWildCards #-}

module Core where

import Types
import Control.Monad.State
import Control.Monad.Reader

runUinitd :: Config -> UinitdState -> Uinitd a -> IO (a, UinitdState)
runUinitd conf state (Uinitd a) = runStateT (runReaderT a conf) state

defConfig :: Config
defConfig = Config {
          serviceDir = "",
          execDir = "",
          serviceList = "",
          logFile = "",
          pidDir = ""
}

defUinitdSt :: UinitdState
defUinitdSt = UinitdState {
            available = [],
            running = []
}

addService :: Service -> Uinitd ()
addService service = do
        UinitdState{..} <- get
        put UinitdState{available = service:available, running = running}
