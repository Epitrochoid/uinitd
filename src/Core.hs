{-# LANGUAGE RecordWildCards #-}

module Core where

import Types
import Control.Monad.State
import Control.Monad.Reader

addService :: Service -> Uinitd ()
addService service = do
        UinitdState{..} <- get
        put UinitdState{available = service:available, running = running}
