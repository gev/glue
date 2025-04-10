module Reacthome.Auth.Domain.Clients where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Client
import Reacthome.Auth.Domain.Client.Id

newtype Clients = Clients
    { findById :: ClientId -> MaybeT IO Client
    }
