module Reacthome.Auth.Domain.Clients where

import Reacthome.Auth.Domain.Client
import Reacthome.Auth.Domain.Client.Id

newtype Clients = Clients
    { findById :: ClientId -> Either String Client
    }
