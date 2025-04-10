module Reacthome.Auth.Domain.Client where

import Reacthome.Auth.Domain.Client.Id
import Reacthome.Auth.Domain.Client.Name
import Reacthome.Auth.Domain.Hash

data Client = Client
    { id :: ClientId
    , name :: ClientName
    , secret :: Hash
    }
    deriving stock (Show)
