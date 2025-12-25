module Reactor.Error where

import Data.Typeable (Typeable, cast)

data ReactorError
    = forall e. (Show e, Eq e, Typeable e) => ReactorError e

instance Show ReactorError where
    show (ReactorError err) = "ReactorError (" <> show err <> ")"

instance Eq ReactorError where
    ReactorError a == ReactorError b = case cast a of
        Just a' -> a' == b
        Nothing -> False
