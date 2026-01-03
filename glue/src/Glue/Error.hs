module Glue.Error where

import Data.Typeable (Typeable)

data ReactorError
    = forall e. (Show e, Eq e, Typeable e) => ReactorError e

instance Show ReactorError where
    show (ReactorError err) = "ReactorError (" <> show err <> ")"

instance Eq ReactorError where
    ReactorError a == ReactorError b = show a == show b
