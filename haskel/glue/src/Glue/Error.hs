module Glue.Error where

import Data.Typeable (Typeable)

data GlueError
    = forall e. (Show e, Eq e, Typeable e) => GlueError e

instance Show GlueError where
    show (GlueError err) = "GlueError (" <> show err <> ")"

instance Eq GlueError where
    GlueError a == GlueError b = show a == show b
