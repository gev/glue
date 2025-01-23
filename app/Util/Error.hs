module Util.Error where

hush :: Either l r -> Maybe r
hush = \case
    Right b -> Just b
    _ -> Nothing
