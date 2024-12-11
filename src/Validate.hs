module Validate (
    mkValid,
) where

import Prelude (Bool, Maybe (Just, Nothing), otherwise, ($))

mkValid :: (s -> Bool) -> (s -> d) -> s -> Maybe d
mkValid isValid mk src
    | isValid src = Just $ mk src
    | otherwise = Nothing
