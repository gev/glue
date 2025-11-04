module Reacthome.Core.Adjustable where

newtype AdjustableValue t = AdjustableValue t

data AdjustableRange t = AdjustableRange
    { min :: t
    , max :: t
    }
