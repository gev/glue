module Lang where

import Data.Text

data Lemma = Lemma
    { id :: Int
    , lemma :: Form
    , forms :: [Form]
    }
    deriving stock (Show)

data Form = Form
    { text :: Text
    , grammemes :: [Grammeme]
    }
    deriving stock (Show)

data Link = Link
    { id :: Int
    , from :: Int
    , to :: Int
    , type_ :: Int
    }
    deriving stock (Show)

type Grammeme = Text
