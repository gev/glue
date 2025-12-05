module Rest.ContentType where

import Data.ByteString

type ContentType = ByteString

ctTextPlane :: ContentType
ctTextPlane = "text/plain"

ctApplicationJson :: ContentType
ctApplicationJson = "application/json"

ctApplicationHtml :: ContentType
ctApplicationHtml = "text/html"
