module Web.Rest.ContentType where

import Data.ByteString

ctTextPlane :: ByteString
ctTextPlane = "text/plain"

ctApplicationJson :: ByteString
ctApplicationJson = "application/json"

ctApplicationHtml :: ByteString
ctApplicationHtml = "text/html"
