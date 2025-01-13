module Service.Register.Finish where

import Data.ByteString.Lazy

finishRegister :: ByteString -> IO (Maybe ByteString)
finishRegister _ = pure $ Just "Finish Register!"
