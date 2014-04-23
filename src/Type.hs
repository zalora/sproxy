module Type where

import           Data.ByteString (ByteString)

type SendData = ByteString -> IO ()
