module Type where

import qualified Data.ByteString.Lazy as BL

type SendData = BL.ByteString -> IO ()
