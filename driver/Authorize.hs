module Authorize (authorize) where

import           Network.HTTP.SProxy

authorize :: AuthorizeAction
authorize email _domain _path _method
  | isAllowed = return ["all"]
  | otherwise = return []
  where
    isAllowed = emailDomain `elem` allowedDomains
    emailDomain = drop 1 . dropWhile (/= '@') $ email

allowedDomains :: [String]
allowedDomains = [
    "zalora.com"
  , "zalora.sg"
  , "zalora.com.my"
  , "zalora.co.th"
  , "zalora.vn"
  , "zalora.co.id"
  , "zalora.com.ph"
  , "zalora.com.hk"
  ]
