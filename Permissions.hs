{-# LANGUAGE TupleSections #-}
module Permissions where


import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Map
import Data.Yaml
import Safe
import Text.Printf

import Prelude hiding (lookup)


data Permissions =
    Permissions
        (Map String ([String], [String])) -- maps from user emails to groups and allowed domains
  deriving (Show)

instance FromJSON Permissions where
    parseJSON (Object o) = join $ create <$>
        (parseJSON =<< (o .: "users")) <*>
        (parseJSON =<< (o .: "groups")) <*>
        (parseJSON =<< (o .: "url-patterns"))
      where
        create :: [User] -> [Group] -> [UrlPattern] -> Parser Permissions
        create users groups urlPatterns = do
            let groupUrlPatternMap = fromList $ fmap (\ (Group n ss) -> (n, ss)) groups
                urlPatternDomainMap = fromList $ fmap (\ (UrlPattern n ds) -> (n, ds)) urlPatterns
            userGroupsDomainMap <- fromList <$>
                forM users (\ (User e gs) ->
                    (\ domains -> (e, (gs, domains))) <$> concat <$>
                    forM gs (\ groupName ->
                        case lookup groupName groupUrlPatternMap of
                            Nothing -> fail ("missing group in config: " ++ groupName)
                            Just urlPatterns ->
                                forM urlPatterns (\ urlPatternName ->
                                    case lookup urlPatternName urlPatternDomainMap of
                                        Nothing -> fail ("missing url-pattern in config: " ++ urlPatternName)
                                        Just domains -> return domains)))
            return $ Permissions userGroupsDomainMap

    parseJSON _ = mzero

-- | User identified by email with a list of groups she is in.
data User = User {
    email :: String,
    userGroups :: [String]
  }
    deriving (Show)

instance FromJSON User where
    parseJSON (Object o) = User <$>
        o .: "email" <*>
        o .: "groups"
    parseJSON _ = mzero

data Group = Group {
    groupName :: String,
    groupUrlPatterns :: [String]
  }
    deriving (Show)

instance FromJSON Group where
    parseJSON (Object o) = Group <$>
        o .: "name" <*>
        o .: "url-patterns"
    parseJSON _ = mzero

data UrlPattern = UrlPattern {
    patternName :: String,
    domain :: String
  }
    deriving (Show)

instance FromJSON UrlPattern where
    parseJSON (Object o) = UrlPattern <$>
        o .: "name" <*>
        o .: "domain"
    parseJSON _ = mzero


-- | Checks if a user (given by email) is authorized for a given host.
-- (Right ()) means authorized.
isAuthorized :: Permissions -> String -> Maybe String -> String -> Either String [String]
isAuthorized (Permissions permissions) email (Just host) url =
    if headMay url /= Just '/' then
        -- we discard any url that is not a normal absolute path for security reasons
        Left ("only absolute paths allowed: " ++ url)
    else case lookup email permissions of
        Nothing -> fail ("email not found: " ++ email)
        Just (groups, domains) -> if not (host `elem` domains) then
            Left (printf "user %s is not authorized to access %s" email host)
          else
            -- authorization successful
            Right groups
isAuthorized _ _ Nothing _ =
    Left "no Host header set"
