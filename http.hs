{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP
import Network.Browser
import Network.URI
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)

test_url :: String
test_url = ""

data Release = Release
    { getId :: String
    , getTitle :: String
    , getStatus :: String
    , getDescription :: String
    , getPhases :: PhaseList
    }
    deriving (Generic, Show)

newtype PhaseList = PhaseList [Phase]
    deriving (Generic, Show)

data Phase = Phase
    { getPhaseId :: String
    , getPhaseTitle :: String
    , getPhaseStatus :: String
    , getTasks :: TaskList
    }
    deriving (Generic, Show)

newtype TaskList = TaskList [Task]
    deriving (Generic, Show)

data Task = Task
    { getTaskId :: String
    , getTaskTitle :: String
    , getTaskStatus :: String
    , getTaskType :: String
    , getPlannedDuration :: Maybe Int
    }
    deriving (Generic, Show)

{-instance ToJSON Release where-}
{-instance ToJSON Phase where-}

instance FromJSON Release where
    parseJSON (Object v) = Release <$>
                            v .: "id" <*>
                            v .: "title" <*>
                            v .: "status" <*>
                            v .: "description" <*>
                            v .: "phases"
    parseJSON _ = empty

instance FromJSON PhaseList where
instance FromJSON TaskList where

instance FromJSON Phase where
    parseJSON (Object v) = Phase <$>
                            v .: "id" <*>
                            v .: "title" <*>
                            v .: "status" <*>
                            v .: "tasks"
    parseJSON _ = empty

instance FromJSON Task where
    parseJSON (Object o) = Task <$>
                            o .: "id" <*>
                            o .: "title" <*>
                            o .: "status" <*>
                            o .: "type" <*>
                            o .:? "plannedDuration"
    parseJSON _ = empty

fromJust :: Maybe a -> a
fromJust (Just a) = a

my_query :: Authority -> String -> BrowserAction (HandleStream String) (URI, Response String)
my_query auth url = do
    setAllowRedirects True
    addAuthority auth
    request $ getRequest url

test :: String -> String -> String -> IO (Maybe Release)
test username password url = do
    let auth = AuthBasic "" username password (fromJust $ parseURI url)
    (_, rsp) <- browse $ my_query auth url
    let maybeRelease = decode (pack $ rspBody rsp) :: Maybe Release
    return maybeRelease

do_test :: IO (Maybe Release)
do_test = test "admin" "pass" test_url

