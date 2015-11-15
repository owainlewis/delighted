{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Delighted.API where

import           Control.Applicative   ((<$>), (<$>))
import           Control.Lens          ((&), (.~), (?~), (^.))
import           Data.Aeson            as Aeson
import           Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Lazy  as LBS
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics
import           Network.Wreq

--------------------------------------------------------------------------------------

requestOpts :: String -> Options
requestOpts apiKey = defaults & auth ?~ basicAuth (C8.pack apiKey) ""
                              & header "Accept" .~ ["application/json"]

buildResource :: String -> String
buildResource resource = "https://api.delighted.com" <> resource

mapToParams :: Options ->
               M.Map String String ->
               Options
mapToParams opts m =
    M.foldrWithKey (\k v ks -> opts & param (T.pack k) .~ [T.pack v]) opts m

getRequest :: String -> String -> M.Map String String -> IO LBS.ByteString
getRequest apiKey resource queryParams = do
    let opts = mapToParams (requestOpts apiKey) queryParams
    r <- getWith opts (buildResource resource)
    pure $ r ^. responseBody

postRequest :: String -> String -> M.Map String String -> IO LBS.ByteString
postRequest apiKey resource queryParams = do
    let opts = mapToParams (requestOpts apiKey) queryParams
    r <- postWith opts (buildResource resource) Aeson.Null
    pure $ r ^. responseBody

postWithPayload :: ToJSON a =>
                   String ->
                   String -> a ->
                   IO LBS.ByteString
postWithPayload apiKey resource payload = do
    let opts = requestOpts apiKey
    r <- postWith opts (buildResource resource) (Aeson.toJSON payload)
    pure $ r ^. responseBody

--------------------------------------------------------------------------------------

data MetricsResponse = MetricsResponse {
    nps              :: Int
  , promoterCount    :: Int
  , promoterPercent  :: Double
  , passiveCount     :: Double
  , passivePercent   :: Double
  , detractorCount   :: Int
  , detractorPercent :: Double
  , responseCount    :: Int
} deriving ( Eq, Ord, Show )

instance FromJSON MetricsResponse where
    parseJSON (Object o) = MetricsResponse <$>
                          (o .: "nps")               <*>
                          (o .: "promoter_count")    <*>
                          (o .: "promoter_percent")  <*>
                          (o .: "passive_count")     <*>
                          (o .: "passive_percent")   <*>
                          (o .: "detractor_count")   <*>
                          (o .: "detractor_percent") <*>
                          (o .: "response_count")

data UnsubscribesResponse = UnsubscribesResponse {
    unsubscribeId    :: String
  , unsubscribeEmail :: String
  , unsubscribeName  :: Maybe String
  , unsubscribed     :: Int
} deriving ( Eq, Ord, Show )

instance FromJSON UnsubscribesResponse where
    parseJSON (Object o) = UnsubscribesResponse <$>
                           (o .: "person_id") <*>
                           (o .: "email")     <*>
                           (o .: "name")      <*>
                           (o .: "unsubscribed_at")

data Person = Person {
    personId          :: String
  , personEmail       :: String
  , personName        :: Maybe String
  , surveyScheduledAt :: Int
-- TODO properties
} deriving ( Eq, Ord, Show )

instance FromJSON Person where
    parseJSON (Object o) = Person <$>
                           (o .: "id") <*>
                           (o .: "email") <*>
                           (o .: "name") <*>
                           (o .: "survey_scheduled_at")

data OK = OK { ok :: Bool } deriving ( Eq, Ord, Show, Generic )

instance FromJSON OK


data SurveyResponse = SurveyResponse {
    responseId     :: String
  , surveyPersonId :: String
  , score          :: Int
  , comment        :: Maybe String
} deriving ( Eq, Show, Ord)

instance FromJSON SurveyResponse where
    parseJSON (Object o) = SurveyResponse <$>
                           (o .: "id") <*>
                           (o .: "person") <*>
                           (o .: "score") <*>
                           (o .: "comment")

--------------------------------------------------------------------------------------

unsubscribe :: String -> String -> IO (Maybe OK)
unsubscribe apiKey email =
    let opts = M.fromList [("person_email", email)] in
    do
      json <- postRequest apiKey "/v1/unsubscribes.json" opts
      pure . decode $ json :: IO (Maybe OK)

getMetrics :: String ->
              M.Map String String ->
              IO (Maybe MetricsResponse)
getMetrics apiKey opts = do
    json <- getRequest apiKey "/v1/metrics.json" opts
    pure . decode $ json :: IO (Maybe MetricsResponse)

-- | Create or update a person and schedule a survey email.
-- You can add properties to a person by passing the properties[key]=value parameter.
-- Sending this data is useful for filtering responses on the dashboard
-- (e.g. you might send “Location”).

-- You can also use properties to integrate the API across different software tools.
-- For example, if you have a unique customer ID that is used across all of your other tools,
-- you could send it as a property to continue that link within Delighted.
-- You can add as many properties as you like for each person.

-- You can create a person without scheduling a survey email by passing the send=false parameter.
-- This is useful if you wish to handle surveying the person yourself and add your own
-- survey response data via our API.
createPerson :: String -> M.Map String String -> IO (Maybe Person)
createPerson apiKey opts = do
    json <- postRequest apiKey "/v1/people.json" opts
    pure . decode $ json :: IO (Maybe Person)

-- | Retrieve all survey responses for your account.
--
getSurveyResponses
  :: String -> M.Map String String -> IO (Maybe [SurveyResponse])
getSurveyResponses apiKey opts = do
    json <- getRequest apiKey "/v1/survey_responses.json" opts
    pure . decode $ json :: IO (Maybe [SurveyResponse])

-- | Retrieve all unsubscribed people for your account.
--
unsubscribes
  :: String ->
     M.Map String String ->
     IO (Maybe [UnsubscribesResponse])
unsubscribes apiKey opts = do
    json <- getRequest apiKey "/v1/unsubscribes.json" opts
    pure . decode $ json :: IO (Maybe [UnsubscribesResponse])

