{-# LANGUAGE OverloadedStrings #-}

module Tomatoes.Client (
  createSession,
  CreateSessionResponse(..),
  createTomato,
  CreateTomatoResponse(..)
) where

import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), eitherDecode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Monoid ((<>))
import Network.HTTP.Client (Manager, Request(method, requestBody),
  RequestBody(RequestBodyBS), Response(responseBody, responseStatus),
  parseRequest, httpLbs)
import Network.HTTP.Types.Status (Status(statusCode))


newtype CreateSessionResponse = CreateSessionResponse String
  deriving (Show)

instance FromJSON CreateSessionResponse where
  parseJSON (Object v) = CreateSessionResponse <$> v .: "token"
  parseJSON s = fail $ "Can't parse " ++ show s


data CreateTomatoResponse = CreateTomatoResponse {
    tomatoId :: String,
    tomatoCreatedAt :: String,
    tomatoUpdatedAt :: String,
    tomatoTags :: [String]
  } deriving (Show)

instance FromJSON CreateTomatoResponse where
  parseJSON (Object v) = CreateTomatoResponse
    <$> v .: "id"
    <*> v .: "created_at"
    <*> v .: "updated_at"
    <*> v .: "tags"
  parseJSON s = fail $ "Can't parse " ++ show s


createSession :: Manager -> ByteString -> IO (Either String CreateSessionResponse)
createSession manager githubToken = do
  initRequest <- parseRequest "http://tomato.es/api/session"
  let request = initRequest {
      method = "POST",
      requestBody = RequestBodyBS $ "provider=github&access_token=" <> githubToken
    }
  eitherDecode . responseBody <$> httpLbs request manager


createTomato :: Manager -> ByteString -> ByteString -> IO (Either String CreateTomatoResponse)
createTomato manager tomatoesToken tags = do
  initRequest <- parseRequest "http://tomato.es/api/tomatoes"
  let request = initRequest {
      method = "POST",
      requestBody = RequestBodyBS $ "token=" <> tomatoesToken
        <> "&tomato[tag_list]=" <> tags
    }
  response <- httpLbs request manager
  case statusCode (responseStatus response) of
     201 -> return $ eitherDecode (responseBody response)
     _ -> return $ Left (unpack (responseBody response))
