{-# LANGUAGE OverloadedStrings #-}

module Tomatoes.Client (
  createSession,
  CreateSessionResponse(..)
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), eitherDecode)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Client (Manager, Request(method, requestBody),
  RequestBody(RequestBodyBS), Response(responseBody), parseRequest, httpLbs)


data CreateSessionResponse = CreateSessionResponse String

instance FromJSON CreateSessionResponse where
  parseJSON (Object v) = CreateSessionResponse <$> v .: "token"
  parseJSON s = fail $ "Can't parse " ++ show s


createSession :: Manager -> ByteString -> IO (Either String CreateSessionResponse)
createSession manager githubToken = do
  initRequest <- parseRequest "http://tomato.es/api/session"
  let request = initRequest {
      method = "POST",
      requestBody = RequestBodyBS $ "provider=github&access_token=" <> githubToken
    }
  eitherDecode . responseBody <$> liftIO (httpLbs request manager)
