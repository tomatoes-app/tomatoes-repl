{-# LANGUAGE OverloadedStrings #-}

module Lib (
  cli
) where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import System.Console.Haskeline (InputT, runInputT, defaultSettings,
  getInputLine, outputStrLn, getPassword)
import System.Exit (exitSuccess)

import Tomatoes.Client (CreateSessionResponse(CreateSessionResponse),
  createSession)
import Tomatoes.Parser (commandParser)
import Tomatoes.Types (Command(Exit, Help, GithubAuth))


data TomatoesCLIState = TomatoesCLIState {
    tomatoesToken :: Maybe ByteString,
    httpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState token _) = show token

type TomatoesT = InputT (StateT TomatoesCLIState IO)


cli :: IO ()
cli = do
    initialState <- getInitialState
    void $ runStateT (runInputT defaultSettings loop) initialState
  where
    loop :: TomatoesT ()
    loop = do
      mInput <- getInputLine prompt
      case mInput of
        Nothing -> return ()
        Just input -> do
          execute $ parseOnly commandParser (pack input)
          loop


getInitialState :: IO TomatoesCLIState
getInitialState = TomatoesCLIState Nothing <$> newManager defaultManagerSettings


prompt :: String
prompt = "🍅 % "


execute :: Either String Command -> TomatoesT ()
execute (Left _) = do
  liftIO $ putStrLn "Command not found"
  execute (Right Help)
execute (Right Exit) = liftIO exitSuccess
execute (Right Help) = liftIO $ putStrLn "Available commands: help, exit, quit"
execute (Right GithubAuth) = do
  mGithubToken <- getPassword (Just '*') "GitHub token: "
  case mGithubToken of
    Nothing -> execute (Right GithubAuth)
    Just githubToken -> do
      manager <- lift $ gets httpManager
      response <- liftIO $ createSession manager (pack githubToken)
      case response of
        Left err -> liftIO . putStrLn $ "Error : " ++ err
        Right (CreateSessionResponse token) -> do
          lift . modify $ \tomatoesState -> tomatoesState {tomatoesToken = Just (pack token)}
          liftIO . putStrLn $ "Success!"
