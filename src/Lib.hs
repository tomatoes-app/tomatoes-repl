{-# LANGUAGE OverloadedStrings #-}

module Lib (
  cli,
  getInitialState
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void, replicateM_)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import System.Console.Haskeline (InputT, Interrupt(Interrupt), runInputT,
  defaultSettings, getInputLine, outputStrLn, outputStr, getPassword,
  withInterrupt, handle)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.Process (createProcess, proc)

import Tomatoes.Client (CreateSessionResponse(CreateSessionResponse),
  createSession, createTomato)
import Tomatoes.Parser (commandParser)
import Tomatoes.Types (Command(Exit, Help, GithubAuth, StartPomodoro))


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
getInitialState = do
  eTomatoesToken <- readConfig
  case eTomatoesToken of
    Left _ -> TomatoesCLIState Nothing <$> newManager defaultManagerSettings
    Right token ->
      TomatoesCLIState (Just . pack $ removeSpaces token)
        <$> newManager defaultManagerSettings
  where
    readConfig :: IO (Either SomeException String)
    readConfig = do
      homePath <- getEnv "HOME"
      try . readFile $ homePath ++ "/.tomatoes"
    removeSpaces [] = []
    removeSpaces (c:cs)
      | c == '\n' || c == ' ' = removeSpaces cs
      | otherwise = c : removeSpaces cs


prompt :: String
prompt = "🍅 % "


execute :: Either String Command -> TomatoesT ()
execute (Left _) = do
  outputStrLn "Command not found"
  execute (Right Help)
execute (Right Exit) = liftIO exitSuccess
execute (Right Help) = outputStrLn "Available commands: help, exit, quit"
execute (Right GithubAuth) = do
  mGithubToken <- getPassword (Just '*') "GitHub token: "
  case mGithubToken of
    Nothing -> execute (Right GithubAuth)
    Just githubToken -> do
      manager <- lift $ gets httpManager
      response <- liftIO $ createSession manager (pack githubToken)
      case response of
        Left err -> outputStrLn $ "Error : " ++ err
        Right (CreateSessionResponse token) -> do
          lift . modify $ \tomatoesState -> tomatoesState {tomatoesToken = Just (pack token)}
          outputStrLn "Success!"
execute (Right StartPomodoro) =
    handle (\Interrupt -> outputStrLn "Cancelled.") $ withInterrupt runPomodoro
  where
    tick = do
      outputStr "#"
      liftIO $ threadDelay 1000000
    tickMinute = do
      replicateM_ 60 tick
      outputStr "\n"
    notify message =
      void . liftIO $ createProcess (proc "terminal-notifier" ["-message", message])
    runPomodoro = do
      replicateM_ 25 tickMinute
      notify "Pomodoro finished"
      mToken <- lift $ gets tomatoesToken
      case mToken of
        Nothing -> return ()
        Just token -> do
          mTags <- getInputLine "Tags: "
          case mTags of
            Nothing -> outputStrLn "no input..."
            Just tags -> do
              manager <- lift $ gets httpManager
              response <- liftIO $ createTomato manager token (pack tags)
              case response of
                Left err -> outputStrLn $ "Error: " ++ err
                Right _ -> outputStrLn "Tomato saved."
