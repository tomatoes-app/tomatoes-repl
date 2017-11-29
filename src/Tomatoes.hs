{-# LANGUAGE OverloadedStrings #-}

module Tomatoes (
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
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.ByteString.Char8 as BS8
import System.Console.Haskeline (InputT, Interrupt(Interrupt), runInputT,
  defaultSettings, getInputLine, outputStrLn, outputStr, getPassword,
  withInterrupt, handle)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.Process (createProcess, proc)

import Tomatoes.Client (CreateSessionResponse(CreateSessionResponse),
  createSession, createTomato)
import Tomatoes.Parser (commandParser)
import Tomatoes.Types (Command(Exit, Help, GithubAuth, StartPomodoro),
  availableCommands)


data TomatoesCLIState = TomatoesCLIState {
    tomatoesToken :: Maybe ByteString,
    httpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState token _) = show token

type TomatoesT = InputT (StateT TomatoesCLIState IO)


-- | Reads a configuration file and starts the Tomatoes CLI.
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
          execute $ parseOnly commandParser (BS8.pack input)
          loop


-- | Generates the initial state of the CLI. It tries to read a Tomatoes API
-- token from a `.tomatoes` file in the `$HOME` directory.
getInitialState :: IO TomatoesCLIState
getInitialState = do
  eTomatoesToken <- readConfig
  case eTomatoesToken of
    Left _ -> TomatoesCLIState Nothing <$> newManager defaultManagerSettings
    Right token ->
      TomatoesCLIState (Just . BS8.pack $ removeSpaces token)
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
  outputStrLn "Parsing error"
  outputStrLn $ "Available commands: " ++ availableCommands
execute (Right Exit) = liftIO exitSuccess
execute (Right Help) = do
  outputStrLn $ "Available commands: " ++ availableCommands
  outputStrLn "TODO: put a more detailed description for each command..."
execute (Right GithubAuth) = do
  mGithubToken <- getPassword (Just '*') "GitHub token: "
  case mGithubToken of
    Nothing -> execute (Right GithubAuth)
    Just githubToken -> do
      manager <- lift $ gets httpManager
      response <- liftIO $ createSession manager (BS8.pack githubToken)
      case response of
        Left err -> outputStrLn $ "Error : " ++ err
        Right (CreateSessionResponse token) -> do
          lift . modify $ \tomatoesState -> tomatoesState {tomatoesToken = Just (BS8.pack token)}
          -- TODO: store the Tomatoes API token in a local configuration file
          outputStrLn "Success!"
execute (Right StartPomodoro) =
    handle (\Interrupt -> outputStrLn "Cancelled.") $ withInterrupt runPomodoro
  where
    tick = do
      -- TODO: handle output in a separate thread to avoid increasing the amount
      -- of time spent running a pomodoro
      outputStr "#"
      liftIO $ threadDelay 1000000
    tickMinute = do
      replicateM_ 60 tick
      outputStr "\n"
    notify message =
      -- TODO: choose the correct command according to the OS
      -- TODO: handle errors, example: the case when a command to notify the
      -- user is not present
      void . liftIO $ createProcess (proc "terminal-notifier" ["-message", message])
    runPomodoro = do
      replicateM_ 25 tickMinute
      -- TODO: keep notifying the user every 30 seconds until the user accepts
      -- creates the new tomato
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
              response <- liftIO $ createTomato manager token (BS8.pack tags)
              case response of
                Left err -> outputStrLn $ "Error: " ++ err
                Right _ -> outputStrLn "Tomato saved."
