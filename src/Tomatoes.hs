{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tomatoes (
  cli,
  getInitialState
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void, replicateM_, forM_)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Data.Time (FormatTime, TimeOfDay, midnight, formatTime,
  defaultTimeLocale, timeToTimeOfDay, timeOfDayToTime)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import qualified Data.ByteString.Char8 as BS8
import System.Console.ANSI (setSGRCode, SGR(SetColor, SetConsoleIntensity,
  Reset), ConsoleLayer(Foreground, Background), ColorIntensity(Vivid),
  Color(Red, Blue, White), ConsoleIntensity(BoldIntensity), saveCursorCode,
  restoreCursorCode, setCursorColumnCode)
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


-- | The CLI state.
data TomatoesCLIState = TomatoesCLIState {
    sTomatoesToken :: Maybe ByteString,
    sCols :: Int,
    sCurrentTimer :: Maybe TimeOfDay,
    _sTomatoesCount :: Int,
    sHttpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState tomatoesToken cols currentTimer tomatoesCount _) =
       "Tomatoes API token: " ++ maybe "N/A" (const "***") tomatoesToken
    ++ ", cols: " ++ show cols
    ++ ", current timer: " ++ maybe "N/A" pomodoroTime currentTimer
    ++ ", tomatoes: " ++ show tomatoesCount

type TomatoesT = InputT (StateT TomatoesCLIState IO)


-- | Returns a formatted pomodoro time.
pomodoroTime :: FormatTime t => t -> String
pomodoroTime = formatTime defaultTimeLocale "%M:%S"


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


-- TODO: notify the user when a local configuration file has been found.
-- | Generates the initial state of the CLI. It tries to read a Tomatoes API
-- token from a `.tomatoes` file in the `$HOME` directory.
getInitialState :: IO TomatoesCLIState
getInitialState =
    TomatoesCLIState
      <$> (either (const Nothing) (Just . BS8.pack . removeSpaces) <$> readConfig)
      -- TODO: read the COLUMNS env and set it as a maximum value
      -- TODO: dinamically change this value by adding a handler for the
      -- SIGWINCH (window change) signal
      <*> pure 80
      <*> pure Nothing
      <*> pure 0
      <*> newManager defaultManagerSettings
  where
    readConfig :: IO (Either SomeException String)
    readConfig = do
      homePath <- getEnv "HOME"
      try . readFile $ homePath ++ "/.tomatoes"
    removeSpaces [] = []
    removeSpaces (c:cs)
      | c == '\n' || c == ' ' = removeSpaces cs
      | otherwise = c : removeSpaces cs


-- TODO: notify user in case the session is not authenticated (it won't be
-- able to store tomatoes)
-- | The default prompt.
prompt :: String
prompt =
     setSGRCode [SetColor Foreground Vivid Red]
  ++ setSGRCode [SetColor Background Vivid White]
  ++ setSGRCode [SetConsoleIntensity BoldIntensity]
  ++ "🍅 "
  ++ setSGRCode [Reset]
  ++ "% "


-- TODO: send notification if after a pause the user doesn't start a new
-- pomodoro
-- TODO: handle pauses
-- TODO: handle empty commands: it shouldn't print anything apart from a new
-- prompt
execute :: Either String Command -> TomatoesT ()
execute (Left _) = do
  outputStrLn "Parsing error"
  outputStrLn $ "Available commands: " ++ availableCommands
execute (Right Exit) = liftIO exitSuccess
execute (Right Help) = do
  outputStr $ setSGRCode [SetColor Foreground Vivid Red]
  outputStr $ setSGRCode [SetColor Background Vivid Blue]
  outputStrLn $ "Available commands: " ++ availableCommands
  outputStrLn "TODO: put a more detailed description for each command..."
  outputStr $ setSGRCode [Reset]
execute (Right GithubAuth) = do
  mGithubToken <- getPassword (Just '*') "GitHub token: "
  case mGithubToken of
    Nothing -> execute (Right GithubAuth)
    Just githubToken -> do
      manager <- lift $ gets sHttpManager
      response <- liftIO $ createSession manager (BS8.pack githubToken)
      case response of
        Left err -> outputStrLn $ "Error : " ++ err
        Right (CreateSessionResponse token) -> do
          lift . modify $ \tomatoesState -> tomatoesState {sTomatoesToken = Just (BS8.pack token)}
          -- TODO: store the Tomatoes API token in a local configuration file
          outputStrLn "Success!"
execute (Right StartPomodoro) =
    handle (\Interrupt -> outputStrLn "Cancelled.") $ withInterrupt runPomodoro
  where
    tick = do
      -- TODO: handle output in a separate thread to avoid increasing the amount
      -- of time spent running a pomodoro
      mCurrentTimer <- lift $ gets sCurrentTimer
      outputStr $ setCursorColumnCode 0
      forM_ mCurrentTimer progressBar
      outputStr saveCursorCode
      outputStr $ setCursorColumnCode 0
      outputStr $ maybe (pomodoroTime midnight) pomodoroTime mCurrentTimer
      outputStr restoreCursorCode
      lift $ modify increaseTimer
      liftIO $ threadDelay 1000000
    increaseTimer tomatoesState@(TomatoesCLIState _ _ (Just time) _ _) =
      tomatoesState {sCurrentTimer = Just (addSeconds 1 time)}
    increaseTimer _ = error "Timer has not been started"
    addSeconds n = timeToTimeOfDay . (+) n . timeOfDayToTime
    percentage :: TimeOfDay -> Double
    percentage = (/ (25 * 60)) . realToFrac . timeOfDayToTime
    progressBar time = do
        cols <- lift $ gets sCols
        outputStr prefix
        outputStr $ replicate (truncate (filled cols)) '='
        outputStr $ replicate (availableCols cols - truncate (filled cols)) ' '
        outputStr suffix
      where
        -- Save space for the time
        prefix = replicate (length (pomodoroTime midnight)) ' ' ++ " |"
        suffix = "|"
        availableCols cols = cols - length prefix - length suffix
        filled cols =
          percentage time * fromIntegral (availableCols cols)
    tickMinute = replicateM_ 60 tick
    notify message =
      -- TODO: choose the correct command according to the OS
      -- TODO: handle errors, example: the case when a command to notify the
      -- user is not present
      void . liftIO $ createProcess (proc "notify-send" ["-t", "10", "Tomatoes", message])
    runPomodoro = do
      lift . modify $ \tomatoesState -> tomatoesState {sCurrentTimer = Just midnight}
      replicateM_ 25 tickMinute
      -- TODO: keep notifying the user every 30 seconds until the user accepts
      -- creates the new tomato
      notify "Pomodoro finished"
      mToken <- lift $ gets sTomatoesToken
      case mToken of
        Nothing -> return ()
        Just token -> do
          mTags <- getInputLine "Tags: "
          case mTags of
            Nothing -> outputStrLn "no input..."
            Just tags -> do
              manager <- lift $ gets sHttpManager
              response <- liftIO $ createTomato manager token (BS8.pack tags)
              case response of
                Left err -> outputStrLn $ "Error: " ++ err
                Right _ -> outputStrLn "Tomato saved."
