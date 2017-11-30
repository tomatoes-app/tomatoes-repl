{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tomatoes (
  cli,
  getInitialState
) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar,
  modifyTVar)
import Control.Exception (SomeException, try)
import Data.Maybe (isNothing)
import Control.Monad (void, when)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Data.Time (FormatTime, NominalDiffTime, TimeOfDay, midnight, formatTime,
  defaultTimeLocale, timeToTimeOfDay, getCurrentTime, diffUTCTime,
  secondsToDiffTime, UTCTime)
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
    _sTomatoesCount :: Int,
    sTimerState :: TVar TimerState,
    sHttpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState tomatoesToken cols tomatoesCount _ _) =
       "Tomatoes API token: " ++ maybe "N/A" (const "***") tomatoesToken
    ++ ", cols: " ++ show cols
    ++ ", tomatoes: " ++ show tomatoesCount

type TomatoesT = InputT (StateT TomatoesCLIState IO)


data TimerState = TimerState {
    tsPomodoroTimerStart :: Maybe UTCTime,
    _tsPauseTimerStart :: Maybe UTCTime
  }


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
      mToken <- lift $ gets sTomatoesToken
      mInput <- getInputLine $ prompt mToken
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
      <*> pure 0
      <*> newTVarIO (TimerState Nothing Nothing)
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


-- | The default prompt.
prompt :: Maybe a -> String
prompt mToken =
     setSGRCode [SetColor Foreground Vivid Red]
  ++ setSGRCode [SetColor Background Vivid White]
  ++ setSGRCode [SetConsoleIntensity BoldIntensity]
  ++ "🍅 "
  ++ setSGRCode [Reset]
  ++ maybe "(not connected) " (const "") mToken
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
    oneSec = 1000000
    pomodoroSecs :: Num a => a
    pomodoroSecs = 25 * 60
    runTimer timerStart = do
      now <- liftIO getCurrentTime
      let delta = diffUTCTime now timerStart
      when (delta < pomodoroSecs + 1) $ do
        outputStr $ setCursorColumnCode 0
        progressBar delta
        outputStr saveCursorCode
        outputStr $ setCursorColumnCode 0
        outputStr $ pomodoroTime (deltaToTimeOfDay delta)
        outputStr restoreCursorCode
        liftIO $ threadDelay oneSec
        runTimer timerStart
    deltaToTimeOfDay :: NominalDiffTime -> TimeOfDay
    deltaToTimeOfDay = timeToTimeOfDay . secondsToDiffTime . truncate
    percentage :: NominalDiffTime -> Double
    percentage = (/ pomodoroSecs) . realToFrac
    progressBar delta = do
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
          percentage delta * fromIntegral (availableCols cols)
    notify message =
      -- TODO: choose the correct command according to the OS
      -- TODO: handle errors, example: the case when a command to notify the
      -- user is not present
      createProcess (proc "notify-send" ["-t", "10", "Tomatoes", message])
    notifyPomodoroEnd tTimerState = do
      (TimerState mPomodoroTimer _) <- atomically $ readTVar tTimerState
      when (isNothing mPomodoroTimer) $ do
        void $ notify "Pomodoro finished"
        threadDelay $ 20 * oneSec
        notifyPomodoroEnd tTimerState
    runPomodoro = do
      now <- liftIO getCurrentTime
      tTimerState <- lift $ gets sTimerState
      liftIO . atomically . modifyTVar tTimerState
        $ \ts -> ts {tsPomodoroTimerStart = Just now}
      runTimer now
      liftIO . atomically . modifyTVar tTimerState
        $ \ts -> ts {tsPomodoroTimerStart = Nothing}
      void . liftIO . forkIO $ notifyPomodoroEnd tTimerState
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
