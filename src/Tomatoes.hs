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
import Control.Monad (void, when)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString (ByteString)
import Data.Char (isSpace, toLower)
import Data.Time (FormatTime, NominalDiffTime, TimeOfDay, midnight, formatTime,
  defaultTimeLocale, timeToTimeOfDay, getCurrentTime, diffUTCTime,
  secondsToDiffTime)
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
import System.FilePath.Posix ((</>))
import System.IO (Handle)
import System.Process (createProcess, proc, std_err, StdStream(NoStream),
  ProcessHandle)

import Tomatoes.Client (CreateSessionResponse(CreateSessionResponse),
  createSession, createTomato, tuName, getUser, TomatoesUser)
import Tomatoes.Parser (commandParser)
import Tomatoes.Types (Command(Exit, Help, Auth, StartPomodoro, StartPause),
  Provider(Github), availableCommands)


-- | The CLI state.
data TomatoesCLIState = TomatoesCLIState {
    sTomatoesToken :: Maybe ByteString,
    sTomatoesUser :: Maybe TomatoesUser,
    sCols :: Int,
    sTomatoesCount :: Int,
    sTimerState :: TVar TimerState,
    sHttpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState tomatoesToken mUser cols tomatoesCount _ _) =
       "Tomatoes API token: " ++ maybe "N/A" (const "***") tomatoesToken
    ++ ", user: " ++ show mUser
    ++ ", cols: " ++ show cols
    ++ ", tomatoes: " ++ show tomatoesCount

type TomatoesT = InputT (StateT TomatoesCLIState IO)


data TimerState =
    InitialState
  | PomodoroRunning
  | WaitingForTags
  | PauseRunning
    deriving (Show)


data Sound = Ringing | Ding
  deriving (Show)


-- | Returns a sound's filename
soundFileName :: Sound -> FilePath
soundFileName Ringing = "sounds/ringing.mp3"
soundFileName Ding = "sounds/ding.mp3"


-- | Returns a formatted pomodoro time.
pomodoroTime :: FormatTime t => t -> String
pomodoroTime = formatTime defaultTimeLocale "%M:%S"


-- | Reads a configuration file and starts the Tomatoes CLI.
cli :: IO ()
cli = do
    initialState <- getInitialState
    -- TODO: add completion function, see
    -- https://downloads.haskell.org/~ghc/8.2.1/docs/html/libraries/haskeline-0.7.4.0/System-Console-Haskeline-Completion.html
    void $ runStateT (runInputT defaultSettings loop) initialState
  where
    loop :: TomatoesT ()
    loop = do
      mToken <- lift $ gets sTomatoesToken
      mUser <- lift $ gets sTomatoesUser
      -- TODO: use getInputLineWithInitial, see
      -- https://downloads.haskell.org/~ghc/8.2.1/docs/html/libraries/haskeline-0.7.4.0/System-Console-Haskeline.html#v:getInputLineWithInitial
      mInput <- getInputLine $ prompt mToken mUser
      case mInput of
        Nothing -> return ()
        Just input -> do
          case parseOnly commandParser (BS8.pack input) of
            Left _ -> do
              outputStrLn "Parsing error"
              outputStrLn $ "Available commands: " ++ availableCommands
            Right command -> execute command
          loop


-- | Generates the initial state of the CLI. It tries to read a Tomatoes API
-- token from a `.tomatoes` file in the `$HOME` directory.
getInitialState :: IO TomatoesCLIState
getInitialState = do
    mToken <- readConfig
    manager <- newManager defaultManagerSettings
    TomatoesCLIState
      <$> pure mToken
      <*> findUser manager mToken
      -- TODO: read the COLUMNS env and set it as a maximum value
      -- TODO: dinamically change this value by adding a handler for the
      -- SIGWINCH (window change) signal
      <*> pure 80
      <*> pure 0
      <*> newTVarIO InitialState
      <*> pure manager
  where
    findUser _ Nothing = return Nothing
    findUser manager (Just token) = either (const Nothing) Just
      <$> getUser manager token
    readConfig :: IO (Maybe ByteString)
    readConfig = do
      home <- getEnv "HOME"
      eToken <- try . readFile $ home </> configFile
      case eToken :: Either SomeException String of
        Left _ -> return Nothing
        Right token -> do
          putStrLn "Configuration file found..."
          return . Just . BS8.pack . filter (not . isSpace) $ token


-- | The configuration file where the app stores and reads the Tomatoes API
-- access token.
configFile :: FilePath
configFile = ".tomatoes"


-- | The default prompt.
prompt :: Maybe a -> Maybe TomatoesUser -> String
prompt mToken mUserName =
     setSGRCode [SetColor Foreground Vivid Red]
  ++ setSGRCode [SetColor Background Vivid White]
  ++ setSGRCode [SetConsoleIntensity BoldIntensity]
  ++ "🍅 "
  ++ setSGRCode [Reset]
  ++ maybe "(not connected) " (const "") mToken
  ++ maybe "" ((\n -> "(" ++ n ++ ") ") . tuName) mUserName
  ++ "% "


-- TODO: handle long pauses after 4 pomodoros
-- TODO: handle empty commands: it shouldn't print anything apart from a new
-- prompt
execute :: Command -> TomatoesT ()
execute Exit = liftIO exitSuccess
execute Help = do
  outputStr $ setSGRCode [SetColor Foreground Vivid Red]
  outputStr $ setSGRCode [SetColor Background Vivid Blue]
  outputStrLn $ "Available commands: " ++ availableCommands
  outputStrLn "TODO: put a more detailed description for each command..."
  outputStr $ setSGRCode [Reset]
execute (Auth Github) = do
  mGithubToken <- getPassword (Just '*') "GitHub token: "
  case mGithubToken of
    Nothing -> outputStrLn "Error: missing input"
    Just githubToken -> do
      manager <- lift $ gets sHttpManager
      response <- liftIO $ createSession manager (BS8.pack githubToken)
      case response of
        Left err -> outputStrLn $ "Error : " ++ err
        Right (CreateSessionResponse token) -> do
          lift . modify $ \tomatoesState -> tomatoesState {sTomatoesToken = Just (BS8.pack token)}
          outputStrLn "Success!"
          eResult <- liftIO . try $ do
            home <- getEnv "HOME"
            writeFile (home </> configFile) token
          case eResult :: Either SomeException () of
            Left err -> outputStrLn
              $ "Error trying to save Tomatoes API token: " ++ show err
            Right _ -> outputStrLn "Tomatoes API token configuration saved."
          mUser <- liftIO $ either (const Nothing) Just
            <$> getUser manager (BS8.pack token)
          lift . modify $ \tomatoesState -> tomatoesState {sTomatoesUser = mUser}
execute StartPomodoro =
    handle interruptHandler $ withInterrupt runPomodoro
  where
    oneSec = 1000000
    oneMin = oneSec * 60
    pomodoroSecs :: Num a => a
    pomodoroSecs = 25 * 60
    isWaitingForTags WaitingForTags = True
    isWaitingForTags _ = False
    validateTags _ Nothing = outputStrLn "Error: missing tags"
    validateTags token (Just "") = do
      mConfirm <-
        getInputLine "Are you sure you want to save without tags? (y/N) "
      case mConfirm of
        Nothing -> outputStrLn "Error: missing input"
        Just x | map toLower x == "y" || map toLower x == "yes" ->
          saveTomato token ""
        Just _ -> getInputLine "Tags: " >>= validateTags token
    validateTags token (Just tags) = saveTomato token tags
    saveTomato token tags = do
      manager <- lift $ gets sHttpManager
      response <- liftIO $ createTomato manager token (BS8.pack tags)
      case response of
        Left err -> outputStrLn $ "Error: " ++ err
        Right _ -> outputStrLn "Tomato saved."
    finishedPomodoroMessage = do
        count <- lift $ gets sTomatoesCount
        return
          $ "You just finished your " ++ show count ++ ordinal count
          ++ " pomodoro, " ++ suffix count
      where
        suffix count
          | count `mod` 4 == 0 = "you deserve a long break!"
          | otherwise = "it's time for a break."
        ordinal 1 = "st"
        ordinal 2 = "nd"
        ordinal 3 = "rd"
        ordinal n
          | n > 20 && n `rem` 10 == 1 = "st"
          | n > 20 && n `rem` 10 == 2 = "nd"
          | n > 20 && n `rem` 10 == 3 = "rd"
          | otherwise = "th"
    runPomodoro = do
      tTimerState <- lift $ gets sTimerState
      liftIO . atomically . modifyTVar tTimerState $ const PomodoroRunning
      startTimer pomodoroSecs
      void . liftIO $ do
        atomically . modifyTVar tTimerState $ const WaitingForTags
        void $ notify "Pomodoro finished!" Ringing
        void . forkIO $ do
          threadDelay oneMin
          notifyUntil
            tTimerState
            isWaitingForTags
            "Did you forget to save your current tomato?"
      lift . modify $ \tomatoesState -> tomatoesState {sTomatoesCount = sTomatoesCount tomatoesState + 1}
      mToken <- lift $ gets sTomatoesToken
      case mToken of
        Nothing -> return ()
        Just token -> getInputLine "Tags: " >>= validateTags token
      finishedPomodoroMessage >>= outputStrLn
      execute StartPause
execute StartPause =
    handle interruptHandler $ withInterrupt runPomodoro
  where
    oneSec = 1000000
    oneMin = oneSec * 60
    pauseSecs :: Num a => a
    pauseSecs = 5 * 60
    isInitialState InitialState = True
    isInitialState _ = False
    runPomodoro = do
      tTimerState <- lift $ gets sTimerState
      liftIO . atomically . modifyTVar tTimerState $ const PauseRunning
      startTimer pauseSecs
      liftIO $ do
        atomically . modifyTVar tTimerState $ const InitialState
        void $ notify "Break is over. It's time to work." Ringing
        void . forkIO $ do
          threadDelay oneMin
          notifyUntil
            tTimerState
            isInitialState
            "Did you forget to start your next tomato?"


-- | Show a progress bar.
progressBar :: Real a => a -> a -> TomatoesT ()
progressBar timerLength delta = do
    cols <- lift $ gets sCols
    outputStr prefix
    outputStr $ replicate (truncate (filled cols)) '░'
    outputStr $ replicate (availableCols cols - truncate (filled cols)) ' '
    outputStr suffix
  where
    -- Save space for the time
    prefix = replicate (length (pomodoroTime midnight)) ' ' ++ " |"
    suffix = "|"
    availableCols cols = cols - length prefix - length suffix
    filled cols =
      percentage * fromIntegral (availableCols cols)
    percentage :: Double
    percentage = realToFrac delta / realToFrac timerLength


-- | Handle interrupts by resetting timer state to `InitialState`.
interruptHandler :: Interrupt -> TomatoesT ()
interruptHandler Interrupt = do
  tTimerState <- lift $ gets sTimerState
  liftIO . atomically . modifyTVar tTimerState $ const InitialState
  outputStrLn "Cancelled."


-- | Start a new timer that lasts 'timerLength'.
startTimer :: NominalDiffTime -> TomatoesT ()
startTimer timerLength = do
    now <- liftIO getCurrentTime
    runTimer now
  where
    oneSec = 1000000
    runTimer timerStart = do
      now <- liftIO getCurrentTime
      let delta = diffUTCTime now timerStart
      when (delta < timerLength + 1) $ do
        outputStr $ setCursorColumnCode 0
        progressBar timerLength delta
        outputStr saveCursorCode
        outputStr $ setCursorColumnCode 0
        outputStr $ pomodoroTime (deltaToTimeOfDay delta)
        outputStr restoreCursorCode
        liftIO $ threadDelay oneSec
        runTimer timerStart
    deltaToTimeOfDay :: NominalDiffTime -> TimeOfDay
    deltaToTimeOfDay = timeToTimeOfDay . secondsToDiffTime . truncate


-- | Executes a sub-process to notify the user.
notify :: String
  -> Sound
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
notify message sound = do
  -- TODO: choose the correct commands according to the OS capabilities
  -- TODO: handle errors, example: the case when a command to notify the
  -- user is not present
  void . forkIO . void $ createProcess (proc "mpg123" [soundFileName sound]) {
      -- Suppress output to stderr
      std_err = NoStream
    }
  createProcess (proc "notify-send" ["-t", "10", "Tomatoes", message])


-- | Notifies `message` every minute until `condition` is false.
notifyUntil :: TVar t -> (t -> Bool) -> String -> IO ()
notifyUntil tTimerState condition message = do
    timerState <- atomically $ readTVar tTimerState
    when (condition timerState) $ do
      void $ notify message Ding
      threadDelay oneMin
      notifyUntil tTimerState condition message
  where
    oneSec = 1000000
    oneMin = oneSec * 60
