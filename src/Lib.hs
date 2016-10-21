{-# LANGUAGE OverloadedStrings #-}

module Lib (
  cli
) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(runStateT), modify, gets)
import Data.Attoparsec.ByteString (takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, string, parseOnly, space,
  isEndOfLine)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import System.Console.Haskeline (InputT, runInputT, defaultSettings,
  getInputLine, outputStrLn)
import System.Exit (exitSuccess)

import Tomatoes.Client (CreateSessionResponse(CreateSessionResponse),
  createSession)


data Command = Exit | Help | GithubAuth ByteString

data TomatoesCLIState = TomatoesCLIState {
    tomatoesToken :: Maybe ByteString,
    httpManager :: Manager
  }

-- define an explicit show instance because manager doesn't implement it
instance Show TomatoesCLIState where
  show (TomatoesCLIState token _) = show token

type TomatoesT = StateT TomatoesCLIState IO


cli :: IO ()
cli = do
    initialState <- getInitialState
    runInputT defaultSettings $ loop initialState
  where
    loop :: TomatoesCLIState -> InputT IO ()
    loop state = do
      mInput <- getInputLine prompt
      case mInput of
        Nothing -> return ()
        Just input -> do
          (_, newState) <- liftIO $ runStateT (execute $ parseOnly commandParser (pack input)) state
          outputStrLn $ "Current state: " ++ show newState
          loop newState


getInitialState :: IO TomatoesCLIState
getInitialState = TomatoesCLIState Nothing <$> newManager defaultManagerSettings


prompt :: String
prompt = "🍅 % "


commandParser :: Parser Command
commandParser = exitParser <|> helpParser <|> authParser


exitParser :: Parser Command
exitParser = (string "exit" <|> string "quit") >> return Exit


helpParser :: Parser Command
helpParser = string "help" >> return Help


authParser :: Parser Command
authParser = string "auth" >> space >> githubAuthParser


githubAuthParser :: Parser Command
githubAuthParser = string "github" >> space >> GithubAuth <$> takeTill isEndOfLine


execute :: Either String Command -> TomatoesT ()
execute (Left _) = do
  liftIO $ putStrLn "Command not found"
  execute (Right Help)
execute (Right Exit) = liftIO exitSuccess
execute (Right Help) = liftIO $ putStrLn "Available commands: help, exit, quit"
execute (Right (GithubAuth githubToken)) = do
  manager <- gets httpManager
  response <- liftIO $ createSession manager githubToken
  case response of
    Left err -> liftIO . putStrLn $ "Error : " ++ err
    Right (CreateSessionResponse token) -> do
      modify $ \tomatoesState -> tomatoesState {tomatoesToken = Just (pack token)}
      liftIO . putStrLn $ "Success!"